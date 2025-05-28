use std::{
    collections::{HashMap, HashSet},
    rc::{Rc, Weak},
};

use enum_ptr::Compact;
use itertools::Itertools;
use oxrdf::{Graph, Literal};

pub fn serialize_graph(graph: Graph, base: &str) -> String {
    // NB: we use rdf_canon here because the one provided by oxrdf hangs
    let idents = rdf_canon::issue_graph_with::<sha2::Sha256>(&graph, &Default::default()).unwrap();
    let graph = rdf_canon::relabel_graph(&graph, &idents).unwrap();

    let mut output = Vec::new();
    let mut ttl = oxttl::TurtleSerializer::new().with_base_iri(base).unwrap();

    // slow but makes test output nicer
    let mut prefixes_to_use = HashSet::new();
    let mut add_prefix = |full_iri: &str| {
        if let Some((known_prefix, iri)) = html2rdf::initial_context_prefixes()
            .mappings()
            .find(|(prefix, iri)| !prefix.is_empty() && full_iri.starts_with(*iri))
        {
            prefixes_to_use.insert((known_prefix, iri));
        }
    };

    for triple in graph.iter() {
        if let oxrdf::SubjectRef::NamedNode(n) = triple.subject {
            add_prefix(n.as_str());
        }

        add_prefix(triple.predicate.as_str());

        if let oxrdf::TermRef::NamedNode(n) = triple.object {
            add_prefix(n.as_str());
        } else if let oxrdf::TermRef::Literal(l) = triple.object {
            if !l.is_plain() {
                add_prefix(l.datatype().as_str());
            }
        }
    }

    for (prefix, iri) in prefixes_to_use {
        ttl = ttl.with_prefix(prefix, iri).unwrap();
    }

    let mut ttl = ttl.for_writer(&mut output);
    for triple in graph.iter().sorted_by_cached_key(|t| {
        (
            t.subject.to_string(),
            if t.predicate.as_str() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" {
                // make "a" come first
                None
            } else {
                Some(t.predicate.to_string())
            },
            t.object.to_string(),
        )
    }) {
        ttl.serialize_triple(triple).unwrap();
    }

    ttl.finish().unwrap();

    String::from_utf8_lossy(&output).into_owned()
}

#[allow(unused)]
pub fn assert_graph(html: &str, ttl: &str) {
    let mut output_graph = Graph::new();
    let mut processor_graph = Graph::new();
    let base = oxiri::Iri::parse("http://example.org/".into()).unwrap();
    html2rdf::parse(html, base.clone(), &mut output_graph, &mut processor_graph).unwrap();

    let mut ttl_graph = Graph::new();
    {
        let ttl_rdf = oxttl::TurtleParser::new().for_slice(ttl.as_bytes());
        for triple in ttl_rdf {
            ttl_graph.insert(&triple.unwrap());
        }
    }

    let output = serialize_graph(output_graph, base.as_str());
    let ttl_output = serialize_graph(ttl_graph, base.as_str());

    pretty_assertions::assert_eq!(output, ttl_output);
}

#[derive(Hash, Eq, PartialEq)]
enum CompactSubj {
    NamedNode(Rc<str>),
    BlankNode(Rc<str>),
}

enum CompactObj {
    NamedNode(Rc<str>),
    BlankNode(Weak<str>),
    Literal(Box<Literal>),
}

pub fn fancy_ttl(graph: Graph) -> String {
    {
        let mut intern: HashSet<Rc<str>> = HashSet::new();
        let mut interned = |s: &str| {
            if let Some(rc_str) = intern.get(s) {
                rc_str.clone()
            } else {
                let rc_str = Rc::<str>::from(s);
                intern.insert(rc_str.clone());
                rc_str
            }
        };

        let groups = graph
            .iter()
            .map(|triple| {
                let s = match triple.subject {
                    oxrdf::SubjectRef::NamedNode(n) => CompactSubj::NamedNode(interned(n.as_str())),
                    oxrdf::SubjectRef::BlankNode(b) => CompactSubj::BlankNode(interned(b.as_str())),
                };

                let p = interned(triple.predicate.as_str());

                let o = match triple.object {
                    oxrdf::TermRef::NamedNode(n) => CompactObj::NamedNode(interned(n.as_str())),
                    oxrdf::TermRef::BlankNode(b) => {
                        CompactObj::BlankNode(Rc::downgrade(&interned(b.as_str())))
                    }
                    oxrdf::TermRef::Literal(l) => CompactObj::Literal(Box::new(l.into())),
                };

                (s, (p, o))
            })
            .into_group_map();

        drop(intern);

        let mut output = String::new();
        for (s, group) in &groups {
            match s {
                CompactSubj::NamedNode(n) => {
                    output.push('<');
                    output.push_str(n);
                    output.push_str("> ");
                }
                CompactSubj::BlankNode(b) => {
                    if Rc::weak_count(b) == 1 {
                        continue; // someone else has/will render
                    }

                    if Rc::strong_count(b) == 1 {
                        output.push_str("[] ");
                    } else {
                        output.push_str("_:");
                        output.push_str(b);
                        output.push(' ');
                    }
                }
            }

            for (p, o) in group {
                output.push_str(&format!("    <{}> ", p));
                match o {
                    CompactObj::NamedNode(n) => {
                        output.push('<');
                        output.push_str(n);
                        output.push_str("> ;")
                    }
                    CompactObj::BlankNode(b) => {
                        if let Some(b) = b.upgrade() {
                            if Rc::strong_count(&b) <= 2 {
                                // render inline
                                output.push_str("[ ... ] ;\n");
                            } else {
                                output.push('<');
                                output.push_str(&b);
                                output.push_str("> ;\n");
                            }
                        } else {
                            // no other refs - it's empty
                            output.push_str("[] ;")
                        }
                    }
                    CompactObj::Literal(literal) => output.push_str(literal.value()),
                }
            }
        }
        output
    }
}
