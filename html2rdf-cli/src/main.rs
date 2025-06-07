use std::{
    collections::HashSet,
    fmt::{Debug, Write},
    process::ExitCode,
    rc::{Rc, Weak},
};

use clap::Parser;
use indexmap::map::RawEntryApiV1;
use itertools::Itertools;

#[derive(Parser)]
#[command(version, about)]
struct Args {
    #[arg(value_name = "URL")]
    target: url::Url,
}

fn main() -> Result<ExitCode, Box<dyn std::error::Error>> {
    let args = Args::parse();
    let client = reqwest::blocking::Client::new();
    let base = args.target.to_string();
    let base_iri = oxiri::Iri::parse(base.clone())?;
    let response = client.get(args.target).send()?.error_for_status()?;
    let content_type = response
        .headers()
        .get(reqwest::header::CONTENT_TYPE)
        .and_then(|v| v.to_str().ok());

    if content_type.is_some_and(|ct| !ct.starts_with("text/html")) {
        eprintln!("Error: content type is not text/html.");
        return Ok(ExitCode::FAILURE);
    }

    let content = response.text()?;
    let mut output_graph = oxrdf::Graph::new();
    let mut processor_graph = oxrdf::Graph::new();
    html2rdf::process(
        &content,
        base_iri.clone(),
        &mut output_graph,
        &mut processor_graph,
    )?;

    {
        // output any warnings/errors
        let serializer = oxttl::TurtleSerializer::new();
        let mut locked_err = std::io::stderr().lock();
        let mut writer = serializer.for_writer(&mut locked_err);
        for triple in processor_graph.iter() {
            writer.serialize_triple(triple)?;
        }

        writer.finish()?;
        drop(processor_graph);
    }

    {
        println!("{}", fancy_ttl(&output_graph));
        return Ok(ExitCode::SUCCESS);
        println!("---");
        // use serializer with all known prefixes
        let serializer = html2rdf::initial_context_prefixes().mappings().try_fold(
            oxttl::TurtleSerializer::new().with_base_iri(base)?,
            |serializer, (prefix, value)| serializer.with_prefix(prefix, value),
        )?;

        let mut locked_out = std::io::stdout().lock();
        let mut writer = serializer.for_writer(&mut locked_out);
        for triple in output_graph.iter() {
            writer.serialize_triple(triple)?;
        }

        writer.finish()?;
        drop(output_graph);
    }

    Ok(ExitCode::SUCCESS)
}

pub fn fancy_ttl(graph: &oxrdf::Graph) -> String {
    let prefixes = html2rdf::initial_context_prefixes()
        .mappings()
        .sorted_by_key(|(_, iri)| iri.len())
        .rev()
        .collect::<Vec<_>>();

    let write_iri = |to: &mut String, iri: &str| {
        for (prefix, value) in &prefixes {
            if let Some(rel) = iri.strip_prefix(&**value) {
                to.push_str(prefix);
                to.push(':');
                to.push_str(rel);
                return;
            }
        }
        to.push('<');
        to.push_str(iri);
        to.push('>');
    };

    {
        // usually an interned string pool would use weak references,
        // but here we want the inverse, when we drop the "interned" version,
        // we will see if we still need the name or not
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
                    oxrdf::SubjectRef::NamedNode(n) => Subj::NamedNode(interned(n.as_str())),
                    oxrdf::SubjectRef::BlankNode(b) => Subj::BlankNode(interned(b.as_str())),
                };

                let p = interned(triple.predicate.as_str());

                let o = match triple.object {
                    oxrdf::TermRef::NamedNode(n) => Obj::NamedNode(interned(n.as_str())),
                    oxrdf::TermRef::BlankNode(b) => {
                        Obj::BlankNode(Rc::downgrade(&interned(b.as_str())))
                    }
                    oxrdf::TermRef::Literal(l) => Obj::Literal(Box::new(l.into())),
                };

                (s, (p, o))
            })
            .into_group_map();

        drop(intern);

        let mut output = String::new();
        for (s, group) in &groups {
            match s {
                Subj::NamedNode(n) => {
                    write_iri(&mut output, n);
                    output.push('\n');
                }
                Subj::BlankNode(b) => {
                    match Rc::weak_count(b) {
                        1 => continue, // someone else has/will render
                        0 => {
                            // no other refs - be anonymous
                            output.push_str("[]\n");
                        }
                        _ => {
                            output.push_str("_:");
                            output.push_str(b);
                            output.push('\n');
                        }
                    }
                }
            }

            for (p, o) in group {
                output.push_str("  ");
                write_iri(&mut output, p);
                output.push(' ');
                match o {
                    Obj::NamedNode(n) => {
                        write_iri(&mut output, n);
                        output.push_str(" ;\n");
                    }
                    Obj::BlankNode(b) => {
                        if let Some(b) = b.upgrade() {
                            if Rc::weak_count(&b) == 1 {
                                // render inline
                                output.push_str("[ ... ] ;\n");
                            } else {
                                output.push_str("_:");
                                output.push_str(&b);
                                output.push_str(" ;\n");
                            }
                        } else {
                            // no other refs - it's empty
                            output.push_str("[] ;\n")
                        }
                    }
                    Obj::Literal(literal) => {
                        output.push('"');
                        output.push_str(literal.value());
                        output.push('"');
                        if !literal.is_plain() {
                            let dt = literal.datatype();
                            output.push_str("^^");
                            write_iri(&mut output, dt.as_str());
                        }
                        if let Some(tag) = literal.language() {
                            output.push('@');
                            output.push_str(tag);
                        }
                        output.push_str(" ;\n");
                    }
                }
            }
            output.push_str(".\n\n");
        }
        output
    }
}

#[derive(Hash, Eq, PartialEq)]
enum Subj {
    NamedNode(Rc<str>),
    BlankNode(Rc<str>),
}

enum Obj {
    NamedNode(Rc<str>),
    BlankNode(Weak<str>),
    Literal(Box<oxrdf::Literal>),
}

mod triplestore {
    use std::{hash::Hash, marker::PhantomData};

    type TripleStore<T> = iddqd::TriHashMap<T>;

    struct Triple<S, P, O> {
        subject: S,
        predicate: P,
        object: O,
    }

    pub struct BlankNodeContext {
        next_id: usize,
    }

    impl BlankNodeContext {
        fn new_anonymous(&mut self) -> BlankNode<'self> {
            let id = self.next_id;
            self.next_id += 1;
        BlankNode {
            id,
            _context: PhantomData,
        }
    }
    }

    struct BlankNode<'a> {
        id: usize,
        _context: PhantomData<fn (&'a ()) -> &'a ()>,
    }

    pub fn can_index_map() {
        let ts: TripleStore<Triple<String, String, String>> = TripleStore::new();
    }
}
