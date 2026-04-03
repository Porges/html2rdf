#![allow(unused)]

use std::{collections::HashSet, str::FromStr, sync::OnceLock};

use html2rdf::{
    Options,
    algorithms::OfflineVocabularyResolver,
    graphs::{self, ProcessorGraph},
    host_language::{Html5, XHtml},
};
use itertools::Itertools;
use oxrdf::{
    Graph,
    graph::{CanonicalizationAlgorithm, CanonicalizationHashAlgorithm},
    vocab::{self, rdf, xsd},
};

pub fn base() -> oxiri::Iri<&'static str> {
    oxiri::Iri::parse_unchecked("http://example.test/")
}

pub fn parse_html(html: &str) -> (Graph, Graph) {
    parse_html_base(html, base(), false, false)
}

pub fn parse_xhtml(html: &str) -> (Graph, Graph) {
    parse_html_base(html, base(), false, true)
}

fn shared_vocab_resolver() -> &'static OfflineVocabularyResolver {
    static RESOLVER: std::sync::OnceLock<OfflineVocabularyResolver> = OnceLock::new();
    RESOLVER.get_or_init(|| {
        let mut resolver = OfflineVocabularyResolver::default();
        for (uri, file) in [
            ("http://rdfa.info/vocabs/rdfa-test#", "rdfa-test-suite.html"),
            ("http://creativecommons.org/ns#", "creative-commons.html"),
        ] {
            let vocab_iri = uri.to_string();
            let html = std::fs::read_to_string(format!(
                "{}/tests/vocabs/{file}",
                env!("CARGO_MANIFEST_DIR")
            ))
            .unwrap();
            resolver.insert_from_html(vocab_iri, &html);
        }
        resolver
    })
}

pub fn parse_html_base(
    html: &str,
    base: oxiri::Iri<&str>,
    expand: bool,
    xhtml: bool,
) -> (Graph, Graph) {
    struct FixedDatePG<'a>(&'a mut oxrdf::Graph, jiff::Timestamp);

    impl<'a> FixedDatePG<'a> {
        pub fn new(other: &'a mut oxrdf::Graph) -> Self {
            let timestamp = jiff::Timestamp::from_str("2020-01-14T00:02:00+12:00").unwrap();
            Self(other, timestamp)
        }
    }

    impl<'a> ProcessorGraph for FixedDatePG<'a> {
        fn emit_message(
            &mut self,
            message_class: html2rdf::graphs::PGClass,
            description: &str,
            context: Option<&str>,
        ) {
            graphs::emit_oxrdf(self.0, message_class, description, context, self.1);
        }
    }

    if !xhtml {
        let mut options = Options::<Html5>::default();
        if expand {
            options = options.enable_vocabulary_expansion(shared_vocab_resolver());
        }

        let mut output = Graph::new();
        let mut processor = Graph::new();
        html2rdf::doc_into_graphs(
            html,
            base,
            options,
            &mut output,
            &mut FixedDatePG::new(&mut processor),
        );
        (output, processor)
    } else {
        let mut options = Options::<XHtml>::default();
        if expand {
            options = options.enable_vocabulary_expansion(shared_vocab_resolver());
        }

        let mut output = Graph::new();
        let mut processor = Graph::new();
        html2rdf::doc_into_graphs(
            html,
            base,
            options,
            &mut output,
            &mut FixedDatePG::new(&mut processor),
        )
        .unwrap();
        (output, processor)
    }
}

pub fn normalize_graph(graph: &mut Graph) {
    graph.canonicalize(CanonicalizationAlgorithm::Rdfc10 {
        hash_algorithm: CanonicalizationHashAlgorithm::Sha256,
    });
}

pub fn serialize_normalized_graph(mut graph: Graph, base: &str) -> String {
    normalize_graph(&mut graph);
    serialize_graph(&graph, base)
}

pub fn serialize_graph(graph: &Graph, base: &str) -> String {
    let mut output = Vec::new();
    let mut ttl = oxttl::TurtleSerializer::new().with_base_iri(base).unwrap();

    // slow but makes test output nicer
    let mut prefixes_to_use = HashSet::new();
    let mut add_prefix = |full_iri: &str| {
        if let Some((known_prefix, iri)) = html2rdf::initial_context::prefixes()
            .mappings()
            .find(|(prefix, iri)| !prefix.is_empty() && full_iri.starts_with(*iri))
        {
            prefixes_to_use.insert((known_prefix, iri));
        }
    };

    for triple in graph.iter() {
        if let oxrdf::NamedOrBlankNodeRef::NamedNode(n) = triple.subject {
            add_prefix(n.as_str());
        }

        add_prefix(triple.predicate.as_str());

        if let oxrdf::TermRef::NamedNode(n) = triple.object {
            add_prefix(n.as_str());
        } else if let oxrdf::TermRef::Literal(l) = triple.object
            && !matches!(l.datatype(), xsd::STRING | rdf::LANG_STRING)
        {
            add_prefix(l.datatype().as_str());
        }
    }

    for (prefix, iri) in prefixes_to_use {
        ttl = ttl.with_prefix(prefix, iri).unwrap();
    }

    let mut ttl = ttl.for_writer(&mut output);
    for triple in graph.iter().sorted_by_cached_key(|t| {
        (
            t.subject.to_string(),
            if t.predicate == vocab::rdf::TYPE {
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

/// Tests if two graphs are the same.
/// - First checks if they are identical.
/// - Next checks if they are isomorphic by normalizing them and comparing again.
pub fn eval_graphs<T>(
    my_graph: &Graph,
    expected_graph: &Graph,
    fail_eval: impl Fn(oxrdf::Graph, oxrdf::Graph) -> T,
    okay_result: T,
) -> T {
    if my_graph == expected_graph {
        return okay_result;
    }

    // try normalizing
    let mut my_graph = my_graph.clone();
    normalize_graph(&mut my_graph);

    let mut expected_graph = expected_graph.clone();
    normalize_graph(&mut expected_graph);

    if my_graph == expected_graph {
        return okay_result;
    }

    fail_eval(my_graph, expected_graph)
}

pub fn check_graphs_eq(my_graph: &Graph, expected_graph: &Graph) -> bool {
    eval_graphs(my_graph, expected_graph, |_, _| false, true)
}

pub fn assert_graphs_eq(
    base: oxiri::Iri<&str>,
    my_graph: &Graph,
    expected_graph: &Graph,
    context: &str,
) {
    eval_graphs(
        my_graph,
        expected_graph,
        |my_graph, expected_graph| {
            pretty_assertions::assert_eq!(
                serialize_normalized_graph(my_graph, base.as_str()),
                serialize_normalized_graph(expected_graph, base.as_str()),
                "{}",
                context,
            );
        },
        (),
    );
}

pub fn assert_ttl(base: oxiri::Iri<&str>, html: &str, expected_ttl: &str) {
    let (mut output_graph, ()) = html2rdf::doc_to_graphs(html, base, Options::<Html5>::default())
        .unwrap_or_else(|inf| match inf {});

    let mut ttl_graph = ttl_to_graph(expected_ttl);
    if output_graph == ttl_graph {
        return; // fast path
    }

    normalize_graph(&mut output_graph);
    normalize_graph(&mut ttl_graph);
    if output_graph == ttl_graph {
        return; // normalized graphs are equal
    }

    let output = serialize_graph(&output_graph, base.as_str());
    let ttl_output = serialize_graph(&ttl_graph, base.as_str());

    pretty_assertions::assert_eq!(
        output,
        ttl_output,
        "comparing based upon HTML:\n{}\n\n(given) TTL:\n{}",
        html,
        expected_ttl
    );
}

pub fn assert_graph_eq_ttl(base: oxiri::Iri<&str>, graph: Graph, expected_ttl: &str) {
    let output = serialize_normalized_graph(graph, base.as_str());
    let ttl_output = serialize_normalized_graph(ttl_to_graph(expected_ttl), base.as_str());
    pretty_assertions::assert_eq!(output, ttl_output);
}

pub fn ttl_to_graph(ttl: &str) -> Graph {
    let mut ttl_graph = Graph::new();
    {
        let ttl_rdf = oxttl::TurtleParser::new().for_slice(ttl.as_bytes());
        for triple in ttl_rdf {
            ttl_graph.insert(&triple.unwrap());
        }
    }

    ttl_graph
}

fn graph_to_dataset(graph: &Graph) -> oxrdf::Dataset {
    oxrdf::Dataset::from_iter(
        graph
            .iter()
            .map(|t| t.in_graph(oxrdf::GraphNameRef::DefaultGraph)),
    )
}

/// Evaluates a SPARQL ASK query against a graph and returns the boolean result.
pub fn ask(graph: &Graph, sparql: &str) -> bool {
    let dataset = graph_to_dataset(graph);
    let query = spargebra::SparqlParser::new()
        .parse_query(sparql)
        .unwrap_or_else(|e| panic!("invalid SPARQL query: {e}\n\n{sparql}"));
    let results = spareval::QueryEvaluator::new()
        .prepare(&query)
        .execute(&dataset);
    match results.unwrap() {
        spareval::QueryResults::Boolean(b) => b,
        _ => panic!("expected ASK (boolean) result"),
    }
}

/// Asserts that a SPARQL ASK query returns true against the given graph.
pub fn assert_ask(graph: &Graph, sparql: &str) {
    assert!(
        ask(graph, sparql),
        "ASK query returned false:\n{sparql}\n\nActual graph:\n{}",
        serialize_normalized_graph(graph.clone(), base().as_str()),
    );
}

/// Asserts that a SPARQL ASK query returns false against the given graph.
pub fn assert_not_ask(graph: &Graph, sparql: &str) {
    assert!(
        !ask(graph, sparql),
        "ASK query returned true:\n{sparql}\n\nActual graph:\n{}",
        serialize_normalized_graph(graph.clone(), base().as_str()),
    );
}
