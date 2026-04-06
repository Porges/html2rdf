//! Tests for the property copying algorithm.
//!
//! [html-rdfa] §3.5: Property Copying
//! [html-rdfa] §3.5.1: Implementing Property Copying

use html2rdf::rdfa::algorithms::property_copying;

mod utils;
use utils::ttl_to_graph;

const TTL_PREFIXES: &str = r#"
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix rdfa: <http://www.w3.org/ns/rdfa#> .
    @prefix ex: <http://example.test/> .
"#;

const SPARQL_PREFIXES: &str = r#"
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfa: <http://www.w3.org/ns/rdfa#>
    PREFIX ex: <http://example.test/>
"#;

fn graph(ttl: &str) -> oxrdf::Graph {
    ttl_to_graph(&format!("{TTL_PREFIXES}{ttl}"))
}

fn assert_ask(graph: &oxrdf::Graph, sparql: &str) {
    utils::assert_ask(graph, &format!("{SPARQL_PREFIXES}{sparql}"));
}

fn assert_not_ask(graph: &oxrdf::Graph, sparql: &str) {
    utils::assert_not_ask(graph, &format!("{SPARQL_PREFIXES}{sparql}"));
}

#[test]
fn basic_copy_from_pattern() {
    let mut graph = graph(
        r#"
        ex:post1 rdfa:copy ex:pattern1 .
        ex:pattern1 a rdfa:Pattern ;
            ex:license <http://creativecommons.org/licenses/by/3.0/> .
    "#,
    );

    property_copying(&mut graph);

    assert_ask(
        &graph,
        r#"
        ASK { ex:post1 ex:license <http://creativecommons.org/licenses/by/3.0/> }
    "#,
    );
    assert_not_ask(&graph, "ASK { ?s rdfa:copy ?o }");
    assert_not_ask(&graph, "ASK { ex:pattern1 ?p ?o }");
}

#[test]
fn multiple_subjects_copy_same_pattern() {
    let mut graph = graph(
        r#"
        ex:post1 rdfa:copy ex:pattern1 .
        ex:post2 rdfa:copy ex:pattern1 .
        ex:pattern1 a rdfa:Pattern ;
            ex:license <http://creativecommons.org/licenses/by/3.0/> .
    "#,
    );

    property_copying(&mut graph);

    assert_ask(
        &graph,
        r#"
        ASK {
            ex:post1 ex:license <http://creativecommons.org/licenses/by/3.0/> .
            ex:post2 ex:license <http://creativecommons.org/licenses/by/3.0/> .
        }
    "#,
    );
    assert_not_ask(&graph, "ASK { ?s rdfa:copy ?o }");
    assert_not_ask(&graph, "ASK { ex:pattern1 a rdfa:Pattern }");
}

#[test]
fn copy_target_not_a_pattern_is_ignored() {
    let mut graph = graph(
        r#"
        ex:post1 rdfa:copy ex:not-a-pattern .
        ex:not-a-pattern ex:prop "hello" .
    "#,
    );

    property_copying(&mut graph);

    // The property should NOT be copied (target is not an rdfa:Pattern)
    assert_not_ask(&graph, r#"ASK { ex:post1 ex:prop "hello" }"#);

    // The rdfa:copy triple remains — pattern-clean conditions are not met
    // (ex:not-a-pattern is not a rdfa:Pattern)
    assert_ask(&graph, "ASK { ex:post1 rdfa:copy ex:not-a-pattern }");

    // The non-pattern's own triples should remain
    assert_ask(&graph, r#"ASK { ex:not-a-pattern ex:prop "hello" }"#);
}

#[test]
fn copy_target_literal_is_ignored() {
    let mut graph = graph(
        r#"
        ex:post1 rdfa:copy "not-a-node" .
    "#,
    );

    property_copying(&mut graph);

    // The rdfa:copy triple remains — target is a literal, not a Pattern
    assert_ask(&graph, r#"ASK { ex:post1 rdfa:copy "not-a-node" }"#);
}

#[test]
fn chained_copy() {
    let mut graph = graph(
        r#"
        ex:post1 rdfa:copy ex:patternA .
        ex:patternA a rdfa:Pattern ;
            rdfa:copy ex:patternB .
        ex:patternB a rdfa:Pattern ;
            ex:prop "deep-value" .
    "#,
    );

    property_copying(&mut graph);

    assert_ask(&graph, r#"ASK { ex:post1 ex:prop "deep-value" }"#);
    assert_not_ask(&graph, "ASK { ?s rdfa:copy ?o }");
    assert_not_ask(&graph, "ASK { ex:patternA a rdfa:Pattern }");
    assert_not_ask(&graph, "ASK { ex:patternB a rdfa:Pattern }");
}

#[test]
fn pattern_literal_properties_are_copied() {
    let mut graph = graph(
        r#"
        ex:post1 rdfa:copy ex:pattern1 .
        ex:pattern1 a rdfa:Pattern ;
            ex:author "Alice" .
    "#,
    );

    property_copying(&mut graph);

    assert_ask(&graph, r#"ASK { ex:post1 ex:author "Alice" }"#);
}

/// A Pattern that copies from another Pattern retains its own type
/// (since it's not consumed as a target by anyone else). Its copied
/// properties are added, and the target's triples are cleaned up.
///
/// NOTE: this is a spec deviation. The spec's pattern-clean rule would
/// remove `?subject rdf:type rdfa:Pattern` unconditionally, but our
/// implementation skips copying that type in step 1 and only removes
/// consumed targets in step 2, so originally-typed Patterns that are
/// not themselves consumed retain their type.
#[test]
fn pattern_copying_from_pattern_retains_own_type() {
    let mut graph = graph(
        r#"
        ex:reusable a rdfa:Pattern ;
            rdfa:copy ex:base-pattern ;
            ex:extra "bonus" .
        ex:base-pattern a rdfa:Pattern ;
            ex:license <http://creativecommons.org/licenses/by/4.0/> .
    "#,
    );

    property_copying(&mut graph);

    // ex:reusable should have copied ex:license from ex:base-pattern
    assert_ask(
        &graph,
        r#"
        ASK { ex:reusable ex:license <http://creativecommons.org/licenses/by/4.0/> }
    "#,
    );

    // ex:reusable is not consumed as a target, so it keeps its type
    assert_ask(&graph, "ASK { ex:reusable a rdfa:Pattern }");

    // ex:reusable's own properties remain
    assert_ask(&graph, r#"ASK { ex:reusable ex:extra "bonus" }"#);

    // ex:base-pattern was consumed — its triples are removed
    assert_not_ask(&graph, "ASK { ex:base-pattern ?p ?o }");
}

/// A non-Pattern resource that rdfa:copy's from a Pattern should not become
/// a valid Pattern target itself. The "for each new rdfa:copy statement added"
/// wording in the spec means only newly-added rdfa:copy triples should be
/// re-evaluated, not that previously-skipped ones should be retried after
/// the graph changes.
///
/// Without this, the rdfa:Pattern type "infects" the subject, and any other
/// resource copying from it would have its original triples destroyed in cleanup.
#[test]
fn non_pattern_subject_not_infected_by_copy() {
    let mut graph = graph(
        r#"
        ex:alice a ex:Person ;
            rdfa:copy ex:bob .
        ex:bob a ex:Person ;
            rdfa:copy ex:name-pattern .
        ex:name-pattern a rdfa:Pattern ;
            ex:name "Foo" .
    "#,
    );

    property_copying(&mut graph);

    // ex:bob copied from ex:name-pattern → gets ex:name "Foo"
    assert_ask(&graph, r#"ASK { ex:bob ex:name "Foo" }"#);

    // ex:bob was NOT originally a Pattern, so ex:alice's copy should not have fired.
    // ex:alice should NOT get ex:name "Foo"
    assert_not_ask(&graph, r#"ASK { ex:alice ex:name "Foo" }"#);

    // ex:bob must retain its original triples (it's a real resource, not a pattern)
    assert_ask(&graph, "ASK { ex:bob a ex:Person }");

    // ex:alice must also retain its original triples
    assert_ask(&graph, "ASK { ex:alice a ex:Person }");
}

/// When a pattern contains rdfa:copy pointing at a non-Pattern, the copy
/// chain is followed (and skipped since the target isn't a Pattern), but
/// the rdfa:copy triple should NOT be propagated to the subject.
///
/// NOTE: this is a spec deviation. The spec would copy the rdfa:copy triple
/// to the subject, leaving a dangling `subject rdfa:copy non-pattern` in
/// the output. Our implementation follows the chain via the worklist without
/// adding the rdfa:copy triple to the graph.
#[test]
fn no_dangling_copy_propagated_to_subject() {
    let mut graph = graph(
        r#"
        ex:post rdfa:copy ex:template .
        ex:template a rdfa:Pattern ;
            rdfa:copy ex:not-a-pattern ;
            ex:prop "from-template" .
        ex:not-a-pattern ex:other "thing" .
    "#,
    );

    property_copying(&mut graph);

    // The template's property should be copied to ex:post
    assert_ask(&graph, r#"ASK { ex:post ex:prop "from-template" }"#);

    // ex:post should NOT have a dangling rdfa:copy to ex:not-a-pattern
    assert_not_ask(&graph, "ASK { ex:post rdfa:copy ?o }");

    // ex:not-a-pattern's own triples should be untouched
    assert_ask(&graph, r#"ASK { ex:not-a-pattern ex:other "thing" }"#);
}
