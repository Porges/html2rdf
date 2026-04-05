// Edge-case tests for RDFa processing.
use html2rdf::{Options, doc_to_graphs, host_language::Html5};
use indoc::indoc;
use oxiri::Iri;
use oxrdf::Graph;

mod utils;
use utils::{assert_ask, assert_not_ask};

fn base() -> Iri<&'static str> {
    Iri::parse("http://example.test/").unwrap()
}

fn parse_html(html: &str) -> (Graph, Graph) {
    doc_to_graphs(html, base(), Options::<Html5>::default()).unwrap_or_else(|inf| match inf {})
}

// A blank node CURIE in @datatype is not a valid IRI, so per the
// TERMorCURIEorAbsIRI rules (rdfa-core 7.4) the value is ignored and
// @datatype is treated as if it were not present.
#[test]
fn blank_node_datatype_treated_as_missing() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <span property="http://example.test/val" datatype="_:bnode">42</span>
    </body>
    </html>"#;

    let (output, _processor) = parse_html(html);
    insta::assert_snapshot!(utils::serialize_normalized_graph(output, base().as_str()), @r#"
    @base <http://example.test/> .
    <> <val> "42" .
    "#);
}

#[test]
fn typeof_on_root_empty_about() {
    let html = r#"
    <html about="" typeof="foaf:Person" rel="foaf:knows">
    <head><title>test</title></head>
    <body>
        <span property="http://example.test/val">42</span>
    </body>
    </html>"#;

    let (output, _processor) = parse_html(html);
    insta::assert_snapshot!(utils::serialize_normalized_graph(output, base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix foaf: <//xmlns.com/foaf/0.1/> .
    <> a foaf:Person ;
    	foaf:knows _:c14n0 .
    _:c14n0 <val> "42" .
    "#);
}

#[test]
fn typeof_on_root_no_about() {
    let html = r#"
    <html typeof="foaf:Person" rel="foaf:knows">
    <body>
        <span property="http://example.test/val">42</span>
    </body>
    </html>"#;

    let (output, _processor) = parse_html(html);
    insta::assert_snapshot!(utils::serialize_normalized_graph(output, base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix foaf: <//xmlns.com/foaf/0.1/> .
    <> a foaf:Person ;
    	foaf:knows _:c14n0 .
    _:c14n0 <val> "42" .
    "#);
}

#[test]
fn both_rel_and_property() {
    let html = indoc! {r##"
        <html>
          <body>
            <div about="#alice" rel="foaf:knows" property="dc:description" resource="#bob">
            Alice's friend Bob
            </div>
          </body>
        </html>
    "##};

    let (output, _processor) = parse_html(html);
    insta::assert_snapshot!(utils::serialize_normalized_graph(output, base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix foaf: <//xmlns.com/foaf/0.1/> .
    @prefix dc: <//purl.org/dc/terms/> .
    <#alice> dc:description "\n    Alice's friend Bob\n    " ;
    	foaf:knows <#bob> .
    "#);
}

// ============================================================================
// @datetime and <time> interaction with step 5 routing
//
// [html-rdfa] extensions #9/#10 say @datetime and <time> text are used at
// step 11 only — they don't mention modifying step 5's routing condition.
//
// However, we treat @datetime and <time> the same as @content for step 5
// routing: their presence routes to step 5.2 (simple property) instead of
// step 5.1 (complex property / chaining).
//
// In practice this makes no observable difference to the output graph,
// because @datetime provides a literal value at step 11 that supersedes
// the typed-resource fallback either way. The tests below document this.
// ============================================================================

/// Baseline: @content routes to step 5.2.
/// @property emits the @content literal, not the typed resource.
#[test]
fn content_blocks_chaining_with_typeof() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div property="http://example.test/prop"
             typeof="http://example.test/Type"
             content="hello">
            <span property="http://example.test/child">nested</span>
        </div>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);

    // @property emits the @content literal (step 5.2 / step 11)
    assert_ask(&output, r#"
        ASK { ?s <http://example.test/prop> "hello" }
    "#);

    // The property value is a literal, NOT the typed bnode
    assert_not_ask(&output, r#"
        ASK { ?s <http://example.test/prop> ?bnode .
              ?bnode a <http://example.test/Type> .
              FILTER(isBlank(?bnode)) }
    "#);
}

/// @datetime is treated like @content for step 5 routing (step 5.2).
/// @typeof creates a bnode as new_subject; @property emits the datetime literal.
/// Children see the bnode as their parent subject.
///
/// Note: the output graph is identical regardless of whether @datetime
/// routes to step 5.1 or 5.2, because @datetime provides a literal value
/// at step 11 that supersedes the typed-resource fallback in either path.
#[test]
fn datetime_with_typeof_emits_literal() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div property="http://example.test/prop"
             typeof="http://example.test/Type"
             datetime="2024-01-15">
            <span property="http://example.test/child">nested</span>
        </div>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);

    // @property value is a datetime literal
    assert_ask(&output, r#"
        ASK { ?s <http://example.test/prop> "2024-01-15"^^<http://www.w3.org/2001/XMLSchema#date> }
    "#);

    // A bnode is created and typed
    assert_ask(&output, r#"
        ASK { ?bnode a <http://example.test/Type> . FILTER(isBlank(?bnode)) }
    "#);

    // Child is about the typed bnode
    assert_ask(&output, r#"
        ASK { ?bnode a <http://example.test/Type> ;
                     <http://example.test/child> "nested" .
              FILTER(isBlank(?bnode)) }
    "#);
}

/// Verify that @datetime + @typeof produces the same output as
/// @content + @datatype + @typeof.
#[test]
fn datetime_treated_as_content_for_step5_routing() {
    // Verify that @datetime + @typeof produces the same output as
    // @content + @typeof (i.e., step 5.2 behavior)
    let html_datetime = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div property="http://example.test/prop"
             typeof="http://example.test/Type"
             datetime="2024-01-15">
            <span property="http://example.test/child">nested</span>
        </div>
    </body>
    </html>"#;

    let html_content = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div property="http://example.test/prop"
             typeof="http://example.test/Type"
             content="2024-01-15"
             datatype="http://www.w3.org/2001/XMLSchema#date">
            <span property="http://example.test/child">nested</span>
        </div>
    </body>
    </html>"#;

    let (output_datetime, _) = parse_html(html_datetime);
    let (output_content, _) = parse_html(html_content);

    assert!(
        utils::check_graphs_eq(&output_datetime, &output_content),
        "@datetime and @content+@datatype should produce identical graphs"
    );
}

/// Same as above but for <time> elements: the implicit datetime
/// also routes to step 5.2.
#[test]
fn time_element_with_typeof_emits_literal() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <time property="http://example.test/prop"
              typeof="http://example.test/Type">2024-01-15</time>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);

    // @property value is the datetime literal (from <time> text → implicit @datetime)
    assert_ask(&output, r#"
        ASK { ?s <http://example.test/prop> "2024-01-15"^^<http://www.w3.org/2001/XMLSchema#date> }
    "#);

    // A bnode IS created and typed (step 5.1 chaining)
    assert_ask(&output, r#"
        ASK { ?bnode a <http://example.test/Type> . FILTER(isBlank(?bnode)) }
    "#);
}
