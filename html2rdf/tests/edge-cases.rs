// Edge-case tests for RDFa processing.
use html2rdf::{Options, doc_to_graphs, host_language::Html5};
use indoc::indoc;
use oxiri::Iri;
use oxrdf::Graph;

mod utils;

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
