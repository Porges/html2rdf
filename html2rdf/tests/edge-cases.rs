// Edge-case tests for RDFa processing.
use html2rdf::parse;
use oxiri::Iri;
use oxrdf::Graph;

mod utils;

fn base() -> Iri<String> {
    Iri::parse("http://example.test/".to_string()).unwrap()
}

fn parse_html(html: &str) -> (Graph, Graph) {
    let mut output_graph = Graph::new();
    let mut processor_graph = Graph::new();
    parse(html, base(), &mut output_graph, &mut processor_graph).unwrap();
    (output_graph, processor_graph)
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
    insta::assert_snapshot!(utils::serialize_graph(output, base().as_str()), @r#"
    @base <http://example.test/> .
    <> <val> "42" .
    "#);
}
