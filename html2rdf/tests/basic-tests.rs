mod utils;
use utils::{base, parse_html};

#[test]
fn basic_test() {
    let input =
        r#"<html><head><title>foo</title></head><body vocab="http://schema.org/"></body></html>"#;

    let (output_graph, processor_graph) = parse_html(input);

    insta::assert_snapshot!(utils::serialize_normalized_graph(output_graph, base().as_str()), @r"
    @base <http://example.test/> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix schema: <//schema.org/> .
    <> rdfa:usesVocabulary schema: .
    ");

    insta::assert_snapshot!(utils::serialize_normalized_graph(processor_graph, base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix xsd: <//www.w3.org/2001/XMLSchema#> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    _:c14n0 a rdfa:DocumentError ;
    	dc:date "2020-01-13T12:02:00Z"^^xsd:dateTime ;
    	dc:description "Unexpected token" ;
    	rdfa:context "http://example.test/" .
    "#);
}

#[test]
fn property_test() {
    let input = r#"
        <html>
        <head><title>foo</title></head>
        <body vocab="http://schema.org/">
            <p typeof="Book"><span property="name" content="bar">foo</span></p>
        </body>
        </html>
        "#;

    let (output_graph, processor_graph) = parse_html(input);

    insta::assert_snapshot!(utils::serialize_normalized_graph(processor_graph, base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix xsd: <//www.w3.org/2001/XMLSchema#> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    _:c14n0 a rdfa:DocumentError ;
    	dc:date "2020-01-13T12:02:00Z"^^xsd:dateTime ;
    	dc:description "Unexpected token" ;
    	rdfa:context "http://example.test/" .
    "#);

    insta::assert_snapshot!(utils::serialize_normalized_graph(output_graph, base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix schema: <//schema.org/> .
    <> rdfa:usesVocabulary schema: .
    _:c14n0 a schema:Book ;
    	schema:name "bar" .
    "#);
}
