mod utils;

use utils::{base, parse_html};

#[test]
fn invalid_blank_node_suffix_emits_warning() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div about="[_:invalid blank]" property="http://example.test/p">value</div>
    </body>
    </html>"#;

    let (_output, processor) = parse_html(html);
    insta::assert_snapshot!(utils::serialize_normalized_graph(processor, base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix xsd: <//www.w3.org/2001/XMLSchema#> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    _:c14n0 a rdfa:UnresolvedCURIE ;
    	dc:date "2020-01-13T12:02:00Z"^^xsd:dateTime ;
    	dc:description "Invalid CURIE: [_:invalid blank] (invalid blank node suffix `invalid blank`)" .
    "#);
}

#[test]
fn undefined_prefix_in_safe_curie_emits_warning() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div about="[undefined:thing]" property="http://example.test/p">value</div>
    </body>
    </html>"#;

    let (_output, processor) = parse_html(html);
    insta::assert_snapshot!(utils::serialize_normalized_graph(processor, base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix xsd: <//www.w3.org/2001/XMLSchema#> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    _:c14n0 a rdfa:UnresolvedCURIE ;
    	dc:date "2020-01-13T12:02:00Z"^^xsd:dateTime ;
    	dc:description "Invalid CURIE: [undefined:thing] (no such prefix defined)" .
    "#);
}

#[test]
fn invalid_iri_from_curie_expansion_emits_warning() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body prefix="bad: http://example.test/invalid{iri}/">
        <div about="[bad:thing]" property="http://example.test/p">value</div>
    </body>
    </html>"#;

    let (_output, processor) = parse_html(html);
    insta::assert_snapshot!(utils::serialize_normalized_graph(processor, base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix xsd: <//www.w3.org/2001/XMLSchema#> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    _:c14n0 a rdfa:UnresolvedCURIE ;
    	dc:date "2020-01-13T12:02:00Z"^^xsd:dateTime ;
    	dc:description "Expanding CURIE [bad:thing] resulted in an invalid IRI (Invalid IRI: <http://example.test/invalid{iri}/thing>; Invalid IRI code point '{')" .
    "#);
}

#[test]
fn empty_safe_curie_silently_ignored() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div about="[]" property="http://example.test/p">value</div>
    </body>
    </html>"#;

    let (_output, processor) = parse_html(html);
    insta::assert_snapshot!(utils::serialize_normalized_graph(processor, base().as_str()), @"");
}

#[test]
fn missing_default_prefix_in_safe_curie_silently_ignored() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div about="[:thing]" property="http://example.test/p">value</div>
    </body>
    </html>"#;

    let (_output, processor) = parse_html(html);
    insta::assert_snapshot!(utils::serialize_normalized_graph(processor, base().as_str()), @"");
}
