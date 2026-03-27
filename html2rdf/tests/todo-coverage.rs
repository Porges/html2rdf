// Tests that exercise known TODO/unimplemented code paths in the library.
use html2rdf::{Options, doc_to_graphs, graphs::ProcessorGraph, host_language::Html5};
use oxiri::Iri;
use oxrdf::Graph;

mod utils;

fn base() -> Iri<&'static str> {
    Iri::parse("http://example.org/").unwrap()
}

fn parse_html<PG: ProcessorGraph + Default>(html: &str) -> (Graph, PG) {
    doc_to_graphs(html, base(), Options::<Html5>::default()).unwrap_or_else(|inf| match inf {})
}

// lib.rs:813 + host_language/html.rs:129 — HTML5 XMLLiteral datatype uses
// scraper's inner_html() without XML canonicalization (c14n).
// Note: the XHTML path (xhtml.rs) now uses bergshamra_c14n for proper c14n.
#[test]
fn xml_literal_not_canonicalized() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div property="http://example.org/content"
             datatype="http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral">
            <span class="inner">hello</span>
        </div>
    </body>
    </html>"#;

    let (output, ()) = parse_html(html);

    // Currently the literal value is just inner_html() without c14n.
    // This test documents the current (incorrect) behavior.
    let literal = output
        .iter()
        .find(|t| t.predicate.as_str() == "http://example.org/content")
        .expect("should have a triple with the property");

    let lit = match literal.object {
        oxrdf::TermRef::Literal(l) => l,
        _ => panic!("expected a literal object"),
    };

    assert_eq!(
        lit.datatype().as_str(),
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"
    );
    // The value is currently just inner_html() — not c14n'd
    assert!(
        lit.value().contains("hello"),
        "literal should contain the text content"
    );
}

// lib.rs:817 + host_language/html.rs:129 — rdf:HTML datatype uses
// scraper's inner_html() without canonicalization in the HTML5 path.
// Note: the XHTML path (xhtml.rs) now uses bergshamra_c14n.
#[test]
fn rdf_html_literal_not_canonicalized() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div property="http://example.org/content"
             datatype="http://www.w3.org/1999/02/22-rdf-syntax-ns#HTML">
            <em>hello</em>
        </div>
    </body>
    </html>"#;

    let (output, ()) = parse_html(html);

    let literal = output
        .iter()
        .find(|t| t.predicate.as_str() == "http://example.org/content")
        .expect("should have a triple with the property");

    let lit = match literal.object {
        oxrdf::TermRef::Literal(l) => l,
        _ => panic!("expected a literal object"),
    };

    assert_eq!(
        lit.datatype().as_str(),
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#HTML"
    );
    assert!(
        lit.value().contains("hello"),
        "literal should contain the text content"
    );
}

// resolver.rs:235,248 — add_prefix() now properly handles errors via
// if let Err(InvalidPrefixError::ReservedPrefix), emitting a processor graph
// warning instead of panicking. The old .expect("TODO") has been fixed.
// This test documents that invalid-looking prefixes (e.g. starting with digits)
// are still accepted by the curie crate.
#[test]
fn invalid_looking_prefix_declaration_succeeds() {
    // A prefix starting with digits is technically invalid per XML NCName rules,
    // but the curie crate accepts it. add_prefix() no longer panics (the old
    // .expect("TODO") was replaced with proper error handling in resolver.rs).
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body prefix="123bad: http://example.org/">
        <span property="123bad:thing">value</span>
    </body>
    </html>"#;

    let (output, ()) = parse_html(html);
    let serialized = utils::serialize_normalized_graph(output, base().as_str());

    // The prefix was accepted and the CURIE resolved.
    // Since base is http://example.org/, <thing> is the relative form of http://example.org/thing.
    assert!(
        serialized.contains("<thing>"),
        "prefix should have been accepted and CURIE expanded, got:\n{serialized}"
    );
}

// lib.rs:813 + host_language/html.rs:129 — HTMLElement::inner_html() does not
// inject active namespace declarations into XMLLiteral top-level elements.
// Note: the XHTML path (xhtml.rs:138-210) now handles namespace preservation
// properly via cloning and inserting xmlns declarations before c14n.
#[test]
fn xml_literal_namespace_not_preserved() {
    // When generating XMLLiterals, active namespace prefixes should be injected
    // into top-level elements. This is not yet implemented.
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body prefix="ex: http://example.org/">
        <div property="ex:content"
             datatype="http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral">
            <span>namespaced content</span>
        </div>
    </body>
    </html>"#;

    let (output, ()) = parse_html(html);

    let literal = output
        .iter()
        .find(|t| t.predicate.as_str() == "http://example.org/content")
        .expect("should have a triple with the property");

    let lit = match literal.object {
        oxrdf::TermRef::Literal(l) => l,
        _ => panic!("expected a literal object"),
    };

    // Currently the XMLLiteral does NOT contain namespace declarations.
    // When the TODO is fixed, this assertion should be updated to check
    // that xmlns:ex="http://example.org/" is present in the literal value.
    assert!(
        !lit.value().contains("xmlns"),
        "namespace preservation is not yet implemented — literal should not contain xmlns (got: {})",
        lit.value()
    );
}

// host_language/html.rs:57-58, xhtml.rs:19-20 — default_vocabulary() returns
// None for both HTML5 and XHTML. This is correct per the specs ("The default
// vocabulary URI is undefined"), not a TODO.
#[test]
fn no_default_vocabulary_without_explicit_vocab() {
    // Without an explicit @vocab, there is no default vocabulary.
    // Terms like "name" won't resolve to anything.
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div typeof="Thing">
            <span property="name">foo</span>
        </div>
    </body>
    </html>"#;

    let (output, ()) = parse_html(html);

    // Without a default vocabulary, "name" and "Thing" are bare terms that
    // cannot be resolved to IRIs, so no triples should be emitted for them.
    let serialized = utils::serialize_normalized_graph(output, base().as_str());
    assert!(
        !serialized.contains("name"),
        "bare term 'name' should not resolve without a default vocabulary, got:\n{serialized}"
    );
}
