// Tests that exercise known TODO/unimplemented code paths in the library.
use html2rdf::parse;
use oxiri::Iri;
use oxrdf::Graph;

mod utils;

fn base() -> Iri<String> {
    Iri::parse("http://example.org/".to_string()).unwrap()
}

fn parse_html(html: &str) -> (Graph, Graph) {
    let mut output_graph = Graph::new();
    let mut processor_graph = Graph::new();
    parse(html, base(), &mut output_graph, &mut processor_graph).unwrap();
    (output_graph, processor_graph)
}

// lib.rs:1693-1695 — XMLLiteral datatype uses inner_html() without XML canonicalization (c14n)
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

    let (output, _processor) = parse_html(html);

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

// lib.rs:1696-1700 — rdf:HTML datatype uses inner_html() without canonicalization
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

    let (output, _processor) = parse_html(html);

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

// lib.rs:1083,1088— add_prefix().expect("TODO") blindly unwraps
// The curie crate currently accepts most prefix strings, so this test
// documents that invalid-looking prefixes don't error and instead succeed.
#[test]
fn invalid_looking_prefix_declaration_succeeds() {
    // A prefix starting with digits is technically invalid per XML NCName rules,
    // but the curie crate accepts it. The .expect("TODO") doesn't panic here,
    // but it would if the curie crate ever started validating prefix names.
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body prefix="123bad: http://example.org/">
        <span property="123bad:thing">value</span>
    </body>
    </html>"#;

    let (output, _processor) = parse_html(html);
    let serialized = utils::serialize_graph(output, base().as_str());

    // The prefix was accepted and the CURIE resolved.
    // Since base is http://example.org/, <thing> is the relative form of http://example.org/thing.
    assert!(
        serialized.contains("<thing>"),
        "prefix should have been accepted and CURIE expanded, got:\n{serialized}"
    );
}

// lib.rs:845 — list elements need a stack marker (incomplete list support)
#[test]
fn inlist_produces_rdf_list() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div vocab="http://schema.org/">
            <span rel="http://example.org/items" inlist="">
                <a href="http://example.org/a">A</a>
            </span>
            <span rel="http://example.org/items" inlist="">
                <a href="http://example.org/b">B</a>
            </span>
        </div>
    </body>
    </html>"#;

    let (output, _processor) = parse_html(html);

    let serialized = utils::serialize_graph(output, base().as_str());

    // Verify the list items are present (the TODO is about emitting them correctly
    // via a stack marker). The items appear as relative IRIs since base is http://example.org/.
    assert!(
        serialized.contains("rdf:first"),
        "should contain rdf:first list triples, got:\n{serialized}"
    );
}

// lib.rs:1675 — XMLLiteral namespace preservation is not implemented
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

    let (output, _processor) = parse_html(html);

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

// lib.rs:788 — HTMLHost::default_vocabulary returns None (TODO)
// lib.rs:806, 810 — XHTMLHost::default_vocabulary and default_language return None (TODO)
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

    let (output, _processor) = parse_html(html);

    // Without a default vocabulary, "name" and "Thing" are bare terms that
    // cannot be resolved to IRIs, so no triples should be emitted for them.
    let serialized = utils::serialize_graph(output, base().as_str());
    assert!(
        !serialized.contains("name"),
        "bare term 'name' should not resolve without a default vocabulary, got:\n{serialized}"
    );
}

// lib.rs:1400 — TODO: check inside/outside parent if for @typeof + @about
#[test]
fn typeof_with_about_sets_typed_resource() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body vocab="http://schema.org/">
        <div about="http://example.org/thing" typeof="Thing">
            <span property="name">My Thing</span>
        </div>
    </body>
    </html>"#;

    let (output, _processor) = parse_html(html);
    let serialized = utils::serialize_graph(output, base().as_str());

    // @typeof with @about should type the resource from @about.
    // The output uses relative IRIs since base is http://example.org/.
    assert!(
        serialized.contains("<thing>"),
        "should have triples about the @about resource, got:\n{serialized}"
    );
    assert!(
        serialized.contains("Thing"),
        "should have the type from @typeof, got:\n{serialized}"
    );
}
