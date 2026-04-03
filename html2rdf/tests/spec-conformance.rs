//! Tests for spec conformance issues identified in the RDFa review.
//!
//! These tests verify behavior required by rdfa-core, html-rdfa, and xhtml-rdfa
//! specifications.

mod utils;
use utils::{assert_ask, assert_not_ask, parse_html};

// ============================================================================
// Issue 1: HTML5 xmlns prefix stripping
//
// [html-rdfa] §5.5.1: "For each Attr... that has a local name that starts with
// @xmlns:, create an IRI mapping by storing the local name part with the
// @xmlns: characters removed..."
//
// ============================================================================

/// A well-known prefix (dc) declared via xmlns: should work because it is
/// also in the initial context.
#[test]
fn xmlns_well_known_prefix_works() {
    let html = r#"<!DOCTYPE html>
    <html xmlns:dc="http://purl.org/dc/terms/">
    <head><title>test</title></head>
    <body>
        <span about="http://example.test/item" property="dc:title">Hello</span>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);
    assert_ask(&output, r#"
        ASK { <http://example.test/item> <http://purl.org/dc/terms/title> "Hello" }
    "#);
}

/// A custom (non-initial-context) prefix declared via xmlns: should also work.
#[test]
fn xmlns_custom_prefix_works() {
    let html = r#"<!DOCTYPE html>
    <html xmlns:myns="http://example.test/ns/">
    <head><title>test</title></head>
    <body>
        <span about="http://example.test/item" property="myns:value">42</span>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);
    assert_ask(&output, r#"
        ASK { <http://example.test/item> <http://example.test/ns/value> "42" }
    "#);
}

/// Verify that xmlns and @prefix declarations for the same custom prefix
/// produce the same result.
#[test]
fn xmlns_and_prefix_attr_produce_same_result() {
    let html_xmlns = r#"<!DOCTYPE html>
    <html xmlns:myns="http://example.test/ns/">
    <head><title>test</title></head>
    <body>
        <span about="http://example.test/item" property="myns:value">42</span>
    </body>
    </html>"#;

    let html_prefix = r#"<!DOCTYPE html>
    <html prefix="myns: http://example.test/ns/">
    <head><title>test</title></head>
    <body>
        <span about="http://example.test/item" property="myns:value">42</span>
    </body>
    </html>"#;

    let (output_xmlns, _) = parse_html(html_xmlns);
    let (output_prefix, _) = parse_html(html_prefix);

    assert!(
        utils::check_graphs_eq(&output_xmlns, &output_prefix),
        "xmlns:myns and prefix=\"myns:\" should produce identical graphs"
    );
}

// ============================================================================
// Issue 2: Prefix names not lowercased
//
// [rdfa-core] §7.5, step 3: "the value to be mapped MUST be converted to
// lower case"
//
// ============================================================================

/// Uppercase prefix declaration via @prefix should be matched case-insensitively.
#[test]
fn prefix_attr_uppercase_is_lowered() {
    let html = r#"<!DOCTYPE html>
    <html prefix="EX: http://example.test/ns/">
    <head><title>test</title></head>
    <body>
        <span about="http://example.test/item" property="ex:value">42</span>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);
    assert_ask(&output, r#"
        ASK { <http://example.test/item> <http://example.test/ns/value> "42" }
    "#);
}

/// Mixed-case prefix should be normalized to lowercase for matching.
#[test]
fn prefix_attr_mixed_case_is_lowered() {
    let html = r#"<!DOCTYPE html>
    <html prefix="MyNs: http://example.test/ns/">
    <head><title>test</title></head>
    <body>
        <span about="http://example.test/item" property="myns:value">42</span>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);
    assert_ask(&output, r#"
        ASK { <http://example.test/item> <http://example.test/ns/value> "42" }
    "#);
}

/// Using an uppercase CURIE prefix to match a lowercase declaration should work
/// because CURIE prefix lookup must be case-insensitive ([rdfa-core] §6).
#[test]
fn curie_prefix_lookup_is_case_insensitive() {
    let html = r#"<!DOCTYPE html>
    <html prefix="ex: http://example.test/ns/">
    <head><title>test</title></head>
    <body>
        <span about="http://example.test/item" property="EX:value">42</span>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);
    assert_ask(&output, r#"
        ASK { <http://example.test/item> <http://example.test/ns/value> "42" }
    "#);
}

// ============================================================================
// Issue 3: Missing property emission in process_chaining incomplete branch
//
// [rdfa-core] §7.5, step 11: "@property" values MUST always be emitted when
// @property is present, even when @rel/@rev produced incomplete triples
// (step 10).
//
// ============================================================================

/// @property with @rel, @about present, but no @resource — property should
/// still emit a plain literal.
#[test]
fn rel_and_property_without_resource_emits_literal() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div about="http://example.test/alice"
             rel="http://example.test/knows"
             property="http://example.test/title">Hello World</div>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);
    assert_ask(&output, r#"
        ASK { <http://example.test/alice> <http://example.test/title> "Hello World" }
    "#);
}

/// @property with @rev, @about present, but no @resource — property should
/// still emit a plain literal.
#[test]
fn rev_and_property_without_resource_emits_literal() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div about="http://example.test/bob"
             rev="http://example.test/knowsOf"
             property="http://example.test/label">Bob</div>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);
    assert_ask(&output, r#"
        ASK { <http://example.test/bob> <http://example.test/label> "Bob" }
    "#);
}

/// Ensure that when a child later completes the incomplete triple, the
/// @property literal is also present.
#[test]
fn rel_property_incomplete_then_completed_by_child() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div about="http://example.test/alice"
             rel="http://example.test/knows"
             property="http://example.test/label">Alice's connections
            <span about="http://example.test/bob"></span>
        </div>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);

    // The incomplete triple from @rel should be completed by child's @about
    assert_ask(&output, r#"
        ASK { <http://example.test/alice> <http://example.test/knows> <http://example.test/bob> }
    "#);

    // The @property literal should also be present
    assert_ask(&output, r#"
        ASK { <http://example.test/alice> <http://example.test/label> ?val
              FILTER(isLiteral(?val)) }
    "#);
}

// ============================================================================
// Issue 4: Missing typed-resource property fallback in process_chaining
// resource branch
//
// [rdfa-core] §7.5, step 11: "otherwise, if @typeof is present and @about is
// not, the value of typed resource"
//
// Step 6: "If @typeof is present and @about is not, set typed resource to
// current object resource"
//
// ============================================================================

/// When @rel + @property + @resource + @typeof are present without @about,
/// @property should use the typed resource (= @resource) as its value.
#[test]
fn rel_property_resource_typeof_no_about_uses_typed_resource() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div rel="http://example.test/rel"
             property="http://example.test/prop"
             resource="http://example.test/target"
             typeof="http://example.test/Type">
            Some text
        </div>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);
    assert_ask(&output, r#"
        ASK {
            <http://example.test/target> a <http://example.test/Type> .
            <http://example.test/> <http://example.test/rel> <http://example.test/target> .
            <http://example.test/> <http://example.test/prop> <http://example.test/target> .
        }
    "#);
}

/// Verify that when @about IS present alongside @rel + @property + @resource +
/// @typeof, the @property value is a plain literal (not the typed resource),
/// because step 11 condition 6 requires @about to be absent.
#[test]
fn rel_property_resource_typeof_with_about_uses_literal() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div about="http://example.test/subject"
             rel="http://example.test/rel"
             property="http://example.test/prop"
             resource="http://example.test/target"
             typeof="http://example.test/Type">
            Some text
        </div>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);

    // @typeof types @about (not @resource) when @about is present
    assert_ask(&output, r#"
        ASK { <http://example.test/subject> a <http://example.test/Type> }
    "#);

    // @property should be a plain literal (condition 6 doesn't apply when @about is present)
    assert_ask(&output, r#"
        ASK { <http://example.test/subject> <http://example.test/prop> ?val
              FILTER(isLiteral(?val)) }
    "#);
}

/// When @rel + @property + @typeof are present without @about and without
/// @resource, the typed resource is a bnode. The @property value should
/// use that bnode. (Regression test — this case already works.)
#[test]
fn rel_property_typeof_no_about_no_resource_uses_bnode() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body>
        <div rel="http://example.test/rel"
             property="http://example.test/prop"
             typeof="http://example.test/Type">
            Some text
        </div>
    </body>
    </html>"#;

    let (output, _) = parse_html(html);

    // There should be a bnode that is typed and used as the property value
    assert_ask(&output, r#"
        ASK {
            ?bnode a <http://example.test/Type> .
            ?s <http://example.test/prop> ?bnode .
            FILTER(isBlank(?bnode))
        }
    "#);
}

// ============================================================================
// Issue 8: head/body check missing from process_complex_property (step 5.1)
//
// [html-rdfa] §3.1, extension #8: "In Section 7.5, processing step 5, and
// processing step 6, if no IRI is provided by a resource attribute (e.g.,
// @about, @href, @resource, or @src), then first check to see if the element
// is the head or body element. If it is, then set new subject to parent
// object."
//
// This test documents that <body typeof="..." property="..."> works correctly.
// The code happens to produce the correct result because the default fallback
// in process_complex_property inherits parent object, matching extension #8.
// ============================================================================

/// <body> with @typeof + @property (step 5.1) should set new subject to
/// parent object (= base) per extension #8, and typed resource to a bnode.
/// The @property value should be the typed bnode, and rdf:type should go
/// on the bnode — not on the base.
#[test]
fn body_typeof_property_uses_parent_object_as_subject() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head><title>test</title></head>
    <body property="http://example.test/prop"
          typeof="http://example.test/Type">
        content
    </body>
    </html>"#;

    let (output, _) = parse_html(html);

    // The base IRI should NOT be typed — a bnode should be
    assert_not_ask(&output, r#"
        ASK { <http://example.test/> a <http://example.test/Type> }
    "#);

    // A bnode should be typed, and the base should link to it via @property
    assert_ask(&output, r#"
        ASK {
            ?bnode a <http://example.test/Type> .
            <http://example.test/> <http://example.test/prop> ?bnode .
            FILTER(isBlank(?bnode))
        }
    "#);
}

/// Same as above but for <head> — extension #8 covers both.
#[test]
fn head_typeof_property_uses_parent_object_as_subject() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head property="http://example.test/prop"
          typeof="http://example.test/Type">
        <title>test</title>
    </head>
    <body></body>
    </html>"#;

    let (output, _) = parse_html(html);

    assert_not_ask(&output, r#"
        ASK { <http://example.test/> a <http://example.test/Type> }
    "#);

    assert_ask(&output, r#"
        ASK {
            ?bnode a <http://example.test/Type> .
            <http://example.test/> <http://example.test/prop> ?bnode .
            FILTER(isBlank(?bnode))
        }
    "#);
}
