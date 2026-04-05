use insta::assert_snapshot;
use oxrdf::{Graph, TermRef, vocab::rdf};

use crate::utils::serialize_graph;

mod utils;

fn double_check_xml_literal(graph: &Graph) {
    // find all XML literals and ensure that if we re-process them,
    // all of the generated triples already appear in the graph
    for triple in graph {
        if let TermRef::Literal(lit) = triple.object
            && lit.datatype() == rdf::XML_LITERAL
        {
            // embed fragment in new document and parse it
            let (new_graph, _) =
                utils::parse_xhtml(&format!("<html><body>{}</body></html>", lit.value()));
            for new_triple in &new_graph {
                assert!(graph.contains(new_triple));
            }
        }
    }
}

#[test]
fn element_prefixed_namespace_present() {
    let input = r#"<?xml version="1.0" encoding="UTF-8"?>
    <html xmlns="http://www.w3.org/1999/xhtml" xmlns:rdfa="http://www.w3.org/ns/rdfa#">
    <body>
    <div property="ex:desc" datatype="rdf:XMLLiteral"><k:p xmlns:k="http://another.ns/">...</k:p></div>
    </body>
    </html>"#;
    let (g, _) = utils::parse_xhtml(input);
    double_check_xml_literal(&g);
    insta::assert_snapshot!(serialize_graph(&g, utils::base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    <> <ex:desc> "<k:p xmlns:k=\"http://another.ns/\" xmlns:rdfa=\"http://www.w3.org/ns/rdfa#\">...</k:p>"^^rdf:XMLLiteral .
    "#);
}

#[test]
fn element_default_namespace_present() {
    let input = r#"<?xml version="1.0" encoding="UTF-8"?>
    <html xmlns="http://www.w3.org/1999/xhtml" xmlns:rdfa="http://www.w3.org/ns/rdfa#">
    <body>
    <div property="ex:desc" datatype="rdf:XMLLiteral"><p xmlns="http://another.ns/">...</p></div>
    </body>
    </html>"#;
    let (g, _) = utils::parse_xhtml(input);
    double_check_xml_literal(&g);
    insta::assert_snapshot!(serialize_graph(&g, utils::base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    <> <ex:desc> "<p xmlns=\"http://another.ns/\" xmlns:rdfa=\"http://www.w3.org/ns/rdfa#\">...</p>"^^rdf:XMLLiteral .
    "#);
}

#[test]
fn element_prefixed_namespace_xhtml_inside() {
    let input = r#"<?xml version="1.0" encoding="UTF-8"?>
    <html xmlns="http://www.w3.org/1999/xhtml">
    <body>
    <div property="ex:desc" datatype="rdf:XMLLiteral"><k:p xmlns:k="http://another.ns/">.<b>.</b>.</k:p></div>
    </body>
    </html>"#;
    let (g, _) = utils::parse_xhtml(input);
    double_check_xml_literal(&g);
    insta::assert_snapshot!(serialize_graph(&g, utils::base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    <> <ex:desc> "<k:p xmlns:k=\"http://another.ns/\">.<b xmlns=\"http://www.w3.org/1999/xhtml\">.</b>.</k:p>"^^rdf:XMLLiteral .
    "#);
}

#[test]
fn element_prefixed_namespace_parent_xhtml_inside() {
    let input = r#"<?xml version="1.0" encoding="UTF-8"?>
    <html xmlns="http://www.w3.org/1999/xhtml">
    <body>
    <div property="ex:desc" datatype="rdf:XMLLiteral"><k:p xmlns:k="http://another.ns/">.<b>.</b>.</k:p></div>
    </body>
    </html>"#;
    let (g, _) = utils::parse_xhtml(input);
    double_check_xml_literal(&g);
    insta::assert_snapshot!(serialize_graph(&g, utils::base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    <> <ex:desc> "<k:p xmlns:k=\"http://another.ns/\">.<b xmlns=\"http://www.w3.org/1999/xhtml\">.</b>.</k:p>"^^rdf:XMLLiteral .
    "#);
}

#[test]
fn element_prefix_xmlns_conflict_parent() {
    let input = r#"<?xml version="1.0" encoding="UTF-8"?>
    <html xmlns="http://www.w3.org/1999/xhtml">
    <body>
    <div property="ex:desc" datatype="rdf:XMLLiteral" xmlns:k="http://another.ns/" prefix="k: http://prefix.ns/"><k:p>.<b property="k:foo">value</b>.</k:p></div>
    </body>
    </html>"#;
    let (g, _) = utils::parse_xhtml(input);
    double_check_xml_literal(&g);
    insta::assert_snapshot!(serialize_graph(&g, utils::base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    <> <ex:desc> "<k:p xmlns:k=\"http://another.ns/\" prefix=\"k: http://prefix.ns/\">.<b xmlns=\"http://www.w3.org/1999/xhtml\" property=\"k:foo\">value</b>.</k:p>"^^rdf:XMLLiteral ;
    	<//prefix.ns/foo> "value" .
    "#);
}

#[test]
fn element_prefix_xmlns_conflict_parent_conflict_child() {
    // if moving the prefix into the child we need to make sure we don't overwrite any existing prefixes!
    let input = r#"<?xml version="1.0" encoding="UTF-8"?>
    <html xmlns="http://www.w3.org/1999/xhtml">
    <body>
    <div property="ex:desc" datatype="rdf:XMLLiteral" xmlns:k="http://another.ns/" prefix="k: http://prefix.ns/"><k:p prefix="k: http://child.ns/">.<b property="k:foo">value</b>.</k:p></div>
    </body>
    </html>"#;
    let (g, _) = utils::parse_xhtml(input);
    double_check_xml_literal(&g);
    insta::assert_snapshot!(serialize_graph(&g, utils::base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    <> <ex:desc> "<k:p xmlns:k=\"http://another.ns/\" prefix=\"k: http://child.ns/\">.<b xmlns=\"http://www.w3.org/1999/xhtml\" property=\"k:foo\">value</b>.</k:p>"^^rdf:XMLLiteral ;
    	<//child.ns/foo> "value" .
    "#);
}

#[test]
fn element_prefix_xmlns_conflict_parent_conflict_child_merged() {
    let input = r#"<?xml version="1.0" encoding="UTF-8"?>
    <html xmlns="http://www.w3.org/1999/xhtml">
    <body>
    <div property="ex:desc" datatype="rdf:XMLLiteral" xmlns:k="http://another.ns/" prefix="k: http://prefix.ns/ j: http://j.prefix/"><k:p xmlns:j="http://j.xmlns/" prefix="k: http://child.ns/">.<b property="k:foo">value</b>.</k:p></div>
    </body>
    </html>"#;
    let (g, _) = utils::parse_xhtml(input);
    double_check_xml_literal(&g);
    insta::assert_snapshot!(serialize_graph(&g, utils::base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    <> <ex:desc> "<k:p xmlns:j=\"http://j.xmlns/\" xmlns:k=\"http://another.ns/\" prefix=\"k: http://child.ns/\">.<b xmlns=\"http://www.w3.org/1999/xhtml\" property=\"k:foo\">value</b>.</k:p>"^^rdf:XMLLiteral ;
    	<//child.ns/foo> "value" .
    "#);
}

#[test]
fn element_prefix_xmlns_conflict_parent_overridden() {
    // we don't copy prefix into the child if the child has an XMLNS for the same prefix
    let input = r#"<?xml version="1.0" encoding="UTF-8"?>
    <html xmlns="http://www.w3.org/1999/xhtml">
    <body>
    <div property="ex:desc" datatype="rdf:XMLLiteral" xmlns:k="http://another.ns/" prefix="k: http://prefix.ns/"><k:p xmlns:k="http://child.ns/">.<b property="k:foo">value</b>.</k:p></div>
    </body>
    </html>"#;
    let (g, _) = utils::parse_xhtml(input);
    double_check_xml_literal(&g);
    insta::assert_snapshot!(serialize_graph(&g, utils::base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    <> <ex:desc> "<k:p xmlns:k=\"http://child.ns/\">.<b xmlns=\"http://www.w3.org/1999/xhtml\" property=\"k:foo\">value</b>.</k:p>"^^rdf:XMLLiteral ;
    	<//child.ns/foo> "value" .
    "#);
}

#[test]
fn element_prefix_xmlns_conflict_child() {
    // moving the XMLNS down is safe because the prefix takes precedence
    let input = r#"<?xml version="1.0" encoding="UTF-8"?>
    <html xmlns="http://www.w3.org/1999/xhtml">
    <body>
    <div property="ex:desc" datatype="rdf:XMLLiteral" xmlns:k="http://another.ns/"><k:p prefix="k: http://prefix.ns/">.<b property="k:foo">value</b>.</k:p></div>
    </body>
    </html>"#;
    let (g, _) = utils::parse_xhtml(input);
    double_check_xml_literal(&g);
    insta::assert_snapshot!(serialize_graph(&g, utils::base().as_str()), @r#"
    @base <http://example.test/> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    <> <ex:desc> "<k:p xmlns:k=\"http://another.ns/\" prefix=\"k: http://prefix.ns/\">.<b xmlns=\"http://www.w3.org/1999/xhtml\" property=\"k:foo\">value</b>.</k:p>"^^rdf:XMLLiteral ;
    	<//prefix.ns/foo> "value" .
    "#);
}

/// When @prefix lowercases FOO→foo, and xmlns:foo already exists with a
/// different URI, we must ensure that it doesn't affect the XML namespaces.
#[test]
fn lowered_prefix_shadows_xmlns_in_xmlliteral() {
    let input = r#"<?xml version="1.0" encoding="UTF-8"?>
    <html xmlns="http://www.w3.org/1999/xhtml" xmlns:foo="http://original.ns/">
    <body prefix="FOO: http://rdfa-prefix.ns/">
    <div property="ex:desc" datatype="rdf:XMLLiteral">
      <foo:bar>content</foo:bar>
    </div>
    </body>
    </html>"#;
    let (g, _) = utils::parse_xhtml(input);

    // The XMLLiteral should preserve the original XML namespace for foo:bar,
    // i.e. xmlns:foo="http://original.ns/", NOT the lowered @prefix value.
    let xml_literal = g
        .iter()
        .find_map(|t| match t.object {
            oxrdf::TermRef::Literal(l) if l.datatype() == rdf::XML_LITERAL => {
                Some(l.value().to_string())
            }
            _ => None,
        })
        .expect("should have XMLLiteral");

    // foo:bar must be in namespace http://original.ns/, not http://rdfa-prefix.ns/
    assert_snapshot!(xml_literal,
        @r#"<foo:bar xmlns:foo="http://original.ns/" prefix="foo: http://rdfa-prefix.ns/">content</foo:bar>"#);
}
