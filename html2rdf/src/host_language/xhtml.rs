use std::fmt::Write;
use std::sync::OnceLock;

use bergshamra_xml::NodeSet;
use curie::PrefixMapping;
use oxilangtag::LanguageTag;
use oxrdf::NamedNode;
use uppsala::{QName, namespace::XML_NAMESPACE};

use crate::{
    errors::{Error, IriError},
    host_language::HostLanguage,
};

#[derive(Default, Clone)]
pub struct XHtml {}

const XHTML_NAMESPACE: &str = "http://www.w3.org/1999/xhtml";

impl HostLanguage for XHtml {
    type ParseError = uppsala::XmlError;
    type DocumentType<'a> = uppsala::Document<'a>;

    fn default_vocabulary(&self) -> Option<NamedNode> {
        None
    }

    fn default_language(&self) -> Option<LanguageTag<String>> {
        None
    }

    fn establish_base<'a>(
        &self,
        doc: &'a Self::DocumentType<'a>,
    ) -> Result<Option<oxiri::Iri<&'a str>>, Error> {
        // TODO: xml:base
        if let Some(html) = doc.first_child_element_by_name_ns(doc.root(), XHTML_NAMESPACE, "html")
            && let Some(head) = doc.first_child_element_by_name_ns(html, XHTML_NAMESPACE, "head")
            && let Some(base) = doc.first_child_element_by_name_ns(head, XHTML_NAMESPACE, "base")
            && let Some(href) = doc.get_attribute(base, "href")
        {
            let iri = oxiri::Iri::parse(href).map_err(|source| {
                Error::from(IriError {
                    source,
                    value: href.to_string(),
                })
            })?;

            return Ok(Some(iri));
        }

        Ok(None)
    }

    fn parse_document<'a>(
        &self,
        doc: &'a str,
        _doc_iri: oxiri::Iri<&str>,
        _processor_graph: &mut impl crate::graphs::ProcessorGraph,
    ) -> Result<Self::DocumentType<'a>, Self::ParseError> {
        uppsala::parse(doc)
    }

    fn initial_terms(&self) -> &'static std::collections::BTreeMap<String, NamedNode> {
        // XHTML extends the default
        // https://www.w3.org/2011/rdfa-context/rdfa-1.1
        static INITIAL_TERMS: OnceLock<std::collections::BTreeMap<String, NamedNode>> =
            OnceLock::new();
        INITIAL_TERMS.get_or_init(|| {
            let mut terms = crate::initial_context::terms().clone();
            for t in [
                "alternate",
                "appendix",
                "cite",
                "bookmark",
                "contents",
                "chapter",
                "copyright",
                "first",
                "glossary",
                "help",
                "icon",
                "index",
                "last",
                "license",
                "meta",
                "next",
                "prev",
                "previous",
                "section",
                "start",
                "stylesheet",
                "subsection",
                "top",
                "up",
            ] {
                terms.insert(
                    t.to_string(),
                    NamedNode::new_unchecked(format!("http://www.w3.org/1999/xhtml/vocab#{t}")),
                );
            }

            terms.insert(
                "p3pv1".to_string(),
                NamedNode::new_unchecked("http://www.w3.org/1999/xhtml/vocab#p3pv1"),
            );

            terms
        })
    }

    fn initial_prefixes(&self) -> &'static curie::PrefixMapping {
        crate::initial_context::prefixes()
    }
}

impl super::Document for uppsala::Document<'_> {
    type ElementType<'a>
        = (&'a Self, uppsala::NodeId)
    where
        Self: 'a;

    fn root_element(&self) -> Option<Self::ElementType<'_>> {
        self.document_element().map(|node_id| (self, node_id))
    }
}

impl super::Element for (&uppsala::Document<'_>, uppsala::NodeId) {
    fn attr(&self, name: &str) -> Option<&str> {
        self.0.get_attribute(self.1, name)
    }

    fn xml_attr(&self, name: &str) -> Option<&str> {
        self.0.get_attribute_ns(self.1, XML_NAMESPACE, name)
    }

    fn tag_name(&self) -> &str {
        self.0.element(self.1).unwrap().name.local_name.as_ref()
    }

    fn text(&self) -> String {
        self.0.text_content_deep(self.1)
    }

    fn inner_html(&self) -> String {
        let mut result = String::new();
        for child in self.0.children_iter(self.1) {
            result.push_str(self.0.node_source(child).unwrap_or_default());
        }
        result
    }

    fn inner_xml(&self, active_mappings: &PrefixMapping) -> String {
        // at the moment we need to clone to add the prefixes
        let mut doc = self.0.clone();
        // NOTE: I assume that ids are stable when cloning
        let el = doc.element_mut(self.1).unwrap();

        // copy in all the active mappings as xmlns
        // this is simpler than tracking whether we need to add them as xmlns
        // or as prefixes, and it matches what the test suite does
        let mut prefixes = Vec::new();
        for (prefix, value) in active_mappings.mappings() {
            // note that there's a little tricky case where a prefix redefined a xmlns
            // in which case we need to ensure that _both_ are present on child nodes
            // so the XML side and the RDFa side remain correct
            if el
                .namespace_declarations
                .iter()
                .any(|(p, v)| p == prefix && v != value)
            {
                prefixes.push((prefix, value));
            } else {
                el.namespace_declarations
                    .push((prefix.into(), value.into()));
            }
        }

        // add the prefixes, if needed, to the child elements
        // again we have to be careful that we don't overwrite a
        // child element's xmlns which redefines it
        if !prefixes.is_empty() {
            for child in doc.children(self.1) {
                if let Some(child_el) = doc.element_mut(child) {
                    let mut prefix_attr = String::new();
                    for (prefix, value) in &prefixes {
                        // we insert it as long as:
                        if
                        // -the child has no xmlns declaration for that prefix
                        !child_el
                            .namespace_declarations
                            .iter()
                            .any(|(p, _)| p.as_ref() == prefix.as_str())
                            // - and it doesn't have its own prefix definition for that prefix
                            && child_el.get_attribute("prefix").is_none_or(|p| !p.contains(&format!("{prefix}: ")))
                        {
                            if !prefix_attr.is_empty() {
                                prefix_attr.push(' ');
                            }
                            _ = write!(prefix_attr, "{prefix}: {value} ");
                        }
                    }
                    if !prefix_attr.is_empty() {
                        child_el.set_attribute(QName::local("prefix"), prefix_attr.into());
                    }
                }
            }
        }

        let mut ns = NodeSet::tree_with_comments(self.1, self.0); // node and all descendants
        ns.remove_id(self.1); // remove the node itself

        // make sure any xmlns which are potentially referenced _only_ in RDFa are still included
        let inclusive_prefixes = active_mappings
            .mappings()
            .map(|(prefix, _)| prefix)
            .collect::<Vec<_>>();

        // generate the canonicalized XML!
        let canon =
            bergshamra_c14n::exclusive::canonicalize(&doc, false, Some(&ns), &inclusive_prefixes)
                .expect("generating canonicalized XML");

        String::from_utf8_lossy(&canon).into_owned()
    }

    fn has_children(&self) -> bool {
        self.0.first_child(self.1).is_some()
    }

    fn child_elements(&self) -> impl DoubleEndedIterator<Item = Self> {
        self.0
            .children(self.1)
            .into_iter()
            .filter_map(|child| self.0.element(child).map(|_| (self.0, child)))
    }

    fn namespaces(&self) -> impl Iterator<Item = (&str, &str)> {
        self.0
            .element(self.1)
            .unwrap()
            .namespace_declarations
            .iter()
            .map(|(prefix, uri)| (prefix.as_ref(), uri.as_ref()))
    }
}
