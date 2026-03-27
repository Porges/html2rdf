use std::{convert::Infallible, fmt::Debug};

use curie::PrefixMapping;
use ouroboros::self_referencing;
use oxilangtag::LanguageTag;
use oxiri::Iri;
use scraper::Html;
use tracing::trace;

use crate::{
    errors::{Error, IriError},
    graphs::{PGClass, ProcessorGraph},
};

#[derive(Clone, Default)]
pub struct Html5 {}

#[self_referencing]
pub struct Element<'a> {
    el_ref: scraper::ElementRef<'a>,
    #[borrows(el_ref)]
    el: &'this scraper::node::Element,
}

impl Debug for Element<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.borrow_el().fmt(f)
    }
}

impl<'a> Element<'a> {
    fn from_el(el: scraper::ElementRef<'a>) -> Self {
        ElementBuilder {
            el_ref: el,
            el_builder: |el_ref| el_ref.value(),
        }
        .build()
    }
}

// [HTML-RDFA] 3.1
// “Documents conforming to the rules in this specification are processed
//  according to [rdfa-core] with the following extensions:
impl super::HostLanguage for Html5 {
    type DocumentType<'a> = Html;
    type ParseError = Infallible;

    fn initial_terms(&self) -> &'static std::collections::BTreeMap<String, oxrdf::NamedNode> {
        crate::initial_context::terms()
    }

    fn initial_prefixes(&self) -> &'static curie::PrefixMapping {
        crate::initial_context::prefixes()
    }

    // “The default vocabulary URI is undefined.
    fn default_vocabulary(&self) -> Option<oxrdf::NamedNode> {
        None
    }

    fn default_language(&self) -> Option<LanguageTag<String>> {
        None
    }

    fn establish_base<'a>(&self, doc: &'a Html) -> Result<Option<Iri<&'a str>>, Error> {
        let base_sel = scraper::selector::Selector::parse("html>head>base").unwrap(); // UNWRAP: tested every time this is run
        if let Some(base_el) = doc.select(&base_sel).next()
            && let Some(base_href) = base_el.attr("href")
        {
            let base = Iri::parse(base_href).map_err(|source| {
                Error::IriError(IriError {
                    source,
                    value: base_href.to_string(),
                })
            })?;

            trace!(%base, "<base> found");

            return Ok(Some(base));
        }

        Ok(None)
    }

    // “HTML+RDFa uses an additional initial context by default,
    //  http://www.w3.org/2011/rdfa-context/html-rdfa-1.1, which must
    //  be applied after the initial context for [rdfa-core]
    //  (http://www.w3.org/2011/rdfa-context/rdfa-1.1).
    // NB: note that the "additional initial context" is currently empty.

    fn parse_document<'a>(
        &self,
        doc: &'a str,
        doc_iri: Iri<&str>,
        processor_graph: &mut impl ProcessorGraph,
    ) -> Result<Self::DocumentType<'a>, Self::ParseError> {
        let result = Html::parse_document(doc);
        for err in &result.errors {
            processor_graph.emit_message(
                PGClass::DocumentError,
                err.as_ref(),
                Some(doc_iri.as_str()),
            );
        }

        Ok(result)
    }
}

impl super::Document for Html {
    type ElementType<'a> = Element<'a>;

    fn root_element(&self) -> Option<Self::ElementType<'_>> {
        Some(Element::from_el(self.root_element()))
    }
}

impl super::Element for Element<'_> {
    fn xml_attr(&self, name: &str) -> Option<&str> {
        self.borrow_el_ref().attr(&format!("xml:{name}"))
    }

    fn attr(&self, name: &str) -> Option<&str> {
        self.borrow_el_ref().attr(name)
    }

    fn tag_name(&self) -> &str {
        self.borrow_el().name()
    }

    fn text(&self) -> String {
        self.borrow_el_ref().text().collect::<String>()
    }

    fn inner_html(&self) -> String {
        self.borrow_el_ref().inner_html()
    }

    fn inner_xml(&self, _active_mappings: &PrefixMapping) -> String {
        // TODO: properly normalize this
        self.inner_html()
    }

    fn has_children(&self) -> bool {
        self.borrow_el_ref().has_children()
    }

    fn child_elements(&self) -> impl DoubleEndedIterator<Item = Self> {
        self.borrow_el_ref()
            .children()
            .filter_map(|child| scraper::ElementRef::wrap(child).map(Element::from_el))
    }

    fn namespaces(&self) -> impl Iterator<Item = (&str, &str)> {
        self.borrow_el().attrs.iter().filter_map(|(qname, value)| {
            Some((qname.local.as_ref().strip_prefix("xmlns")?, value.as_ref()))
        })
    }
}
