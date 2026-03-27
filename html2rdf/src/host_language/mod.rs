//! Abstractions and implementations for RDFa host languages.

use std::{collections::BTreeMap, fmt::Debug};

use curie::PrefixMapping;
use oxilangtag::LanguageTag;
use oxiri::Iri;
use oxrdf::NamedNode;

#[cfg(feature = "html")]
mod html;

#[cfg(feature = "html")]
pub use html::Html5;

#[cfg(feature = "xhtml")]
mod xhtml;

#[cfg(feature = "xhtml")]
pub use xhtml::XHtml;

use crate::{errors::Error, graphs::ProcessorGraph};

/// An RDFa host language.
pub trait HostLanguage: Clone {
    /// The error type returned in parsing fails.
    type ParseError;

    /// The type of document returned.
    type DocumentType<'i>: Document;

    /// The set of 'initial terms' for this host language.
    ///
    /// These are used to resolve Terms in RDFa.
    fn initial_terms(&self) -> &'static BTreeMap<String, oxrdf::NamedNode>;

    /// The set of 'initial prefixes' for this host language.
    ///
    /// These are used to resolve CURIEs in RDFa.
    fn initial_prefixes(&self) -> &'static PrefixMapping;

    /// The default language for this host.
    ///
    /// This is an extension to RDFa.
    fn default_language(&self) -> Option<LanguageTag<String>>;

    /// The default vocabulary for this host.
    fn default_vocabulary(&self) -> Option<NamedNode>;

    fn parse_document<'d>(
        &self,
        doc: &'d str,
        doc_iri: Iri<&str>,
        processor_graph: &mut impl ProcessorGraph,
    ) -> Result<Self::DocumentType<'d>, Self::ParseError>;

    /// Inspect the parsed document and figure out if it
    /// has an embedded base specified.
    ///
    /// For example, this reads the `<base>` element in the
    /// HTML host language.
    fn establish_base<'d>(
        &self,
        doc: &'d Self::DocumentType<'_>,
    ) -> Result<Option<Iri<&'d str>>, Error>;
}

/// A document parsed by an RDFa host language.
pub trait Document {
    type ElementType<'d>: Element
    where
        Self: 'd;

    /// The root of the document: where processing will begin.
    fn root_element(&self) -> Option<Self::ElementType<'_>>;
}

/// An element in a document parsed by an RDFa host language.
pub trait Element: Debug {
    /// The (local) tag name of this element.
    fn tag_name(&self) -> &str;
    /// Retrieves the value of the given attribute, with no namespace.
    fn attr(&self, name: &str) -> Option<&str>;
    /// Retrieves the value of the given attribute, in the XML namespace.
    fn xml_attr(&self, name: &str) -> Option<&str>;
    /// Retrieves a list of namespaces (prefix, URI) defined on this element.
    fn namespaces(&self) -> impl Iterator<Item = (&str, &str)>;

    /// The text (content) value of the element.
    fn text(&self) -> String;
    /// The 'inner HTML' (literal source) for the element.
    fn inner_html(&self) -> String;
    /// The 'inner XML' (exclusively-canonicalized XML) for the element.
    fn inner_xml(&self, active_mappings: &PrefixMapping) -> String;

    /// Does the element have (element) children?
    fn has_children(&self) -> bool;
    /// Iterate the direct child elements.
    fn child_elements(&self) -> impl DoubleEndedIterator<Item = Self>;
}
