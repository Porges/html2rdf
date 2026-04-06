#![forbid(unsafe_code)]
#![warn(clippy::empty_docs)]

//! `html2rdf` provides an implementation of RDFa processing for Rust.
//!
//! It supports full [RDFa Core 1.1][rdfa-core], as well as
//! [XHTML+RDFa 1.1][xhtml-rdfa] and [HTML+RDFa 1.1][html-rdfa].
//!
//! Core functionality is well-tested but there might be some edge-cases
//! which are not yet properly handled, especialyl in the X/HTML-specific
//! extensions.
//!
//! [rdfa-core]: https://www.w3.org/TR/rdfa-core/
//! [xhtml-rdfa]: https://www.w3.org/TR/xhtml-rdfa/
//! [html-rdfa]: https://www.w3.org/TR/html-rdfa/
//!
//! ## Features
//! The following features are available, all enabled by default:
//! - `html`: enables HTML5 processing (via `scraper`)
//! - `xhtml`: enables XHTML processing (via `uppsala`)
//! - `vocab-online`: enables the use of [`algorithms::OnlineVocabularyProcessor`] (this also enables `html`)
//!
//! ## Known Issues
//! - `XMLLiteral` values are not yet correctly canonicalized in HTML5

use oxiri::Iri;
use oxrdf::Graph;

use crate::{
    graphs::ProcessorGraph, host_language::HostLanguage, rdfa::algorithms::VocabularyResolver,
};

pub mod errors;
pub mod graphs;
pub mod host_language;
pub mod rdfa;
pub mod vocab;

/// Specifies options for use in RDFa processing.
pub struct Options<H> {
    perform_property_copying: bool,
    vocab_resolver: Option<Box<dyn VocabularyResolver>>,
    host_language: H,
}

impl<H: Default> Default for Options<H> {
    fn default() -> Self {
        Self::new(H::default())
    }
}

impl<H> Options<H> {
    /// Creates a new [`Options`] for the given host language,
    /// with default settings.
    pub fn new(host_language: H) -> Self {
        Self {
            host_language,
            perform_property_copying: true,
            vocab_resolver: None,
        }
    }

    /// Enables the RDFA vocabulary expansion feature.
    ///
    /// Note that if using [`OnlineVocabularyResolver`],
    /// this may reach out to untrusted HTTP(S) endpoints.
    #[must_use]
    pub fn enable_vocabulary_expansion(
        mut self,
        resolver: impl VocabularyResolver + 'static,
    ) -> Self {
        self.vocab_resolver = Some(Box::new(resolver));
        self
    }
}

/// Processes the given input and produces graphs containing triples.
///
/// The input is processed according to the [`HostLanguage`]
/// provided on the `options`.
///
/// The `base` IRI provided is used as the base of the document
/// (note that this can be overridden within the document itself).
///
/// # Errors
/// This function only returns an error if the host language encounters one
/// during parsing – all other errors or warnings produced are
/// generated as triples into the processor graph.
pub fn doc_to_graphs<H: HostLanguage, P: ProcessorGraph + Default>(
    input: &str,
    base: Iri<&str>,
    options: Options<H>,
) -> Result<(Graph, P), H::ParseError> {
    let mut output = Graph::default();
    let mut processor = P::default();
    doc_into_graphs(input, base, options, &mut output, &mut processor)?;
    Ok((output, processor))
}

/// Processes the given input and produces triples in the given graphs.
///
/// The input is processed according to the [`HostLanguage`]
/// provided on the `options`.
///
/// The `base` IRI provided is used as the base of the document
/// (note that this can be overridden within the document itself).
///
/// Note that if property-copying or vocabulary-expansion is enabled,
/// they will be performed on the complete output graph, including
/// any triples that were already present.
///
/// # Errors
/// This function only returns an error if the host language encounters one
/// during parsing – all other errors or warnings produced are
/// generated as triples into the processor graph.
pub fn doc_into_graphs<H: HostLanguage>(
    input: &str,
    base: Iri<&str>,
    mut options: Options<H>,
    output_graph: &mut Graph,
    processor_graph: &mut impl ProcessorGraph,
) -> Result<(), H::ParseError> {
    let prop_copy = options.perform_property_copying;
    let resolver = std::mem::take(&mut options.vocab_resolver);
    rdfa::parse(input, base, options, output_graph, processor_graph)?;
    if prop_copy {
        rdfa::algorithms::property_copying(output_graph);
    }
    if let Some(resolver) = resolver {
        rdfa::algorithms::vocabulary_expansion(output_graph, base, resolver.as_ref());
    }
    Ok(())
}
