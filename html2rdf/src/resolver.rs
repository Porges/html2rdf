use std::{collections::BTreeMap, rc::Rc};

use curie::{Curie, ExpansionError, PrefixMapping};
use itertools::{Either, Itertools};
use oxilangtag::LanguageTag;
use oxiri::Iri;
use oxrdf::{BlankNode, NamedNode, NamedNodeRef, NamedOrBlankNode};

use tracing::trace;

use crate::{
    OutputGraph,
    errors::{CurieError, CurieOrIriError, InvalidTerm, IriError, TermOrIriError},
    graphs::{PGClass, ProcessorGraph},
    host_language::HostLanguage,
    vocab::rdfa,
};

/// Shared data which does not change throughout parsing.
pub struct ResolverConstantData {
    // “The base. This will usually be the IRI of the document being processed,
    //  but it could be some other IRI, set by some other mechanism, such as the (X)HTML base element.
    //  The important thing is that it establishes an IRI against which relative paths can be resolved.
    pub base: Iri<String>,
    /// This is used to represent [_:], which is allowed in RDFa
    /// but not in Turtle.
    empty_bnode: BlankNode,
    host_prefix_mappings: &'static PrefixMapping,
    host_default_vocab: Option<NamedNode>,
    // “The term mappings, a list of terms and their associated IRIs.
    //  This specification does not define an initial list. Host Languages MAY define an initial list.
    // “The local term mappings, a list of terms and their associated IRIs.
    // Note: there's no way to update term mappings inside a document.
    host_term_mappings: &'static BTreeMap<String, NamedNode>,
}

/// The [`Resolver`] stores all the information needed to resolve Terms, CURIEs, and relative IRIs.
#[derive(Clone)]
pub struct Resolver<'r> {
    constant_data: &'r ResolverConstantData,
    // “The default vocabulary, a value to use as the prefix IRI when a term unknown to the RDFa Processor
    //  is used. This specification does not define an initial setting for the default vocabulary.
    //  Host Languages MAY define an initial setting.
    pub default_vocab: Option<Rc<NamedNode>>,
    // “The language. Note that there is no default language.
    pub language: Option<Rc<LanguageTag<String>>>,
    // “A list of current, in-scope IRI mappings.
    pub iri_mappings: Rc<curie::PrefixMapping>,
}

impl ResolverConstantData {
    pub fn new(base: Iri<&str>, host: &impl HostLanguage) -> Self {
        Self {
            host_term_mappings: host.initial_terms(),
            host_default_vocab: host.default_vocabulary(),
            host_prefix_mappings: host.initial_prefixes(),
            // generate a random blank node for the empty blank node value
            empty_bnode: BlankNode::default(),
            // resolve the base to remove any fragments
            // so that we can use it directly as the "empty CURIE" value
            base: base.resolve("").unwrap(),
        }
    }
}

impl<'r> Resolver<'r> {
    pub fn new(constant_data: &'r ResolverConstantData) -> Self {
        Resolver {
            constant_data,
            // “A local default vocabulary, an IRI to use as a prefix mapping when a term is used.
            default_vocab: None,
            // “An initially empty language value.
            language: None,
            // “An initially empty list of IRI mappings, called the local list of IRI mappings.
            iri_mappings: Rc::new(PrefixMapping::default()),
        }
    }

    fn reset_default_vocab(&mut self) {
        self.default_vocab = self.constant_data.host_default_vocab.clone().map(Rc::new);
    }

    pub fn set_default_vocab(&mut self, vocab: Option<Rc<NamedNode>>) -> Option<Rc<NamedNode>> {
        std::mem::replace(&mut self.default_vocab, vocab)
    }

    pub fn update_vocab(
        &mut self,
        vocab: &str,
        processor_graph: &mut impl ProcessorGraph,
        output_graph: &mut impl OutputGraph,
    ) {
        // > If the value is empty,
        if vocab.is_empty() {
            // then the local default vocabulary MUST be reset to the Host Language defined default (if any).
            trace!("@vocab is empty, resetting default vocabulary to host language default");
            self.reset_default_vocab();
        }
        // > If @vocab is present and contains a value,
        else {
            match self.resolve_relative_iri(vocab) {
                Ok(vocab) => {
                    // > the local default vocabulary is updated according to the
                    // > section on CURIE and IRI Processing.
                    trace!(vocabulary = %vocab, "default vocabulary set via @vocab");
                    // > The value of @vocab is used to generate a triple as follows:
                    output_graph.emit(
                        // >   subject = base
                        NamedNodeRef::from(self.constant_data.base.as_ref()).into(),
                        // >   predicate = http://www.w3.org/ns/rdfa#usesVocabulary
                        rdfa::USES_VOCABULARY,
                        // >   object = value from @vocab
                        vocab.as_ref().into(),
                    );

                    self.default_vocab = Some(Rc::new(vocab));
                }
                Err(e) => {
                    processor_graph.emit_message(
                        PGClass::Warning,
                        &format!("Invalid IRI in @vocab: {e}"),
                        None, // TODO: context
                    );
                }
            }
        }
    }

    pub fn update_iri_mappings(
        &mut self,
        xmlns_prefixes: Vec<(&str, &str)>,
        prefixes: Vec<(&str, &str)>,
        processor_graph: &mut impl ProcessorGraph,
    ) {
        // note that we do not ever set_default,
        // this would define a "no prefix" mapping
        // which is MUST NOT in RDFa

        let iri_mappings = Rc::make_mut(&mut self.iri_mappings);

        // add XMLNS (must be first)
        for (prefix, iri) in xmlns_prefixes {
            if let Err(curie::InvalidPrefixError::ReservedPrefix) =
                iri_mappings.add_prefix(prefix, iri)
            {
                processor_graph.emit_message(
                    PGClass::PrefixRedefinition,
                    "ignoring attempt to add prefix assignment via @xmlns for reserved prefix '_'",
                    None, // TODO: context
                );
            }
        }

        // add others (after XMLNS)
        for (prefix, iri) in prefixes {
            if let Err(curie::InvalidPrefixError::ReservedPrefix) =
                iri_mappings.add_prefix(prefix, iri)
            {
                processor_graph.emit_message(
                    PGClass::PrefixRedefinition,
                    "ignoring attempt to add prefix assignment via @prefix for reserved prefix '_'",
                    None, // TODO: context
                );
            }
        }
    }

    pub fn update_current_language(
        &mut self,
        lang: &str,
        processor_graph: &mut impl ProcessorGraph,
    ) {
        if lang.is_empty() {
            self.language = None;
        } else {
            match LanguageTag::parse_and_normalize(lang) {
                Ok(lang) => {
                    trace!(current_language = %lang, "current language set");
                    self.language = Some(Rc::new(lang));
                }
                Err(e) => {
                    processor_graph.emit_message(
                        PGClass::Warning,
                        &format!("Invalid language identifier ({lang}): {e}"),
                        None, // TODO: context
                    );
                }
            }
        }
    }
}
impl Resolver<'_> {
    /// Resolves an IRI against the [`Self::base`] value.
    fn resolve_relative_iri(&self, value: &str) -> Result<NamedNode, IriError> {
        let iri = self
            .constant_data
            .base
            .resolve(value)
            .map_err(|source| IriError {
                value: value.to_string(),
                source,
            })?;

        Ok(NamedNode::from(iri))
    }

    /// A reference to the base IRI.
    pub fn base(&self) -> Iri<&str> {
        self.constant_data.base.as_ref()
    }

    // When resolving a term, the outcome might be that it _must_ be ignored.
    // This is indicated by returning [`InvalidTerm`].
    fn resolve_term(&self, term: &str) -> Result<Option<NamedNode>, InvalidTerm> {
        // [rdfa-core] 7.5.3
        // > Some RDFa attributes have a datatype that permits a term to be referenced.
        // > RDFa defines the syntax of a term as:
        // >
        // > term     ::=  NCNameStartChar termChar*
        // > termChar ::=  ( NameChar - ':' ) | '/'
        // >
        // > > [!Note]
        // > > For the avoidance of doubt, this production means a 'term' in RDFa is an
        // > > XML NCName that also permits slash as a non-leading character.
        if !term.is_empty()
            && !term.starts_with('/')
            && term
                .split('/')
                .all(|s| rxml_validation::validate_ncname(s).is_ok())
        {
            // > When an RDFa attribute permits the use of a term,
            // > and the value being evaluated matches the production for term above,
            // > it is transformed to an IRI using the following logic:
            //
            // > If there is a local default vocabulary the IRI is obtained
            // > by concatenating that value and the term.
            if let Some(vocab) = &self.default_vocab {
                let mut iri = vocab.as_str().to_string();
                iri.push_str(term);
                let named_node = NamedNode::new(iri).expect("always a valid IRI");
                Ok(Some(named_node))
            }
            // > Otherwise, check if the term matches an item in the list of local term mappings.
            //
            // > First compare against the list case-sensitively,
            else if let Some(term_iri) = self.constant_data.host_term_mappings.get(term) {
                Ok(Some(term_iri.clone()))
            }
            // > and if there is no match then compare case-insensitively.
            else if let Some(term) = self
                .constant_data
                .host_term_mappings
                .iter()
                .find_map(|(key, iri)| key.eq_ignore_ascii_case(term).then(|| iri.clone()))
            {
                // TODO: what does “case-insensitively” mean?
                // > If there is a match, use the associated IRI.
                Ok(Some(term))
            } else {
                // > Otherwise, the term has no associated IRI and MUST be ignored.
                Err(InvalidTerm)
            }
        } else {
            // not a term
            Ok(None)
        }
    }

    /// Resolves a (non-safe) CURIE to an IRI or bnode.
    fn resolve_curie(&self, value: &str) -> Result<NamedOrBlankNode, CurieError> {
        // This part is confusing—
        //
        // [rdfa-core] 7.4.2
        // > An empty attribute value (e.g., typeof='') is still a CURIE,
        // > and is processed as such. The rules for this processing are defined in Sequence.
        // > Specifically, however, an empty attribute value is never treated as a relative
        // > IRI by this specification.
        //
        // As far as I can tell the "rules for this processing" do not appear anywhere, and
        // furthermore the following seems to contradict it...
        //
        // [rdfa-core] 7.4
        // > A consequence of this is that when the value of an attribute of this datatype
        // [... SafeCURIEorCURIEorIRI ..]
        // > is the empty string (e.g., @about=""), that value resolves to an IRI.
        // > An IRI of "" is a relative IRI that is interpreted as being the same as the base.
        // > In other words, a value of "" will usually resolve to the IRI of the current document.
        //
        // Here I determine that: a CURIE is never empty (despite the CURIE spec allowing it)
        // because an empty CURIE value needs to be resolved against the 'mapping to use when there
        // is no prefix', which is explicitly stated as being not defined in RDFa. So, we treat empty CURIEs
        // as not-valid. Then, empty values will be resolved against the base IRI as stated above.
        //
        // (The 'mapping to use with the default prefix' _is_ defined, but that requires the value ":"
        // with an empty prefix and an empty reference.)
        if value.is_empty() {
            return Err(CurieError::EmptyCurie);
        }

        let curie = if let Some((prefix, suffix)) = value.split_once(':') {
            if prefix == "_" {
                if suffix.is_empty() {
                    // Note:
                    // [_:] is permitted by RDFa but it is not allowed by oxrdf
                    // since the Turtle syntax does not permit it. So, we turn
                    // all references to [_:] into a single (randomized) blank node.
                    return Ok(self.constant_data.empty_bnode.clone().into());
                }

                return Ok(BlankNode::new(suffix)
                    .map_err(|_| CurieError::InvalidBlankNodeSuffix {
                        value: value.to_string(),
                        suffix: suffix.to_string(),
                    })?
                    .into());
            }

            Curie::new(Some(prefix), suffix)
        } else {
            Curie::new(None, value)
        };

        let mut expanded = self.iri_mappings.expand_curie(&curie);
        if let Err(ExpansionError::Invalid) = &expanded {
            // TODO: this should really be moved into the active mappings
            // to ensure it is exported to XML (behaviour is preserved)
            expanded = self.constant_data.host_prefix_mappings.expand_curie(&curie);
        }

        match expanded {
            Ok(iri) => {
                // See note just before [rdfa-core] 7.4.1:
                // Usually this should be an absolute IRI, but it is
                // _possible_ (though not recommended) that a relative IRI
                // can be used as a prefix, so we will resolve it in case.
                match self.resolve_relative_iri(&iri) {
                    Ok(absolute_iri) => Ok(absolute_iri.into()),
                    // expansion resulted in an invalid IRI
                    Err(source) => Err(CurieError::InvalidIRI {
                        source,
                        value: value.to_string(),
                    }),
                }
            }
            Err(source) => {
                //warn!(error = ?source, %curie, "error expanding CURIE");
                Err(CurieError::ExpansionError {
                    source,
                    value: value.to_string(),
                })
            }
        }
    }

    /// Resolves a SafeCURIE or CURIE to an IRI or bnode.
    ///
    /// Returns a parsed IRI or None if it is not a CURIE (might be a raw IRI).
    /// In the case of a malformed CURIE, returns an error.
    fn resolve_safecurie_or_curie(
        &self,
        value: &str,
    ) -> Result<Option<NamedOrBlankNode>, CurieError> {
        // 7.4 CURIE and IRI Processing
        //
        // > SafeCURIEorCURIEorIRI
        //
        // > When the value is surrounded by square brackets,
        if value.starts_with('[') && value.ends_with(']') {
            // > then the content within the brackets is evaluated
            // > as a CURIE according to the CURIE Syntax Definition.
            match self.resolve_curie(&value[1..value.len() - 1]) {
                Ok(iri) => Ok(Some(iri)),
                Err(err) => Err(err),
            }
        } else {
            // > Otherwise, the value is evaluated as a CURIE.
            match self.resolve_curie(value) {
                // > If it is a valid CURIE, the resulting IRI is used;
                Ok(value) => Ok(Some(value)),
                // > otherwise, the value is processed as an IRI.
                Err(_) => Ok(None),
            }
        }
    }

    /// Resolves an IRI-only attribute value.
    pub fn resolve_attribute_iri(&self, value: &str) -> Option<NamedNode> {
        if value.is_empty() {
            // > Specifically, however, an empty attribute value is never treated
            // > as a relative IRI by this specification.
            return None;
        }

        self.resolve_relative_iri(value).ok()
    }

    /// Resolves an absolute IRI.
    pub fn resolve_absolute_iri(value: &str) -> Result<NamedNode, IriError> {
        NamedNode::new(value.to_string()).map_err(|source| IriError {
            value: value.to_string(),
            source,
        })
    }

    /// Resolves a `SafeCURIEorCURIEorIRI`.
    pub fn safecurie_or_curie_or_iri(
        &self,
        value: &str,
    ) -> Result<NamedOrBlankNode, CurieOrIriError> {
        match self.resolve_safecurie_or_curie(value) {
            Ok(Some(val)) => Ok(val),
            Ok(None) => {
                // not a CURIE, will try IRI
                match self.resolve_relative_iri(value) {
                    Ok(val) => Ok(val.into()),
                    Err(err) => Err(CurieOrIriError::Iri(err)),
                }
            }
            Err(err) => Err(CurieOrIriError::Curie(err)),
        }
    }

    fn curie_or_absiri(&self, value: &str) -> Result<NamedOrBlankNode, IriError> {
        match self.resolve_curie(value) {
            Ok(val) => Ok(val),
            // TODO: throwing away error?
            Err(_) => match Self::resolve_absolute_iri(value) {
                Ok(iri) => Ok(NamedOrBlankNode::from(iri)),
                Err(iri_err) => Err(iri_err),
            },
        }
    }

    pub fn term_or_curie_or_absiri(&self, value: &str) -> Result<NamedOrBlankNode, TermOrIriError> {
        match self.resolve_term(value) {
            // A valid term
            Ok(Some(result)) => Ok(NamedOrBlankNode::from(result)),
            // An invalid Term - report error
            Err(err) => Err(TermOrIriError::Term(err)),
            // Not a Term - try CURIE/IRI
            Ok(None) => self.curie_or_absiri(value).map_err(TermOrIriError::Iri),
        }
    }

    pub fn many_curie_or_absiri(&self, value: &str) -> (Vec<NamedOrBlankNode>, Vec<IriError>) {
        value
            .split_ascii_whitespace()
            .partition_map(|v| match self.curie_or_absiri(v) {
                Ok(v) => Either::Left(v),
                Err(e) => Either::Right(e),
            })
    }

    pub fn many_term_or_curie_or_absiri(
        &self,
        value: &str,
    ) -> (Vec<NamedOrBlankNode>, Vec<TermOrIriError>) {
        value
            .split_ascii_whitespace()
            .partition_map(|p| match self.term_or_curie_or_absiri(p) {
                Ok(v) => Either::Left(v),
                Err(e) => Either::Right(e),
            })
    }
}
