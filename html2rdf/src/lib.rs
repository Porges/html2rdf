use std::{
    borrow::Cow, cell::RefCell, collections::BTreeMap, marker::PhantomData, rc::Rc, str::FromStr,
};

use curie::{Curie, ExpansionError, PrefixMapping};
use icu_locale::LanguageIdentifier;
use itertools::Itertools;
use mitsein::{iter1::FromIterator1, prelude::Vec1};
use oxiri::{Iri, IriParseError};
use oxrdf::{
    Graph, NamedNode, NamedNodeRef, NamedOrBlankNode, TermRef, TripleRef,
    vocab::{self, rdf},
};
use scraper::{ElementRef, Html, node::Element};
use tracing::trace;

pub mod vocabs;

pub fn process(
    input: &str,
    base: Iri<String>,
    output_graph: &mut Graph,
    processor_graph: &mut Graph,
) -> Result<(), Error> {
    parse(input, base, output_graph, processor_graph)?;
    property_copying(output_graph);

    Ok(())
}

fn property_copying(graph: &mut Graph) {
    // Step 1: add
    {
        let mut added_any = true;
        let mut new_triples = Vec::new();
        while added_any {
            new_triples.clear();
            added_any = false;

            for copy_triple in graph.triples_for_predicate(vocabs::rdfa::COPY) {
                let copy_target: oxrdf::NamedOrBlankNodeRef = match copy_triple.object {
                    TermRef::NamedNode(n) => n.into(),
                    TermRef::BlankNode(n) => n.into(),
                    TermRef::Literal(_) => {
                        continue; // TODO: warning?
                    }
                };

                if !graph.contains(TripleRef::new(
                    copy_target,
                    rdf::TYPE,
                    vocabs::rdfa::PATTERN,
                )) {
                    continue; // TODO: warning?
                }

                for trip in graph.triples_for_subject(copy_target) {
                    new_triples.push(
                        TripleRef::new(copy_triple.subject, trip.predicate, trip.object)
                            .into_owned(),
                    );
                }
            }

            for triple in &new_triples {
                added_any |= graph.insert(triple);
            }
        }
    }

    // Step 2: remove
    {
        let mut triples_to_remove = Vec::new();
        for copy_triple in graph.triples_for_predicate(vocabs::rdfa::COPY) {
            triples_to_remove.push(copy_triple.into_owned());
            let copy_target: oxrdf::NamedOrBlankNodeRef = match copy_triple.object {
                TermRef::NamedNode(n) => n.into(),
                TermRef::BlankNode(n) => n.into(),
                TermRef::Literal(_) => continue,
            };

            if !graph.contains(TripleRef::new(
                copy_target,
                rdf::TYPE,
                vocabs::rdfa::PATTERN,
            )) {
                continue;
            }

            triples_to_remove.push(
                TripleRef::new(copy_triple.subject, rdf::TYPE, vocabs::rdfa::PATTERN).into_owned(),
            );

            for trip in graph.triples_for_subject(copy_target) {
                triples_to_remove.push(trip.into_owned());
            }
        }

        for triple in triples_to_remove {
            graph.remove(&triple);
        }
    }
}

pub fn parse(
    input: &str,
    mut base: Iri<String>,
    output_graph: &mut Graph,
    processor_graph: &mut Graph,
) -> Result<(), Error> {
    let o_graph = OutputGraph(output_graph);
    let p_graph = ProcessorGraph(processor_graph);

    let get_err = || -> Result<(), Error> {
        let doc = Html::parse_document(input);
        if !doc.errors.is_empty() {
            for err in doc.errors.iter() {
                println!("Error: {}", err);
            }
        }
        let mut proc = RDFaProcessor::new(o_graph, p_graph);

        let base_sel = scraper::selector::Selector::parse("html>head>base").unwrap();

        if let Some(base_el) = doc.select(&base_sel).next()
            && let Some(base_href) = base_el.attr("href")
        {
            base = Iri::parse(base_href.to_string()).map_err(|source| Error::IriParseError {
                source,
                iri: base_href.to_string(),
            })?;

            trace!(%base, "<base> found");
        }

        let eval_context = EvaluationContext::new(base);
        proc.run(eval_context, doc)?;
        Ok(())
    }();

    if let Err(e) = get_err {
        ProcessorGraph(processor_graph).emit(PGType::DocumentError, &e.to_string());
    }

    Ok(())
}

type SharedList = RefCell<Vec<Rc<oxrdf::Term>>>;

#[derive(Default, Clone)]
struct ListMapping {
    lists: BTreeMap<oxrdf::NamedNode, Rc<SharedList>>,
}

enum Attr<T> {
    Missing,
    Empty,
    Value(T),
}

impl<T> Attr<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> Attr<U> {
        match self {
            Attr::Missing => Attr::Missing,
            Attr::Empty => Attr::Empty,
            Attr::Value(v) => Attr::Value(f(v)),
        }
    }

    pub fn is_present(&self) -> bool {
        !matches!(self, Attr::Missing)
    }

    pub fn as_value(&self) -> Option<&T> {
        match self {
            Attr::Value(v) => Some(v),
            Attr::Missing | Attr::Empty => None,
        }
    }

    pub fn into_value(self) -> Option<T> {
        match self {
            Attr::Value(v) => Some(v),
            Attr::Missing | Attr::Empty => None,
        }
    }
}

impl ListMapping {
    pub fn ensure_list(&mut self, predicate: &oxrdf::NamedNode) -> Rc<SharedList> {
        if let Some(list) = self.lists.get(predicate) {
            return list.clone();
        }

        trace!(%predicate, "created new list for predicate");
        let shared_list: Rc<SharedList> = Default::default();
        let replaced = self.lists.insert(predicate.clone(), shared_list.clone());
        debug_assert!(replaced.is_none());
        shared_list
    }

    pub fn insert_value(&mut self, predicate: oxrdf::NamedNode, term: Rc<oxrdf::Term>) {
        trace!(%predicate, %term, "inserting term into list");
        let list = match self.lists.get(&predicate) {
            Some(list) => list,
            None => {
                trace!(%predicate, "created new list for predicate");
                self.lists.entry(predicate).or_default()
            }
        };

        list.borrow_mut().push(term);
    }
}

// “During processing, each rule is applied using information provided by an evaluation context.
//  An initial context is created when processing begins. That context has the following members:
struct EvaluationContext<H> {
    resolver: ResolverData<H>,

    // “The parent subject. The initial value will be the same as the initial value of base,
    //  but it will usually change during the course of processing.
    parent_subject: Rc<oxrdf::NamedOrBlankNode>,

    // “The parent object. In some situations the object of a statement becomes the subject
    //  of any nested statements, and this member is used to convey this value. Note that this
    //  value may be a bnode, since in some situations a number of nested statements are grouped
    //  together on one bnode. This means that the bnode must be set in the containing statement and passed down.
    parent_object: Option<Rc<oxrdf::NamedOrBlankNode>>,

    // “A list of incomplete triples. A triple can be incomplete when no object resource is provided
    //  alongside a predicate that requires a resource (i.e., @rel or @rev). The triples can be completed
    //  when a resource becomes available, which will be when the next subject is specified
    //  (part of the process called chaining).
    incomplete_triples: Vec<IncompleteTriple>,

    // “A list mapping that associates IRIs with lists.
    list_mapping: Rc<RefCell<ListMapping>>,
}

impl<H> Clone for EvaluationContext<H> {
    fn clone(&self) -> Self {
        Self {
            resolver: self.resolver.clone(),
            parent_subject: self.parent_subject.clone(),
            parent_object: self.parent_object.clone(),
            incomplete_triples: self.incomplete_triples.clone(),
            list_mapping: self.list_mapping.clone(),
        }
    }
}

struct ResolverData<H> {
    // “The base. This will usually be the IRI of the document being processed,
    //  but it could be some other IRI, set by some other mechanism, such as the (X)HTML base element.
    //  The important thing is that it establishes an IRI against which relative paths can be resolved.
    base: Rc<Iri<String>>,
    /// This is used to represent [_:], which is allowed in RDFa
    /// but not in Turtle.
    empty_bnode: Rc<oxrdf::BlankNode>,
    // “The default vocabulary, a value to use as the prefix IRI when a term unknown to the RDFa Processor
    //  is used. This specification does not define an initial setting for the default vocabulary.
    //  Host Languages MAY define an initial setting.
    default_vocab: Option<oxrdf::NamedNode>,
    // “The term mappings, a list of terms and their associated IRIs.
    //  This specification does not define an initial list. Host Languages MAY define an initial list.
    term_mappings: Rc<BTreeMap<String, oxrdf::NamedNode>>,
    // “The language. Note that there is no default language.
    language: Option<Rc<LanguageIdentifier>>,
    // “A list of current, in-scope IRI mappings.
    iri_mappings: Rc<curie::PrefixMapping>,

    _phantom: PhantomData<H>,
}

impl<H> Clone for ResolverData<H> {
    fn clone(&self) -> Self {
        Self {
            base: self.base.clone(),
            empty_bnode: self.empty_bnode.clone(),
            default_vocab: self.default_vocab.clone(),
            term_mappings: self.term_mappings.clone(),
            language: self.language.clone(),
            iri_mappings: self.iri_mappings.clone(),
            _phantom: self._phantom.clone(),
        }
    }
}

impl<H: HostLanguage> ResolverData<H> {
    fn new(base: Iri<String>) -> Self {
        let mut iri_mappings = PrefixMapping::default();
        for (prefix, iri) in initial_context_prefixes().mappings() {
            iri_mappings.add_prefix(prefix, iri).unwrap()
        }
        let term_mappings = initial_context_terms().clone();

        ResolverData {
            // “A local default vocabulary, an IRI to use as a prefix mapping when a term is used.
            default_vocab: None,
            // “An initially empty language value.
            language: None,
            // “An initially empty list of IRI mappings, called the local list of IRI mappings.
            iri_mappings: Rc::new(iri_mappings),
            // “The local term mappings, a list of terms and their associated IRIs.
            term_mappings: Rc::new(term_mappings),
            // generate a random blank node for the empty blank node value
            empty_bnode: Rc::new(oxrdf::BlankNode::default()),
            // resolve the base to remove any fragments
            // so that we can use it directly as the "empty CURIE" value
            base: Rc::new(base.resolve("").unwrap()),
            _phantom: Default::default(),
        }
    }

    fn for_element<'e, 'rp, 'p>(
        mut self,
        el: &'e Element,
        processor_graph: &'rp RefCell<ProcessorGraph<'p>>,
        output_graph: &mut OutputGraph,
    ) -> Result<Resolver<'e, 'rp, 'p, H>, Error> {
        // [rdfa-core] 7.5: 2.
        self.update_vocab(el, output_graph);
        // [rdfa-core] 7.5: 3.
        self.update_iri_mappings(el)?;
        // [rdfa-core] 7.5: 4.
        self.update_current_language(el, processor_graph);

        Ok(Resolver {
            el,
            processor_graph,
            data: self,
        })
    }

    /// Resolves an IRI against the [`Self::base`] value.
    /// Note that unlike CURIEs, an empty IRI value is ignored.
    fn resolve_relative_iri(&self, value: &str) -> Result<NamedNode, IriParseError> {
        // [rdfa-core] 7.4.2 [NOTE]
        // > Specifically, however, an empty attribute value is _never_ treated
        // > as a relative IRI by this specification.
        // if value.is_empty() {
        //     return Ok(None);
        // }

        let iri = self.base.resolve(value)?;
        debug_assert!(Iri::parse(iri.as_str()).is_ok());
        Ok(oxrdf::NamedNode::new_unchecked(iri.into_inner()))
    }

    /// An empty CURIE resolves to the [`Self::base`] value.
    fn empty_curie(&self) -> oxrdf::NamedNodeRef<'_> {
        oxrdf::NamedNodeRef::new_unchecked(self.base.as_str())
    }

    fn update_vocab(&mut self, el: &Element, output_graph: &mut OutputGraph) {
        // “the local default vocabulary is set to the default vocabulary from the evaluation context.
        // (do nothing)

        // [rdfa-core] 7.5: 2.
        // > Next the current element is examined for any change to the default vocabulary via @vocab.
        if let Some(vocab) = el.attr("vocab") {
            // > If the value is empty,
            if vocab.is_empty() {
                // then the local default vocabulary MUST be reset to the Host Language defined default (if any).
                trace!("@vocab is empty, resetting default vocabulary to host language default");
                self.default_vocab = H::default_vocabulary();
            }
            // > If @vocab is present and contains a value,
            else if let Ok(vocab) = self.resolve_relative_iri(vocab) {
                // > the local default vocabulary is updated according to the
                // > section on CURIE and IRI Processing.
                trace!(vocabulary = %vocab, "default vocabulary set via @vocab");
                // > The value of @vocab is used to generate a triple as follows:
                output_graph.emit(TripleRef::new(
                    // >   subject = base
                    oxrdf::NamedNodeRef::new_unchecked(&self.base),
                    // >   predicate = http://www.w3.org/ns/rdfa#usesVocabulary
                    vocabs::rdfa::USES_VOCABULARY,
                    // >   object = value from @vocab
                    &vocab,
                ));

                self.default_vocab = Some(vocab);
            }
        }
    }

    fn update_iri_mappings(&mut self, el: &Element) -> Result<(), Error> {
        // 3.
        // “Next, the current element is examined for IRI mappings and these are added to the local list of IRI mappings.
        //  Note that an IRI mapping will simply overwrite any current mapping in the list that has the same name;
        //
        // [HTML-RDFA]
        // “Extracting URI Mappings declared via @xmlns: while operating from within a DOM Level 2 based RDFa processor
        //  can be achieved using the following algorithm:
        // “While processing each DOM2 [Element] as described in [rdfa-core], Section 7.5: Sequence, Step #2:
        // “1. For each [Attr] in the [Node.attributes] list that has a [namespace prefix] value of @xmlns,
        //     create an [IRI mapping] by storing the [local name] as the value to be mapped, and the
        //     [Node.nodeValue] as the value to map.
        // (Note: this is not done because html5ever/scraper never reports namespace prefixes…)
        // “2. For each [Attr] in the [Node.attributes] list that has a [namespace prefix] value of null
        //     and a [local name] that starts with @xmlns:, create an [IRI mapping] by storing the [local name]
        //     part with the @xmlns: characters removed as the value to be mapped, and the [Node.nodeValue] as
        //     the value to map.
        // (Note: this is what is implemented below…)
        let xmlns_prefixes = el
            .attrs
            .iter()
            .filter_map(|(qn, value)| -> Option<_> {
                if qn.prefix.is_none() {
                    let prefix = qn.local.strip_prefix("xmlns:")?;
                    Some((prefix, value))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        let prefixes = el
            .attr("prefix")
            .map(|x| {
                x.split_ascii_whitespace()
                    .tuples()
                    .map(|(prefix, value)| {
                        if let Some(prefix) = prefix.strip_suffix(':') {
                            Ok((prefix, value))
                        } else {
                            Err(Error::NoColonPrefix)
                        }
                    })
                    .collect::<Result<Vec<_>, Error>>()
            })
            .transpose()?
            .unwrap_or_default();

        if !xmlns_prefixes.is_empty() || !prefixes.is_empty() {
            // note that we do not ever set_default,
            // this would define a "no prefix" mapping
            // which is MUST NOT in RDFa

            let mut iri_mappings = Rc::unwrap_or_clone(std::mem::take(&mut self.iri_mappings));

            // add XMLNS (must be first)
            for (prefix, iri) in xmlns_prefixes {
                iri_mappings.add_prefix(prefix, iri).expect("TODO");
            }

            // add others (after XMLNS)
            for (prefix, iri) in prefixes {
                iri_mappings.add_prefix(prefix, iri).expect("TODO");
            }

            self.iri_mappings = Rc::new(iri_mappings);
        }

        Ok(())
    }

    fn update_current_language<'rp, 'p>(
        &mut self,
        el: &Element,
        processor_graph: &'rp RefCell<ProcessorGraph<'p>>,
    ) {
        // “the current language value is set to the language value from the evaluation context.
        // (do nothing)

        // 4. Language
        // “The current element is also parsed for any language information,
        //  and if present, current language is set accordingly;
        //
        // [HTML-RDFA] 3.1
        // “The current language can be set using either the @lang
        //  or @xml:lang attributes. When the @lang attribute and
        //  the @xml:lang attribute are specified on the same element,
        //  the @xml:lang attribute takes precedence. When both @lang
        //  and @xml:lang are specified on the same element, they MUST
        //  have the same value. Further details related to setting the
        //  current language can be found in section 3.3 Specifying the
        //  Language for a Literal.
        if let Some(lang) = el.attr("xml:lang").or(el.attr("lang")) {
            if lang.is_empty() {
                self.language = None;
            } else {
                match LanguageIdentifier::from_str(lang) {
                    Ok(lang) => {
                        trace!(current_language = %lang, "current language set");
                        self.language = Some(Rc::new(lang));
                    }
                    Err(e) => {
                        processor_graph.borrow_mut().emit(
                            PGType::Warning,
                            &format!("Invalid language identifier ({lang}): {e}"),
                        );
                    }
                }
            }
        }
    }
}

// “During the course of processing a number of locally scoped values are needed, as follows:
struct Resolver<'e, 'rp, 'p, H> {
    data: ResolverData<H>,
    el: &'e Element,
    processor_graph: &'rp RefCell<ProcessorGraph<'p>>,
}

impl<'e, 'rp, 'p, H> Clone for Resolver<'e, 'rp, 'p, H> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            el: self.el,
            processor_graph: self.processor_graph,
        }
    }
}

impl<'e, 'rp, 'p: 'rp, H: HostLanguage> Resolver<'e, 'rp, 'p, H> {
    /// An empty CURIE resolves to the [`Self::base`] value.
    fn empty_curie(&self) -> oxrdf::NamedNodeRef<'_> {
        self.data.empty_curie()
    }

    // When resolving a term, the outcome might be that it _must_ be ignored.
    // This is indicated by returning [`None`].
    fn resolve_term(&self, term: &str) -> Result<Option<oxrdf::NamedNode>, NotTERM> {
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
            if let Some(vocab) = &self.data.default_vocab {
                let mut iri = vocab.as_str().to_string();
                iri.push_str(term);
                let named_node = oxrdf::NamedNode::new(iri).expect("always a valid IRI");
                Ok(Some(named_node))
            }
            // > Otherwise, check if the term matches an item in the list of local term mappings.
            //
            // > First compare against the list case-sensitively,
            else if let Some(term_iri) = self.data.term_mappings.get(term) {
                Ok(Some(term_iri.clone()))
            }
            // > and if there is no match then compare case-insensitively.
            else if let Some(term) = self
                .data
                .term_mappings
                .iter()
                .find_map(|(key, iri)| key.eq_ignore_ascii_case(term).then(|| iri.clone()))
            {
                // TODO: what does “case-insensitively” mean?
                // > If there is a match, use the associated IRI.
                Ok(Some(term))
            } else {
                // > Otherwise, the term has no associated IRI and MUST be ignored.
                Ok(None)
            }
        } else {
            // not a term
            Err(NotTERM)
        }
    }

    /// Resolves a (non-safe) CURIE to an IRI or bnode.
    fn resolve_curie(&self, value: &str) -> Result<NamedOrBlankNode, CurieError> {
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
                    return Ok(self.data.empty_bnode.as_ref().clone().into());
                }

                return Ok(oxrdf::BlankNode::new(suffix)
                    .map_err(|_| CurieError::InvalidBlankNodeSuffix(suffix.to_string()))?
                    .into());
            }

            Curie::new(Some(prefix), suffix)
        } else {
            Curie::new(None, value)
        };

        match self.data.iri_mappings.expand_curie(&curie) {
            Ok(iri) => {
                // See note just before [rdfa-core] 7.4.1:
                // Usually this should be an absolute IRI, but it is
                // _possible_ (though not recommended) that a relative IRI
                // can be used as a prefix, so we will resolve it in case.
                match self.data.resolve_relative_iri(&iri) {
                    Ok(absolute_iri) => Ok(absolute_iri.into()),
                    Err(_) => Err(CurieError::InvalidIRI(iri)),
                }
            }
            Err(err) => Err(CurieError::ExpansionError(err)),
        }
    }

    /// Resolves a SafeCURIE or CURIE to an IRI or bnode.
    fn resolve_safecurie_or_curie(
        &self,
        value: &str,
    ) -> Result<Option<NamedOrBlankNode>, NotCURIE> {
        if value.starts_with('[') && value.ends_with(']') {
            match self.resolve_curie(&value[1..value.len() - 1]) {
                Ok(iri) => Ok(Some(iri)),
                Err(err) => {
                    // if it's a SafeCURIE we MUST ignore it
                    // produce a warning if the prefix was not defined
                    match err {
                        CurieError::EmptyCurie
                        | CurieError::ExpansionError(ExpansionError::MissingDefault) => {}
                        CurieError::InvalidBlankNodeSuffix(suffix) => {
                            self.processor_graph.borrow_mut().emit(
                                PGType::UnresolvedCurie,
                                &format!(
                                    "Invalid CURIE: {value} (invalid blank node suffix: `{suffix}`)",
                                ),
                            );
                        }
                        CurieError::InvalidIRI(iri) => {
                            self.processor_graph.borrow_mut().emit(
                                PGType::UnresolvedCurie,
                                &format!(
                                    "Invalid CURIE: {value} (expanded to invalid IRI value <{iri}>)",
                                ),
                            );
                        }
                        CurieError::ExpansionError(ExpansionError::Invalid) => {
                            self.processor_graph.borrow_mut().emit(
                                PGType::UnresolvedCurie,
                                &format!("Invalid CURIE: {value} (no such prefix defined)"),
                            );
                        }
                    }
                    Ok(None)
                }
            }
        } else {
            match self.resolve_curie(value) {
                Ok(value) => Ok(Some(value)),
                Err(_) => Err(NotCURIE),
            }
        }
    }

    /// Resolves an IRI-only attribute value.
    fn attribute_iri(&self, value: &str) -> Option<NamedNode> {
        self.data.resolve_relative_iri(value).ok()
    }

    /// Resolves an absolute IRI.
    fn resolve_absolute_iri(&self, value: &str) -> Result<Option<NamedNode>, IriParseError> {
        Ok(Some(oxrdf::NamedNode::new(value.to_string())?))
    }

    fn safecurie_or_curie_or_iri(&self, value: &str) -> Option<NamedOrBlankNode> {
        match self.resolve_safecurie_or_curie(value) {
            Ok(val) => val, // Some value or None = MUST be ignored
            Err(NotCURIE) => {
                // not a CURIE, will try IRI
                match self.data.resolve_relative_iri(value) {
                    Ok(val) => Some(val.into()),
                    Err(err) => {
                        self.report_invalid_iri(err, value);
                        None
                    }
                }
            }
        }
    }

    fn curie_or_absiri(&self, value: &str) -> Option<NamedOrBlankNode> {
        match self.resolve_curie(value) {
            Ok(val) => Some(val),
            // MUST be ignored
            Err(_) => match self.resolve_absolute_iri(value) {
                Ok(iri) => iri.map(NamedOrBlankNode::from),
                Err(iri_err) => {
                    self.report_invalid_iri(iri_err, value);
                    None
                }
            },
        }
    }

    fn report_invalid_iri(&self, iri_err: IriParseError, value: &str) {
        self.processor_graph.borrow_mut().emit(
            PGType::Warning,
            &format!("Invalid IRI: <{}> ({})", value, iri_err),
        );
    }

    fn term_or_curie_or_absiri(&self, value: &str) -> Option<NamedOrBlankNode> {
        match self.resolve_term(value) {
            Ok(result) => result.map(NamedOrBlankNode::from), // value or MUST be ignored
            Err(NotTERM) => self.curie_or_absiri(value),
        }
    }

    fn many_curie_or_absiri(&self, value: &str) -> impl Iterator<Item = NamedOrBlankNode> {
        value
            .split_ascii_whitespace()
            .filter_map(|v| self.curie_or_absiri(v))
    }

    fn many_term_or_curie_or_absiri(&self, value: &str) -> impl Iterator<Item = NamedOrBlankNode> {
        value
            .split_ascii_whitespace()
            .filter_map(|v| self.term_or_curie_or_absiri(v))
    }

    fn attr_iri(&self, name: &str) -> Attr<NamedNode> {
        match self.el.attr(name) {
            None => Attr::Missing,
            Some(v) => match self.attribute_iri(v) {
                None => Attr::Empty,
                Some(v) => Attr::Value(v),
            },
        }
    }

    fn attr_1_term_or_curie_or_absiri(&self, name: &str) -> Attr<NamedNode> {
        match self.el.attr(name) {
            None => Attr::Missing,
            Some(v) => match self.term_or_curie_or_absiri(v) {
                None => Attr::Empty,
                // TODO: produce a warning when blanknodes are dropped
                Some(NamedOrBlankNode::BlankNode(_)) => Attr::Empty,
                Some(NamedOrBlankNode::NamedNode(v)) => Attr::Value(v),
            },
        }
    }

    fn attr_1_safecurie_or_curie_or_iri(&self, name: &str) -> Attr<NamedOrBlankNode> {
        match self.el.attr(name) {
            None => Attr::Missing,
            Some(v) => match self.safecurie_or_curie_or_iri(v) {
                None => Attr::Empty,
                Some(v) => Attr::Value(v),
            },
        }
    }

    fn attr_many_curie_or_absiri(&self, name: &str) -> Attr<Vec1<NamedNode>> {
        match self.el.attr(name) {
            None => Attr::Missing,
            Some(v) => {
                match Vec1::try_from_iter(self.many_curie_or_absiri(v).filter_map(|v| {
                    match v {
                        NamedOrBlankNode::NamedNode(named_node) => Some(named_node),
                        // TODO: produce a warning here when dropping blank nodes
                        NamedOrBlankNode::BlankNode(_) => None,
                    }
                })) {
                    Err(_) => Attr::Empty,
                    Ok(v) => Attr::Value(v),
                }
            }
        }
    }

    fn attr_many_term_or_curie_or_absiri(&self, name: &str) -> Attr<Vec1<NamedNode>> {
        match self.el.attr(name) {
            None => Attr::Missing,
            Some(v) => {
                match Vec1::try_from_iter(self.many_term_or_curie_or_absiri(v).filter_map(|v| {
                    match v {
                        NamedOrBlankNode::NamedNode(named_node) => Some(named_node),
                        // TODO: produce a warning here when dropping blank nodes
                        /*
                        self.processor.borrow_mut().emit(
                            PGType::Warning,
                            &format!("@{} cannot refer to a bnode: [{}]", name, b),
                        );
                        */
                        NamedOrBlankNode::BlankNode(_) => None,
                    }
                })) {
                    Err(_) => Attr::Empty,
                    Ok(v) => Attr::Value(v),
                }
            }
        }
    }
}

struct LocalScope<'e> {
    // “An initially empty list of incomplete triples, called the local list of incomplete triples.
    incomplete_triples: Vec<IncompleteTriple>,
    // “A skip element flag, which indicates whether the current element can safely be ignored
    //  since it has no relevant RDFa attributes. Note that descendant elements will still be processed.
    skip_element: bool,
    // “A new subject value, which once calculated will set the parent subject in an evaluation context,
    //  as well as being used to complete any incomplete triples, as described in the next section.
    // NB: we return this from establish_subject
    // “A value for the current object resource, the resource to use when creating triples that have a resource object.
    current_object_resource: Option<Rc<NamedOrBlankNode>>,
    // “A value for the typed resource, the source for creating rdf:type relationships to types specified in @typeof.
    typed_resource: Option<Rc<NamedOrBlankNode>>,
    // “The local list mapping, mapping IRIs to lists
    list_mappings: Rc<RefCell<ListMapping>>,

    // other local vars we need:
    is_root_element: bool,
    in_list: bool,
    property: Attr<Vec1<NamedNode>>,
    content: Option<&'e str>,
    type_of: Attr<Vec1<NamedNode>>,
    about: Attr<Rc<NamedOrBlankNode>>,
    datatype: Attr<NamedNode>,
    resource_present: bool,
    resource_value: Option<Rc<NamedOrBlankNode>>,
}

enum CurieError {
    EmptyCurie,
    InvalidBlankNodeSuffix(String),
    InvalidIRI(String),
    ExpansionError(curie::ExpansionError),
}

struct NotCURIE;
struct NotTERM;

impl<'e> LocalScope<'e> {
    fn new<H: HostLanguage>(
        eval_context: &EvaluationContext<H>,
        resolver: &Resolver<'e, '_, '_, H>,
    ) -> Self {
        // subject attributes
        let about = resolver
            .attr_1_safecurie_or_curie_or_iri("about")
            .map(Rc::new);
        // resource attributes
        let resource = resolver.attr_1_safecurie_or_curie_or_iri("resource");
        let href = resolver.attr_iri("href");
        let src = resolver.attr_iri("src");
        let resource_present = resource.is_present() || href.is_present() || src.is_present();
        let resource_value = resource
            .into_value()
            .or_else(|| Some(href.into_value()?.into()))
            .or_else(|| Some(src.into_value()?.into()))
            .map(Rc::new);

        let is_root_element = resolver.el.name() == "html";
        debug_assert!(is_root_element == eval_context.parent_object.is_none());

        // “First, the local values are initialized, as follows:
        Self {
            // “the skip element flag is set to 'false';
            skip_element: false,
            // “new subject is set to null;
            // new_subject: None,
            // “current object resource is set to null;
            current_object_resource: None,
            // “typed resource is set to null;
            typed_resource: None,
            // “the local list of incomplete triples is set to null;
            incomplete_triples: Default::default(),
            // “the list mapping is set to (a reference of) the list mapping from the evaluation context;
            list_mappings: eval_context.list_mapping.clone(),

            // local attributes:
            about,
            content: resolver.el.attr("content"),
            datatype: resolver.attr_1_term_or_curie_or_absiri("datatype"),
            in_list: resolver.el.attr("inlist").is_some(),
            is_root_element,
            property: resolver.attr_many_term_or_curie_or_absiri("property"),
            resource_present,
            resource_value,
            type_of: resolver.attr_many_term_or_curie_or_absiri("typeof"),
        }
    }

    fn extract_relations<H: HostLanguage>(&self, resolver: &Resolver<H>) -> Option<Vec<Relation>> {
        let rel_dir = if self.in_list {
            Relation::List
        } else {
            Relation::Forward
        };

        let rev_dir = Relation::Reverse;

        let rel: Option<Vec<Relation>>;
        let rev: Option<Vec<Relation>>;
        if self.property.is_present() {
            // [html-rdfa] extension #7
            // > if the @property attribute and the @rel and/or @rev attribute exists
            // > on the same element, the non-CURIE and non-URI @rel and @rev values
            // > are ignored. If, after this, the value of @rel and/or @rev becomes empty,
            // > then the processor MUST act as if the respective attribute is not present.
            rel = match resolver.attr_many_curie_or_absiri("rel") {
                Attr::Missing | Attr::Empty => None,
                Attr::Value(v) => Some(v.into_iter().map(rel_dir).collect()),
            };
            rev = match resolver.attr_many_curie_or_absiri("rev") {
                Attr::Missing | Attr::Empty => None,
                Attr::Value(v) => Some(v.into_iter().map(rev_dir).collect()),
            };
        } else {
            rel = match resolver.attr_many_term_or_curie_or_absiri("rel") {
                Attr::Missing => None,
                Attr::Empty => Some(Vec::new()),
                Attr::Value(v) => Some(v.into_iter().map(rel_dir).collect()),
            };
            rev = match resolver.attr_many_term_or_curie_or_absiri("rev") {
                Attr::Missing => None,
                Attr::Empty => Some(Vec::new()),
                Attr::Value(v) => Some(v.into_iter().map(rev_dir).collect()),
            };
        }

        match (rel, rev) {
            (None, None) => None,
            (Some(rel), None) => Some(rel),
            (None, Some(rev)) => Some(rev),
            (Some(mut rel), Some(rev)) => {
                rel.extend(rev);
                Some(rel)
            }
        }
    }

    // Note that with the HTML host language,
    // the subject will _never_ be None.
    fn establish_subject<H: HostLanguage>(
        &mut self,
        relations: &Option<Vec<Relation>>,
        eval_context: &EvaluationContext<H>,
        resolver: &Resolver<H>,
    ) -> Option<Rc<NamedOrBlankNode>> {
        //5.
        // “If the current element contains no @rel or @rev attribute,
        if relations.is_none() {
            // “then the next step is to establish a value for new subject.
            //  This step has two possible alternatives.
            //
            // 5.1
            // “If the current element contains the @property attribute, but does
            //  not contain either the @content or @datatype attributes, then
            if self.property.is_present() && self.content.is_none() && !self.datatype.is_present() {
                // > new subject is set to the resource obtained from the first match from the following rule:
                let new_subject =
                    // > - by using the resource from @about, if present,
                    if let Some(about) = self.about.as_value() {
                    //  obtained according to the section on CURIE and IRI Processing;
                        trace!(new_subject = %about, "using @about as new subject");
                        Some(about.clone())
                    }
                    // > - otherwise, if the element is the root element of the document,
                    else if self.is_root_element {
                        //  then act as if there is an empty @about present,
                        //  and process it according to the rule for @about, above;
                        let new_subject = Rc::new(resolver.empty_curie().into());
                        trace!(new_subject = %new_subject, "using empty @about as new subject");
                        Some(new_subject)
                    }
                    // > - otherwise, if parent object is present, new subject is set to the value of parent object.
                    else if let Some(parent_obj) = &eval_context.parent_object {
                        trace!(new_subject = %parent_obj, "using parent object as new subject");
                        Some(parent_obj.clone())
                    } else {
                        None // NB: unreachable in HTML
                    };

                // “If @typeof is present then typed resource is set to the resource obtained from
                //  the first match from the following rules:
                if self.type_of.is_present() {
                    let typed_resource =
                        // “by using the resource from @about, if present,
                        if let Some(about) = self.about.as_value() {
                            //  obtained according to the section on CURIE and IRI Processing;
                            trace!(typed_resource = %about, "using @about as typed resource");
                            about.clone()
                        }
                        // “otherwise, if the element is the root element of the document,
                        else if self.is_root_element {
                            // “then act as if there is an empty @about present
                            //  and process it according to the previous rule;
                            Rc::new(resolver.empty_curie().into())
                        }
                        // “otherwise,
                        else {
                            // “by using the resource from @resource, if present, obtained according to the section on CURIE and IRI Processing;
                            // “otherwise, by using the IRI from @href, if present, obtained according to the section on CURIE and IRI Processing;
                            // “otherwise, by using the IRI from @src, if present, obtained according to the section on CURIE and IRI Processing;
                            let r = if let Some(resource) = &self.resource_value {
                                trace!(typed_resource = %resource, "using @resource/@href/@src as typed resource");
                                resource.clone()
                            }
                            // “otherwise, the value of typed resource is set to a newly created bnode.
                            else {
                                let typed_resource = oxrdf::BlankNode::default().into();
                                trace!(typed_resource = %typed_resource, "using new blank node as typed resource");
                                Rc::new(typed_resource)
                            };

                            // ERRATA: this is indented in the spec but maybe should not be? - test suite passes either way
                            // “The value of the current object resource is then set to the value of typed resource.
                            self.current_object_resource = Some(r.clone());
                            r
                        };

                    self.typed_resource = Some(typed_resource);
                }

                new_subject
            }
            // 5.2: “otherwise:
            else {
                let new_subject =
                    // > If the element contains an @about, @href, @src, or @resource attribute,
                    // > new subject is set to the resource obtained as follows:
                    if let Some(about) = self.about.as_value() {
                        // > by using the resource from @about, if present,
                        // > obtained according to the section on CURIE and IRI Processing;
                        trace!(new_subject = %about, "using @about as new subject");
                        Some(about.clone())
                    } else if self.resource_present && let Some(resource) = &self.resource_value {
                        // “otherwise, by using the resource from @resource, if present, obtained according to the section on CURIE and IRI Processing;
                        // “otherwise, by using the IRI from @href, if present, obtained according to the section on CURIE and IRI Processing;
                        // “otherwise, by using the IRI from @src, if present, obtained according to the section on CURIE and IRI Processing.
                        trace!(new_subject = %resource, "using @resource/@href/@src as new subject");
                        Some(resource.clone())
                    } else if resolver.el.name() == "head" || resolver.el.name() == "body" {
                        // [html-rdfa] extension #8
                        // > if no IRI is provided by a resource attribute
                        // > (e.g., @about, @href, @resource, or @src), then
                        // > first check to see if the element is the head or
                        // > body element. If it is, then set new subject to parent object.
                        trace!(new_subject = %eval_context.parent_object.as_ref().unwrap(), "[head/body] using parent object as new subject");
                        eval_context.parent_object.clone()
                    }
                    // > otherwise, if no resource is provided by a resource attribute,
                    // > then the first match from the following rules will apply:
                    // > if the element is the root element of the document,
                    else if self.is_root_element {
                        // > then act as if there is an empty @about present,
                        // > and process it according to the rule for @about, above;
                        let new_subject = Rc::new(resolver.empty_curie().into());
                        trace!(new_subject = %new_subject, "[root element] using empty CURIE as new subject");
                        Some(new_subject)
                    }
                    // > otherwise, if @typeof is present,
                    else if self.type_of.is_present() {
                        // > then new subject is set to be a newly created bnode;
                        let new_subject = Rc::new(oxrdf::BlankNode::default().into());
                        trace!(new_subject = %new_subject, "using blank node as new subject (@typeof present)");
                        Some(new_subject)
                    }
                    // > otherwise, if parent object is present,
                    else if let Some(parent_obj) = &eval_context.parent_object {
                        trace!(new_subject = %parent_obj, "using parent object as new subject");

                        // “Additionally, if @property is not present then the skip element flag is set to 'true'.
                        if !self.property.is_present() {
                            trace!("skip element set to 'true' (no @property present)");
                            self.skip_element = true;
                        }

                        // “new subject is set to the value of parent object.
                        Some(parent_obj.clone())
                    } else {
                        None // NB: unreachable in HTML
                    };

                // “Finally, if @typeof is present, set the typed resource to the value of new subject.
                if self.type_of.is_present() {
                    self.typed_resource = new_subject.clone();
                }

                new_subject
            }
        }
        // 6.
        // > If the current element does contain a @rel or @rev attribute,
        else {
            // > then the next step is to establish _both_ a value for new subject
            // > and a value for current object resource:
            let new_subject = // NB: this is always Some for the HTML host language
                // > new subject is set to the resource obtained from the first match from the following rules:
                // > by using the resource from @about, if present, obtained according to the section on CURIE and IRI Processing;
                if let Some(about) = self.about.as_value() {
                    trace!(new_subject = %about, "using @about as new subject");
                    let new_subject = about.clone();

                    // “if the @typeof attribute is present, set typed resource to new subject.
                    if self.type_of.is_present() {
                        self.typed_resource = Some(new_subject.clone());
                        trace!(typed_resource = %about, "using new subject (@about) as typed resource (@typeof present)");
                    }

                    Some(new_subject)
                }
                // “If no resource is provided then the first match from the following rules will apply:
                // “if the element is the root element of the document
                else if self.is_root_element {
                    // “then act as if there is an empty @about present,
                    //  and process it according to the rule for @about, above;
                    let new_subject = Rc::new(resolver.empty_curie().into());
                    trace!(new_subject = %new_subject, "using empty CURIE as new subject (root element)");
                    Some(new_subject)
                } else if let Some(parent_obj) = &eval_context.parent_object {
                    // ”otherwise, if parent object is present, new subject is set to that.
                    trace!(new_subject = %parent_obj, "using parent object as new subject");
                    Some(parent_obj.clone())
                } else {
                    None // unreachable in HTML
                };

            // > Then the current object resource is set to the resource obtained from the first match from the following rules:
            if let Some(resource) = self.resource_value.as_deref() {
                // “by using the resource from @resource, if present, obtained according to the section on CURIE and IRI Processing;
                //  otherwise, by using the IRI from @href, if present, obtained according to the section on CURIE and IRI Processing;
                //  otherwise, by using the IRI from @src, if present, obtained according to the section on CURIE and IRI Processing;
                trace!(
                    current_object_resource = %resource,
                    "using @resource/@href/@src as current object resource"
                );
                self.current_object_resource = Some(resource.clone().into());
            }
            // “otherwise, if @typeof is present and @about is not, use a newly created bnode.
            else if self.type_of.is_present() && !self.about.is_present() {
                let current_object_resource = Rc::new(oxrdf::BlankNode::default().into());
                trace!(%current_object_resource, "using blank node as current object resource (@typeof present)");
                self.current_object_resource = Some(current_object_resource);
            }

            // “If @typeof is present and @about is not,
            if self.type_of.is_present() && !self.about.is_present() {
                // “set typed resource to current object resource.
                trace!(typed_resource = ?self.current_object_resource, "using current object resource as typed resource (@typeof but no @about)");
                self.typed_resource = self.current_object_resource.clone();
            }

            new_subject

            // “Note that final value of the current object resource will either be null
            //  (from initialization) or a full IRI or bnode.
            // NB: this is guaranteed by the types
        }
    }

    fn emit_type_values(&self, output: &mut OutputGraph) {
        // “If in any of the previous steps a typed resource was set to a non-null value,
        //  it is now used to provide a subject for type values;
        if let Some(typed_resource) = self.typed_resource.as_deref() {
            // “One or more 'types' for the typed resource can be set by using @typeof.
            //  If present, the attribute may contain one or more IRIs, obtained according
            //  to the section on CURIE and IRI Processing, each of which is used to generate
            //  a triple as follows:
            if let Some(type_of) = self.type_of.as_value() {
                for type_iri in type_of {
                    output.emit(TripleRef::new(
                        // subject = typed resource
                        typed_resource,
                        // predicate = http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                        oxrdf::vocab::rdf::TYPE,
                        // object = current full IRI of 'type' from typed resource
                        type_iri,
                    ));
                }
            }
        }
    }

    fn emit_relation_triples(
        &self,
        current_object_resource: &NamedOrBlankNode,
        new_subject: &NamedOrBlankNode,
        relations: &[Relation],
        output: &mut OutputGraph,
    ) {
        if relations.is_empty() {
            return;
        }

        trace!(%current_object_resource, "generating triples");
        let term: Rc<oxrdf::Term> = Rc::new((*current_object_resource).clone().into());
        for relation in relations {
            // > Predicates for the current object resource can be set by using one or both of the @rel and the
            // > @rev attributes but, in case of the @rel attribute, only if the @inlist is not present:
            match relation {
                // > If the element contains both the @inlist and the @rel attributes the @rel may contain
                // > one or more resources, obtained according to the section on CURIE and IRI Processing
                // > each of which is used to add an entry to the list mapping as follows:
                //
                // > if the local list mapping does not contain a list associated with the IRI,
                // > instantiate a new list and add to local list mappings
                //
                // > add the current object resource to the list associated with the
                // > resource in the local list mapping
                Relation::List(predicate) => {
                    self.list_mappings
                        .borrow_mut()
                        .insert_value(predicate.clone(), term.clone());
                }
                // > If present, @rel may contain one or more resources, obtained according
                // > to the section on CURIE and IRI Processing each of which is used to
                // > generate a triple as follows:
                Relation::Forward(predicate) => {
                    output.emit(TripleRef::new(
                        //  subject = new subject
                        new_subject,
                        //  predicate = full IRI
                        predicate,
                        //  object = current object resource
                        current_object_resource,
                    ));
                }
                Relation::Reverse(predicate) => {
                    // “If present, @rev may contain one or more resources,
                    //  obtained according to the section on CURIE and IRI Processing
                    // each of which is used to generate a triple as follows:
                    output.emit(TripleRef::new(
                        //  subject = current object resource
                        current_object_resource,
                        //  predicate = full IRI
                        predicate,
                        //  object = new subject
                        new_subject,
                    ));
                }
            }
        }
    }

    fn establish_content_value<'a, H: HostLanguage>(
        &'_ self,
        element: &'a ElementRef,
        resolver: &Resolver<'e, '_, '_, H>,
    ) -> (Cow<'a, str>, Option<NamedNodeRef<'_>>)
    where
        'e: 'a,
    {
        let mut otherwise_datatype: Option<NamedNodeRef> = None;
        let content_val: Cow<str> = if let Some(content) = self.content {
            content.into()
        } else {
            // [html-rdfa] extension #9 & #10
            let value = resolver.el.attr("datetime").map(Cow::Borrowed).or_else(|| {
                if resolver.el.name() == "time" {
                    Some(Cow::Owned(element.text().join("")))
                } else {
                    None
                }
            });

            if let Some(dt) = value {
                if oxsdatatypes::Duration::from_str(&dt).is_ok() {
                    otherwise_datatype = Some(oxrdf::vocab::xsd::DURATION);
                } else if oxsdatatypes::DateTime::from_str(&dt).is_ok() {
                    otherwise_datatype = Some(oxrdf::vocab::xsd::DATE_TIME);
                } else if oxsdatatypes::Date::from_str(&dt).is_ok() {
                    otherwise_datatype = Some(oxrdf::vocab::xsd::DATE);
                } else if oxsdatatypes::Time::from_str(&dt).is_ok() {
                    otherwise_datatype = Some(oxrdf::vocab::xsd::TIME);
                } else if oxsdatatypes::GYearMonth::from_str(&dt).is_ok() {
                    otherwise_datatype = Some(oxrdf::vocab::xsd::G_YEAR_MONTH);
                } else if oxsdatatypes::GYear::from_str(&dt).is_ok() {
                    otherwise_datatype = Some(oxrdf::vocab::xsd::G_YEAR);
                }

                dt
            } else {
                Cow::Owned(element.text().join(""))
            }
        };

        (content_val, otherwise_datatype)
    }

    fn establish_property_value<'c>(
        &'c self,
        content_val: &'c str,
        content_type: Option<NamedNodeRef<'c>>,
        lang: Option<&'c str>,
        element: &ElementRef,
        relations: &Option<Vec<Relation>>,
        // a buffer, if needed
        serialized: &'c mut String,
    ) -> oxrdf::TermRef<'c>
    where
        'e: 'c,
    {
        match &self.datatype {
            Attr::Empty => {
                trace!("datatype empty, using plain literal");
                // “otherwise, as a plain literal if @datatype is present but has an empty value
                //  according to the section on CURIE and IRI Processing. The actual literal is
                //  either the value of @content (if present) or a string created by concatenating
                //  the value of all descendant text nodes, of the current element in turn.
                if let Some(lang) = lang {
                    oxrdf::LiteralRef::new_language_tagged_literal_unchecked(&content_val, lang)
                        .into()
                } else {
                    oxrdf::LiteralRef::new_simple_literal(&content_val).into()
                }
            }
            Attr::Value(datatype) => {
                if datatype.as_str() == oxrdf::vocab::rdf::XML_LITERAL.as_str() {
                    // “otherwise, as an XML literal if @datatype is present and is set to
                    //  XMLLiteral in the vocabulary http://www.w3.org/1999/02/22-rdf-syntax-ns#.
                    // “The value of the XML literal is a string created by serializing to text,
                    //  all nodes that are descendants of the current element, i.e., not including
                    //  the element itself, and giving it a datatype of XMLLiteral in the vocabulary
                    //  http://www.w3.org/1999/02/22-rdf-syntax-ns#. The format of the resulting
                    //  serialized content is as defined in Exclusive XML Canonicalization Version 1.0 [XML-EXC-C14N].
                    //
                    // [HTML-RDFA]
                    // “When generating literals of type XMLLiteral, the processor MUST ensure that the
                    //  output XMLLiteral is a namespace well-formed XML fragment. A namespace well-formed XML
                    //  fragment has the following properties:
                    // “- The XML fragment, when placed inside of a single root element, MUST validate as well-formed
                    //    XML. The normative language that describes a well-formed XML document is specified in
                    //    Section 2.1 "Well-Formed XML Documents" of the XML specification.
                    // “- The XML fragment, when placed inside of a single root element, MUST retain all active
                    //    namespace information. The currently active attributes declared using @xmlns and @xmlns:
                    //    that are stored in the RDFa processor's current evaluation context in the IRI mappings
                    //    MUST be preserved in the generated XMLLiteral. The PREFIX value for @xmlns:PREFIX MUST
                    //    be entirely transformed into lower-case characters when preserving the value in the
                    //    XMLLiteral. All active namespaces declared via @xmlns, @xmlns:, and @prefix MUST be
                    //    placed in each top-level element in the generated XMLLiteral, taking care to not overwrite
                    //    pre-existing namespace values.
                    // (TODO: the above is not yet implemented, since I can't figure out how to work with
                    //  the scraper API effectively here...)
                    /*
                    let mut output = String::new();
                    for child in element.children() {
                        if let Some(el) = child.value().as_element() {
                            let mut el = el.clone();
                            for (prefix, iri) in local.iri_mappings.mappings() {
                                let name = html5ever::QualName::new(
                                    None,
                                    html5ever::ns!(xmlns),
                                    html5ever::LocalName::from(prefix.as_str()),
                                );
                                el.attrs.push((name, iri.to_string().into()));
                            }
                        } else {
                        }
                    } */
                    *serialized = element.inner_html();
                    oxrdf::LiteralRef::new_typed_literal(serialized.as_str(), datatype).into()
                    // TODO: incorrect, needs to be c14n'd
                } else if datatype.as_str() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#HTML" {
                    *serialized = element.inner_html();
                    oxrdf::LiteralRef::new_typed_literal(serialized.as_str(), datatype).into()
                    // TODO: incorrect, needs to be c14n'd
                } else {
                    // “as a typed literal if @datatype is present, does not have an empty value according
                    //  to the section on CURIE and IRI Processing, and is not set to XMLLiteral in the
                    //  vocabulary http://www.w3.org/1999/02/22-rdf-syntax-ns#.
                    //  The actual literal is either the value of @content (if present) or
                    //  a string created by concatenating the value of all descendant text nodes,
                    //  of the current element in turn. The final string includes the datatype IRI,
                    //  as described in [RDF-SYNTAX-GRAMMAR], which will have been obtained according
                    //  to the section on CURIE and IRI Processing.
                    oxrdf::LiteralRef::new_typed_literal(&content_val, datatype).into()
                }
            }
            Attr::Missing => {
                if let Some(content_type) = content_type {
                    // [html-rdfa] extension #9
                    // “Otherwise, if the value of @datetime lexically matches a valid xsd:date, xsd:time,
                    //  xsd:dateTime, xsd:duration, xsd:gYear, or xsd:gYearMonth a typed literal must be generated,
                    //  with its datatype set to the matching xsd datatype.
                    oxrdf::LiteralRef::new_typed_literal(&content_val, content_type).into()
                } else {
                    // “otherwise, as a plain literal using the value of @content if @content is present.
                    if let Some(content) = self.content {
                        if let Some(lang) = &lang {
                            oxrdf::LiteralRef::new_language_tagged_literal_unchecked(content, lang)
                                .into()
                        } else {
                            oxrdf::LiteralRef::new_simple_literal(content).into()
                        }
                    }
                    // “otherwise, if the @rel, @rev, and @content attributes are not present,
                    //  as a resource obtained from one of the following:
                    //  by using the resource from @resource, if present, obtained according to the section on CURIE and IRI Processing;
                    // otherwise, by using the IRI from @href, if present, obtained according to the section on CURIE and IRI Processing;
                    // otherwise, by using the IRI from @src, if present, obtained according to the section on CURIE and IRI Processing.
                    else if relations.is_none()
                        && self.content.is_none()
                        && let Some(resource_value) = &self.resource_value
                    {
                        resource_value.as_ref().into()
                    }
                    // otherwise, if @typeof is present and @about is not, the value of typed resource.
                    else if self.type_of.is_present() && !self.about.is_present() {
                        self.typed_resource.as_ref().unwrap().as_ref().into()
                    }
                    // otherwise as a plain literal.
                    else if let Some(lang) = &lang {
                        oxrdf::LiteralRef::new_language_tagged_literal_unchecked(&content_val, lang)
                            .into()
                    } else {
                        oxrdf::LiteralRef::new_simple_literal(&content_val).into()
                    }
                }
            }
        }
    }
}

enum Relation {
    Forward(oxrdf::NamedNode),
    Reverse(oxrdf::NamedNode),
    List(oxrdf::NamedNode),
}

#[derive(Clone, Debug)]
enum IncompleteTriple {
    List(Rc<RefCell<Vec<Rc<oxrdf::Term>>>>),
    Forward(oxrdf::NamedNode),
    Reverse(oxrdf::NamedNode),
}

impl<H: HostLanguage> EvaluationContext<H> {
    fn new(base: Iri<String>) -> Self {
        let resolver_data = ResolverData::new(base);
        Self {
            parent_subject: Rc::new(resolver_data.empty_curie().into()),
            parent_object: None,
            list_mapping: Default::default(),
            incomplete_triples: Default::default(),
            resolver: resolver_data,
        }
    }
}

#[derive(derive_more::Error, derive_more::Display, derive_more::From, Debug)]
pub enum Error {
    #[display("IRI parse error: `{iri}`")]
    IriParseError {
        source: oxiri::IriParseError,
        iri: String,
    },

    #[display("Invalid prefix: the prefix '_' is reserved.")]
    ReservedPrefixError(#[error(not(source))] curie::InvalidPrefixError),

    #[display("Invalid CURIE: {}", match _0 { curie::ExpansionError::Invalid => "the prefix on the CURIE has no valid mapping", ExpansionError::MissingDefault => "The CURIE uses a default prefix, but one has not been set" })]
    CurieError(#[error(not(source))] curie::ExpansionError),

    #[display("@prefix syntax error: prefix must end with ':'.")]
    NoColonPrefix,

    LanguageIdentifierError(icu_locale::ParseError),
}

struct RDFaProcessor<'o, 'p> {
    output: RefCell<OutputGraph<'o>>,
    processor: RefCell<ProcessorGraph<'p>>,
}

struct OutputGraph<'o>(&'o mut oxrdf::Graph);
struct ProcessorGraph<'p>(&'p mut oxrdf::Graph);

impl<'o> OutputGraph<'o> {
    fn emit(&mut self, tr: TripleRef) {
        trace!(triple = %tr, "emitting output triple");
        self.0.insert(tr);
    }
}

impl<'p> ProcessorGraph<'p> {
    fn emit(&mut self, pg_type: PGType, msg: &str) {
        let warning_subj: oxrdf::NamedOrBlankNode = oxrdf::BlankNode::default().into();
        let pg_type: oxrdf::NamedNodeRef = pg_type.into();
        // new bnode is-a PGClass
        let node = TripleRef::new(&warning_subj, oxrdf::vocab::rdf::TYPE, pg_type);
        // add description
        let desc = TripleRef::new(
            &warning_subj,
            vocabs::dc::DESCRIPTION,
            oxrdf::LiteralRef::new_simple_literal(msg),
        );
        trace!(triple = %node, "emitting processor triple (type)");
        self.0.insert(node);
        trace!(triple = %desc, "emitting processor triple (description)");
        self.0.insert(desc);
    }
}

enum PGType {
    // Bases
    Error,
    Warning,
    // Derived
    DocumentError,
    VocabReferenceError,
    UnresolvedCurie,
    UnresolvedTerm,
    PrefixRedefinition,
}

impl From<PGType> for oxrdf::NamedNodeRef<'static> {
    fn from(val: PGType) -> Self {
        match val {
            PGType::Error => vocabs::rdfa::ERROR,
            PGType::Warning => vocabs::rdfa::WARNING,
            PGType::DocumentError => vocabs::rdfa::DOCUMENT_ERROR,
            PGType::VocabReferenceError => vocabs::rdfa::VOCAB_REFERENCE_ERROR,
            PGType::UnresolvedCurie => vocabs::rdfa::UNRESOLVED_CURIE,
            PGType::UnresolvedTerm => vocabs::rdfa::UNRESOLVED_TERM,
            PGType::PrefixRedefinition => vocabs::rdfa::PREFIX_REDEFINITION,
        }
    }
}

trait HostLanguage {
    fn default_language() -> Option<LanguageIdentifier>;
    fn default_vocabulary() -> Option<oxrdf::NamedNode>;
}

pub fn initial_context_terms() -> &'static BTreeMap<String, oxrdf::NamedNode> {
    // https://www.w3.org/2011/rdfa-context/rdfa-1.1
    // Vocabulary terms
    static INITIAL_CONTEXT: std::sync::OnceLock<BTreeMap<String, oxrdf::NamedNode>> =
        std::sync::OnceLock::new();
    INITIAL_CONTEXT.get_or_init(|| {
        [
            (
                "describedBy".to_string(),
                oxrdf::NamedNode::new_unchecked("http://www.w3.org/2007/05/powder-s#describedby"),
            ),
            ("license".to_string(), vocabs::xhv::LICENSE.into()),
            ("role".to_string(), vocabs::xhv::ROLE.into()),
        ]
        .into_iter()
        .collect()
    })
}

pub fn initial_context_prefixes() -> &'static PrefixMapping {
    static INITIAL_CONTEXT: std::sync::OnceLock<PrefixMapping> = std::sync::OnceLock::new();
    // https://www.w3.org/2011/rdfa-context/rdfa-1.1
    // Vocabulary prefixes
    INITIAL_CONTEXT.get_or_init(|| {
        let mut mapping = PrefixMapping::default();
        for (prefix, iri) in [
            // Defined by [rdfa-core]
            ("", "http://www.w3.org/1999/xhtml/vocab#"),
            // W3C documents
            ("as", "https://www.w3.org/ns/activitystreams#"),
            ("csvw", "http://www.w3.org/ns/csvw#"),
            ("dcat", "http://www.w3.org/ns/dcat#"),
            ("dqv", "http://www.w3.org/ns/dqv#"),
            ("duv", "http://www.w3.org/ns/duv#"),
            ("grddl", "http://www.w3.org/2003/g/data-view#"),
            ("jsonld", "http://json-ld.org/vocab#"),
            ("ma", "http://www.w3.org/ns/ma-ont#"),
            ("org", "http://www.w3.org/ns/org#"),
            ("owl", "http://www.w3.org/2002/07/owl#"),
            ("prov", "http://www.w3.org/ns/prov#"),
            ("qb", "http://purl.org/linked-data/cube#"),
            ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
            ("rdfa", "http://www.w3.org/ns/rdfa#"),
            ("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
            ("rif", "http://www.w3.org/2007/rif#"),
            ("rr", "http://www.w3.org/ns/r2rml#"),
            ("sd", "http://www.w3.org/ns/sparql-service-description#"),
            ("skos", "http://www.w3.org/2004/02/skos/core#"),
            ("skosxl", "http://www.w3.org/2008/05/skos-xl#"),
            ("sosa", "http://www.w3.org/ns/sosa/"),
            ("ssn", "http://www.w3.org/ns/ssn/"),
            ("time", "http://www.w3.org/2006/time#"),
            ("void", "http://rdfs.org/ns/void#"),
            ("wdr", "http://www.w3.org/2007/05/powder#"),
            ("wdrs", "http://www.w3.org/2007/05/powder-s#"),
            ("xhv", "http://www.w3.org/1999/xhtml/vocab#"),
            ("xml", "http://www.w3.org/XML/1998/namespace"),
            ("xsd", "http://www.w3.org/2001/XMLSchema#"),
            // "widely used"
            ("cc", "http://creativecommons.org/ns#"),
            ("ctag", "http://commontag.org/ns#"),
            ("dc", "http://purl.org/dc/terms/"),
            ("dc11", "http://purl.org/dc/elements/1.1/"),
            ("dcterms", "http://purl.org/dc/terms/"),
            ("foaf", "http://xmlns.com/foaf/0.1/"),
            ("gr", "http://purl.org/goodrelations/v1#"),
            ("ical", "http://www.w3.org/2002/12/cal/icaltzd#"),
            ("og", "http://ogp.me/ns#"),
            ("rev", "http://purl.org/stuff/rev#"),
            ("schema", "http://schema.org/"),
            ("schemas", "https://schema.org/"),
            ("sioc", "http://rdfs.org/sioc/ns#"),
            ("v", "http://rdf.data-vocabulary.org/#"),
            ("vcard", "http://www.w3.org/2006/vcard/ns#"),
        ] {
            mapping.add_prefix(prefix, iri).unwrap();
        }
        mapping
    })
}

struct HTMLHost {}

// [HTML-RDFA] 3.1
// “Documents conforming to the rules in this specification are processed
//  according to [rdfa-core] with the following extensions:
impl HostLanguage for HTMLHost {
    // “The default vocabulary URI is undefined.
    fn default_vocabulary() -> Option<oxrdf::NamedNode> {
        None // TODO
    }

    fn default_language() -> Option<LanguageIdentifier> {
        None
    }

    // “HTML+RDFa uses an additional initial context by default,
    //  http://www.w3.org/2011/rdfa-context/html-rdfa-1.1, which must
    //  be applied after the initial context for [rdfa-core]
    //  (http://www.w3.org/2011/rdfa-context/rdfa-1.1).
    // NB: note that the "additional initial context" is currently empty.
}

impl<'o, 'p> RDFaProcessor<'o, 'p> {
    fn new(output_graph: OutputGraph<'o>, processor_graph: ProcessorGraph<'p>) -> Self {
        Self {
            output: RefCell::new(output_graph),
            processor: RefCell::new(processor_graph),
        }
    }

    fn run(&mut self, eval_context: EvaluationContext<HTMLHost>, html: Html) -> Result<(), Error> {
        enum S<'a> {
            Child(
                ElementRef<'a>,
                Rc<EvaluationContext<HTMLHost>>,
                tracing::Span,
            ),
            OutputList(Rc<NamedOrBlankNode>, Rc<RefCell<ListMapping>>),
        }

        let mut stack = vec![S::Child(
            html.root_element(),
            Rc::new(eval_context),
            tracing::trace_span!("element", name = "html"),
        )];

        while let Some(stack_item) = stack.pop() {
            match stack_item {
                S::Child(element, eval_context, parent_span) => {
                    let _span = parent_span.entered();

                    let new_ctx = Rc::new(self.process_element(eval_context, element)?);
                    if element.has_children() {
                        stack.push(S::OutputList(
                            new_ctx.parent_subject.clone(),
                            new_ctx.list_mapping.clone(),
                        ));
                    }

                    for child in element.children().rev() {
                        if let Some(elref) = ElementRef::wrap(child) {
                            stack.push(S::Child(
                                elref,
                                new_ctx.clone(),
                                tracing::trace_span!("element", name = elref.value().name()),
                            ));
                        }
                    }

                    // 14.
                    // “Finally, if there is one or more mapping in the local list mapping,
                    //  list triples are generated as follows:
                    // “For each IRI in the local list mapping, if the equivalent list does not
                    //  exist in the evaluation context, indicating that the list was originally
                    //  instantiated on the current element, use the list as follows:
                }
                S::OutputList(subject, list_mapping) => {
                    if let Ok(list_mapping) = Rc::try_unwrap(list_mapping) {
                        // 14. (cont)
                        for (iri, list) in list_mapping.into_inner().lists.iter() {
                            // “If there are zero items in the list associated with the IRI, generate the following triple:
                            let mut next: NamedOrBlankNode = vocab::rdf::NIL.into();
                            for item in list.borrow().iter().rev() {
                                let me = oxrdf::BlankNode::default();
                                self.output.get_mut().emit(TripleRef::new(
                                    &me,
                                    vocab::rdf::FIRST,
                                    item.as_ref(),
                                ));
                                self.output.get_mut().emit(TripleRef::new(
                                    &me,
                                    vocab::rdf::REST,
                                    &next,
                                ));
                                next = me.into();
                            }
                            self.output.get_mut().emit(TripleRef::new(
                                subject.as_ref(),
                                iri,
                                &next,
                            ));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    // TOD: implement https://www.w3.org/TR/html-rdfa/#h-additional-rules
    fn process_element<H: HostLanguage>(
        &mut self,
        eval_context: Rc<EvaluationContext<H>>,
        element: scraper::ElementRef,
    ) -> Result<EvaluationContext<H>, Error> {
        trace!(element = ?element, "processing element");

        let el = element.value();

        // 2. 3. 4.
        let resolver = eval_context.resolver.clone().for_element(
            el,
            &self.processor,
            self.output.get_mut(),
        )?;

        // 1.
        let mut local = LocalScope::new(&eval_context, &resolver);

        // 5.
        let relations = local.extract_relations(&resolver);

        // [role-attribute]
        // > If a Host Language contains the @role attribute, then an
        // > RDFa processor processing a document written in that Host Language
        // > according to the rules of that Host Language MAY generate additional
        // > triples for role attributes. If these additional triples are being generated,
        // > then they MUST be generated as follows:
        if let Some(role) = el.attr("role") {
            let role_subject: oxrdf::NamedOrBlankNode =
                // > If @id is present, it is used to supply the subject by concatenating
                // > the document's 'base', a fragment separator '#', and the value of @id.
                if let Some(id) = el.attr("id") {
                    oxrdf::NamedNode::new(resolver.data.base.to_string() + "#" + id)
                        .unwrap()
                        .into()
                }
                // > Otherwise the subject is a unique newly created bnode.
                else {
                    oxrdf::BlankNode::default().into()
                };

            // > Each value of @role is an object, forming an RDF triple with the
            // > subject and predicate defined above. An RDFa Processor MUST behave
            // > as if there is an in-scope vocabulary of http://www.w3.org/1999/xhtml/vocab# for
            // > the value(s) of the @role attribute.
            let mut role_local = resolver.clone();
            role_local.data.default_vocab = Some(oxrdf::NamedNode::new_unchecked(
                "http://www.w3.org/1999/xhtml/vocab#".to_string(),
            ));

            // > Remember that @role values are defined using the datatype TERMorCURIEorAbsIRIs.
            // > An RDFa Processor will intepret these values using the rules for that that datatype
            // > as defined in [RDFA-CORE].
            for role in role_local.many_term_or_curie_or_absiri(role) {
                // > The predicate is the term `role` in the vocabulary
                // > defined at http://www.w3.org/1999/xhtml/vocab.
                self.output
                    .get_mut()
                    .emit(TripleRef::new(&role_subject, vocabs::xhv::ROLE, &role));
            }
        }

        // 6.
        let new_subject = local.establish_subject(&relations, &eval_context, &resolver);

        // 7.
        local.emit_type_values(self.output.get_mut());

        // 8.
        // “If in any of the previous steps a new subject
        //  was set to a non-null value different from the parent object;
        if let Some(ns) = &new_subject
            && Some(ns) != eval_context.parent_object.as_ref()
        {
            // “The list mapping taken from the evaluation context is set to a new, empty mapping.
            trace!("new list mapping set");
            local.list_mappings = Default::default();
        }

        // 9.
        if let Some(relations) = &relations {
            // “If in any of the previous steps a current object resource was set to a non-null value,
            //  it is now used to generate triples and add entries to the local list mapping:
            if let Some(current_object_resource) = local.current_object_resource.as_deref() {
                local.emit_relation_triples(
                    current_object_resource,
                    new_subject.as_ref().unwrap(),
                    relations,
                    self.output.get_mut(),
                );
            }
            // [rdfa-core] 7.5: 10.
            // > If however current object resource was set to null, but there are predicates present,
            // > then they must be stored as incomplete triples, pending the discovery of a subject
            // > that can be used as the object.
            else {
                debug_assert!(local.current_object_resource.is_none());
                trace!("generating incomplete triples (current object resource null)");
                // > Also, current object resource should be set to a newly created bnode
                // > (so that the incomplete triples have a subject to connect to if they are ultimately turned into triples);
                local.current_object_resource = Some(Rc::new(oxrdf::BlankNode::default().into()));
                for relation in relations {
                    match relation {
                        // > Predicates for incomplete triples can be set by using one or both of the @rel and @rev attributes:
                        //
                        // > If the element contains the @inlist attribute,
                        // > then if the local list mapping does not contain a list associated with the IRI,
                        // > instantiate a new list and add to local list mappings.
                        Relation::List(p) => {
                            let list = local.list_mappings.borrow_mut().ensure_list(p);
                            local.incomplete_triples.push(IncompleteTriple::List(list));
                        }
                        // > If present, @rel must contain one or more resources,
                        // > obtained according to the section  on CURIE and IRI Processing,
                        // > each of which is added to the local list of incomplete triples as follows:
                        Relation::Forward(p) => {
                            // > - predicate = full IRI
                            // > - direction = forward
                            local
                                .incomplete_triples
                                .push(IncompleteTriple::Forward(p.clone()));
                        }
                        // > If present, @rev must contain one or more resources,
                        // > obtained according to the section on CURIE and IRI Processing,
                        // > each of which is added to the local list of incomplete triples as follows:
                        Relation::Reverse(p) => {
                            // > - predicate = full IRI
                            // > - direction = reverse
                            local
                                .incomplete_triples
                                .push(IncompleteTriple::Reverse(p.clone()));
                        }
                    }
                }

                trace!(triples = ?local.incomplete_triples, "incomplete triples ");
            }
        }

        // 11.
        // “The next step of the iteration is to establish any current property value;
        //  Predicates for the current property value can be set by using @property.
        //  If present, one or more resources are obtained according to the section on
        //  CURIE and IRI Processing, and then the actual literal value is obtained as follows:
        let mut no_prop = Attr::Missing;
        std::mem::swap(&mut no_prop, &mut local.property);
        if let Some(properties) = no_prop.into_value() {
            let (content_val, content_type) = local.establish_content_value(&element, &resolver);
            let lang = resolver.data.language.as_ref().map(|l| l.to_string());
            let mut buffer = String::new();
            let current_property_value = local.establish_property_value(
                &content_val,
                content_type,
                lang.as_deref(),
                &element,
                &relations,
                &mut buffer,
            );

            // “The current property value is then used with each predicate as follows:
            //
            // “If the element also includes the @inlist attribute, the current property
            //  value is added to the local list mapping as follows:
            if local.in_list {
                // “if the local list mapping does not contain a list associated with the predicate IRI,
                //  instantiate a new list and add to local list mappings
                //
                //  add the current property value to the list associated with the predicate IRI in the local list mapping
                let term: Rc<oxrdf::Term> = Rc::new(current_property_value.into());
                for property in properties {
                    local
                        .list_mappings
                        .borrow_mut()
                        .insert_value(property, term.clone());
                }
            } else {
                // “Otherwise the current property value is used to generate a triple as follows:
                if let Some(sub) = new_subject.as_deref() {
                    for property in properties {
                        self.output.get_mut().emit(TripleRef::new(
                            // subject = new subject
                            sub,
                            // predicate = full IRI
                            property.as_ref(),
                            // object = current property value
                            current_property_value,
                        ));
                    }
                }
            };
        }

        // 12.
        // “If the skip element flag is 'false', and new subject was set to a non-null value,
        //  then any incomplete triples within the current context should be completed:
        if !local.skip_element
            && let Some(new_subject) = &new_subject
        {
            // “The list of incomplete triples from the current evaluation context
            //  (not the local list of incomplete triples) will contain zero or more predicate
            //  IRIs. This list is iterated over and each of the predicates is used with parent
            //  subject and new subject to generate a triple or add a new element to the local
            //  list mapping. Note that at each level there are two lists of incomplete triples;
            //  one for the current processing level (which is passed to each child element in
            //  the previous step), and one that was received as part of the evaluation context.
            //  It is the latter that is used in processing during this step.
            for incomplete in eval_context.incomplete_triples.iter() {
                // “Note that each incomplete triple has a direction value that is used to determine
                //  what will become the subject, and what will become the object, of each generated triple:
                match incomplete {
                    // “If direction is 'none',
                    //  the new subject is added to the list from the iterated incomplete triple.
                    IncompleteTriple::List(list) => list
                        .borrow_mut()
                        .push(Rc::new(new_subject.as_ref().clone().into())),
                    // “If direction is 'forward' then the following triple is generated:
                    IncompleteTriple::Forward(predicate) => {
                        self.output.get_mut().emit(TripleRef::new(
                            // subject = parent subject
                            eval_context.parent_subject.as_ref(),
                            // predicate = the predicate from the iterated incomplete triple
                            predicate.as_ref(),
                            // object = new subject
                            new_subject.as_ref(),
                        ));
                    }
                    // “If direction is 'reverse' then this is the triple generated:
                    IncompleteTriple::Reverse(predicate) => {
                        self.output.get_mut().emit(TripleRef::new(
                            // subject = new subject
                            new_subject.as_ref(),
                            // predicate = the predicate from the iterated incomplete triple
                            predicate.as_ref(),
                            // object = parent subject
                            eval_context.parent_subject.as_ref(),
                        ));
                    }
                }
            }
        }

        // 13.
        // “Next, all elements that are children of the current element are processed
        //  using the rules described here, using a new evaluation context, initialized as follows:
        //
        //  If the skip element flag is 'true' then the new evaluation context is a copy of the current
        //  context that was passed in to this level of processing, with the language and list of IRI
        //  mappings values replaced with the local values;
        if local.skip_element {
            let mut eval_context = Rc::unwrap_or_clone(eval_context);
            // ERRATA: in addition to current_language and iri_mappings,
            // we copy default_vocab and term_mappings (both unspecified)
            // default_vocab at least seems to be required to pass test suite
            eval_context.resolver = resolver.data;
            Ok(eval_context)
        } else {
            // “ Otherwise, the values are:
            Ok(EvaluationContext {
                // “ the parent subject is set to the value of new subject, if non-null,
                //   or the value of the parent subject of the current evaluation context;
                parent_subject: new_subject
                    .clone()
                    .unwrap_or_else(|| eval_context.parent_subject.clone()),
                // “ the parent object is set to value of current object resource, if non-null,
                //   or the value of new subject, if non-null, or the value of the parent subject
                //   of the current evaluation context;
                parent_object: Some(
                    local
                        .current_object_resource
                        .as_ref()
                        .or(new_subject.as_ref())
                        .cloned()
                        .unwrap_or_else(|| eval_context.parent_subject.clone()),
                ),
                // “ the list of incomplete triples is set to the local list of incomplete triples;
                incomplete_triples: local.incomplete_triples,
                // “ the list mapping is set to the local list mapping;
                list_mapping: local.list_mappings,
                // “ the base is set to the base value of the current evaluation context;
                // “ language is set to the value of current language.
                // “ the default vocabulary is set to the value of the local default vocabulary.
                // “ the list of IRI mappings is set to the local list of IRI mappings;
                // ERRATA: undocumented, but assumed that term_mappings is copied
                resolver: resolver.data,
            })
        }
    }
}
