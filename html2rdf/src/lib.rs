use std::cell::RefCell;
use std::collections::BTreeMap;
use std::ops::Not;
use std::rc::Rc;
use std::{borrow::Cow, str::FromStr};

use curie::{Curie, ExpansionError, PrefixMapping};
use icu::locale::LanguageIdentifier;
use itertools::Itertools;
use oxiri::{Iri, IriParseError};
use oxrdf::vocab::{self, rdf};
use oxrdf::{Graph, NamedNode, NamedNodeRef, NamedOrBlankNode, TermRef, TripleRef};
use scraper::{ElementRef, Html};
use vec1::{Size0Error, Vec1};

pub mod reference_impl;

macro_rules! trace {
    ($($args:expr),*) => {
        #[cfg(debug_assertions)]
        println!($($args),*);
    };
}

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
    let mut added_any = true;
    while added_any {
        added_any = false;

        let mut new_triples = Graph::new();
        for copy_triple in graph.triples_for_predicate(rdfa_vocab::COPY) {
            let copy_target: oxrdf::NamedOrBlankNodeRef = match copy_triple.object {
                TermRef::NamedNode(n) => n.into(),
                TermRef::BlankNode(n) => n.into(),
                TermRef::Literal(_) => {
                    continue; // TODO: warning?
                }
            };

            if !graph.contains(TripleRef::new(copy_target, rdf::TYPE, rdfa_vocab::PATTERN)) {
                continue; // TODO: warning?
            }

            for trip in graph.triples_for_subject(copy_target) {
                new_triples.insert(TripleRef::new(
                    copy_triple.subject,
                    trip.predicate,
                    trip.object,
                ));
            }
        }

        for triple in new_triples.iter() {
            added_any |= graph.insert(triple);
        }
    }

    let mut triples_to_remove = Graph::new();
    for copy_triple in graph.triples_for_predicate(rdfa_vocab::COPY) {
        triples_to_remove.insert(copy_triple);
        let copy_target: oxrdf::NamedOrBlankNodeRef = match copy_triple.object {
            TermRef::NamedNode(n) => n.into(),
            TermRef::BlankNode(n) => n.into(),
            TermRef::Literal(_) => continue,
        };

        if !graph.contains(TripleRef::new(copy_target, rdf::TYPE, rdfa_vocab::PATTERN)) {
            continue;
        }

        triples_to_remove.insert(TripleRef::new(
            copy_triple.subject,
            rdf::TYPE,
            rdfa_vocab::PATTERN,
        ));

        for trip in graph.triples_for_subject(copy_target) {
            triples_to_remove.insert(trip);
        }
    }

    for triple in triples_to_remove.iter() {
        graph.remove(triple);
    }
}

pub fn parse(
    input: &str,
    mut base: Iri<String>,
    output_graph: &mut Graph,
    processor_graph: &mut Graph,
) -> Result<(), Error> {
    let get_err = || -> Result<(), Error> {
        let doc = Html::parse_document(input);
        if !doc.errors.is_empty() {
            for err in doc.errors.iter() {
                println!("Error: {}", err);
            }
        }
        let mut proc = RDFaProcessor::new(output_graph, processor_graph);

        let base_sel = scraper::selector::Selector::parse("html>head>base").unwrap();

        if let Some(base_el) = doc.select(&base_sel).next() {
            if let Some(base_href) = base_el.attr("href") {
                base =
                    Iri::parse(base_href.to_string()).map_err(|source| Error::IriParseError {
                        source,
                        iri: base_href.to_string(),
                    })?;

                trace!("<base> found: {base}");
            }
        }

        let eval_context = EvaluationContext::new(base);
        proc.run(eval_context, doc)?;
        Ok(())
    }();

    if let Err(e) = get_err {
        emit_processor(processor_graph, PGType::DocumentError, &e.to_string());
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

    pub fn value(&self) -> Option<&T> {
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

        trace!(" - Created new list for predicate: {}", predicate);
        let shared_list: Rc<SharedList> = Default::default();
        let replaced = self.lists.insert(predicate.clone(), shared_list.clone());
        debug_assert!(replaced.is_none());
        shared_list
    }

    pub fn insert_value(&mut self, predicate: oxrdf::NamedNode, term: Rc<oxrdf::Term>) {
        trace!(" - Inserting into list ({predicate}): {}", term);
        let list = match self.lists.get(&predicate) {
            Some(list) => list,
            None => {
                trace!("  - Created new list for predicate: {}", predicate);
                self.lists.entry(predicate).or_default()
            }
        };

        list.borrow_mut().push(term);
    }

    pub fn is_empty(&self) -> bool {
        self.lists.is_empty()
    }

    pub fn except_for(&self, other: &ListMapping) -> Self {
        let mut lists = self.lists.clone();
        lists.retain(|key, _| !other.lists.contains_key(key));
        Self { lists }
    }
}

#[derive(Clone)]
// “During processing, each rule is applied using information provided by an evaluation context.
//  An initial context is created when processing begins. That context has the following members:
struct EvaluationContext {
    /// This is used to represent [_:], which is allowed in RDFa
    /// but not in Turtle.
    empty_bnode: oxrdf::BlankNode,

    // “The base. This will usually be the IRI of the document being processed,
    //  but it could be some other IRI, set by some other mechanism, such as the (X)HTML base element.
    //  The important thing is that it establishes an IRI against which relative paths can be resolved.
    base: Iri<String>,

    // “The parent subject. The initial value will be the same as the initial value of base,
    //  but it will usually change during the course of processing.
    parent_subject: Rc<oxrdf::NamedOrBlankNode>,

    // “The parent object. In some situations the object of a statement becomes the subject
    //  of any nested statements, and this member is used to convey this value. Note that this
    //  value may be a bnode, since in some situations a number of nested statements are grouped
    //  together on one bnode. This means that the bnode must be set in the containing statement and passed down.
    parent_object: Option<Rc<oxrdf::NamedOrBlankNode>>, // note that this is a subject, not an object, but the parent object cannot be a term

    // “A list of current, in-scope IRI mappings.
    iri_mappings: Rc<curie::PrefixMapping>,

    // “A list of incomplete triples. A triple can be incomplete when no object resource is provided
    //  alongside a predicate that requires a resource (i.e., @rel or @rev). The triples can be completed
    //  when a resource becomes available, which will be when the next subject is specified
    //  (part of the process called chaining).
    incomplete_triples: Vec<IncompleteTriple>,

    // “A list mapping that associates IRIs with lists.
    list_mapping: Rc<RefCell<ListMapping>>,

    // “The language. Note that there is no default language.
    language: Option<Rc<LanguageIdentifier>>,

    // “The term mappings, a list of terms and their associated IRIs.
    //  This specification does not define an initial list. Host Languages MAY define an initial list.
    term_mappings: Rc<BTreeMap<String, oxrdf::NamedNode>>,

    // “The default vocabulary, a value to use as the prefix IRI when a term unknown to the RDFa Processor
    //  is used. This specification does not define an initial setting for the default vocabulary.
    //  Host Languages MAY define an initial setting.
    default_vocab: Option<oxrdf::NamedNode>,
}

// “During the course of processing a number of locally scoped values are needed, as follows:
#[derive(Clone)]
struct LocalScope<'a> {
    emit_warning: &'a dyn Fn(PGType, String),
    eval_context: &'a EvaluationContext,
    // “An initially empty list of IRI mappings, called the local list of IRI mappings.
    iri_mappings: Rc<curie::PrefixMapping>,
    // “An initially empty list of incomplete triples, called the local list of incomplete triples.
    incomplete_triples: Vec<IncompleteTriple>,
    // “An initially empty language value.
    current_language: Option<Rc<LanguageIdentifier>>,
    // “A skip element flag, which indicates whether the current element can safely be ignored
    //  since it has no relevant RDFa attributes. Note that descendant elements will still be processed.
    skip_element: bool,
    // “A new subject value, which once calculated will set the parent subject in an evaluation context,
    //  as well as being used to complete any incomplete triples, as described in the next section.
    new_subject: Option<Rc<NamedOrBlankNode>>,
    // “A value for the current object resource, the resource to use when creating triples that have a resource object.
    current_object_resource: Option<Rc<NamedOrBlankNode>>,
    // “A value for the typed resource, the source for creating rdf:type relationships to types specified in @typeof.
    typed_resource: Option<Rc<NamedOrBlankNode>>,
    // “The local term mappings, a list of terms and their associated IRIs.
    term_mappings: Rc<BTreeMap<String, oxrdf::NamedNode>>,
    // “The local list mapping, mapping IRIs to lists
    list_mappings: Rc<RefCell<ListMapping>>,
    // “A local default vocabulary, an IRI to use as a prefix mapping when a term is used.
    default_vocab: Option<oxrdf::NamedNode>,
}

enum CurieError {
    EmptyCurie,
    InvalidIRI(String),
    ExpansionError(curie::ExpansionError),
}

struct NotCURIE;
struct NotTERM;

impl<'b> LocalScope<'b> {
    fn new(eval_context: &'b EvaluationContext, emit_warning: &'b dyn Fn(PGType, String)) -> Self {
        // “First, the local values are initialized, as follows:
        Self {
            emit_warning,
            eval_context,
            // “the skip element flag is set to 'false';
            skip_element: false,
            // “new subject is set to null;
            new_subject: None,
            // “current object resource is set to null;
            current_object_resource: None,
            // “typed resource is set to null;
            typed_resource: None,
            // “the local list of IRI mappings is set to the list of IRI mappings from the evaluation context;
            iri_mappings: eval_context.iri_mappings.clone(),
            // “the local list of incomplete triples is set to null;
            incomplete_triples: Default::default(),
            // “the list mapping is set to (a reference of) the list mapping from the evaluation context;
            list_mappings: eval_context.list_mapping.clone(),
            // “the current language value is set to the language value from the evaluation context.
            current_language: eval_context.language.clone(),
            // “the local term mappings is set to the term mappings from the evaluation context.
            term_mappings: eval_context.term_mappings.clone(),
            // “the local default vocabulary is set to the default vocabulary from the evaluation context.
            default_vocab: eval_context.default_vocab.clone(),
        }
    }

    /// An empty CURIE resolves to the [`Self::base`] value.
    fn empty_curie(&self) -> oxrdf::NamedNodeRef {
        oxrdf::NamedNodeRef::new_unchecked(self.eval_context.base.as_str())
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
            if let Some(vocab) = &self.default_vocab {
                let mut iri = vocab.as_str().to_string();
                iri.push_str(term);
                let named_node = oxrdf::NamedNode::new(iri).expect("always a valid IRI");
                Ok(Some(named_node))
            }
            // > Otherwise, check if the term matches an item in the list of local term mappings.
            //
            // > First compare against the list case-sensitively,
            else if let Some(term_iri) = self.term_mappings.get(term) {
                Ok(Some(term_iri.clone()))
            }
            // > and if there is no match then compare case-insensitively.
            else if let Some(term) = self
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
                    return Ok(self.eval_context.empty_bnode.clone().into());
                }

                return Ok(oxrdf::BlankNode::new(suffix).expect("TODO").into());
            }

            Curie::new(Some(prefix), suffix)
        } else {
            Curie::new(None, value)
        };

        match self.iri_mappings.expand_curie(&curie) {
            Ok(iri) => {
                // See note just before [rdfa-core] 7.4.1:
                // Usually this should be an absolute IRI, but it is
                // _possible_ (though not recommended) that a relative IRI
                // can be used as a prefix, so we will resolve it in case.
                match self.resolve_relative_iri(&iri) {
                    Ok(absolute_iri) => Ok(absolute_iri.into()),
                    Err(_) => Err(CurieError::InvalidIRI(iri)),
                }
            }
            Err(err) => Err(CurieError::ExpansionError(err)),
        }
    }

    /// Resolves a SafeCURIE or CURIE to an IRI or bnode.
    fn resolve_safecuri_or_curie(&self, value: &str) -> Result<Option<NamedOrBlankNode>, NotCURIE> {
        if value.starts_with('[') && value.ends_with(']') {
            match self.resolve_curie(&value[1..value.len() - 1]) {
                Ok(iri) => Ok(Some(iri)),
                Err(err) => {
                    // if it's a SafeCURIE we MUST ignore it
                    // produce a warning if the prefix was not defined
                    match err {
                        CurieError::EmptyCurie
                        | CurieError::ExpansionError(ExpansionError::MissingDefault) => {}
                        CurieError::InvalidIRI(iri) => {
                            (self.emit_warning)(
                                PGType::UnresolvedCurie,
                                format!(
                                    "Invalid CURIE: {value} (expanded to invalid IRI value <{iri}>)",
                                ),
                            );
                        }
                        CurieError::ExpansionError(ExpansionError::Invalid) => {
                            (self.emit_warning)(
                                PGType::UnresolvedCurie,
                                format!("Invalid CURIE: {value} (no such prefix defined)"),
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
        self.resolve_relative_iri(value).ok()
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

        let iri = self.eval_context.base.resolve(value)?;
        debug_assert!(Iri::parse(iri.as_str()).is_ok());
        Ok(oxrdf::NamedNode::new_unchecked(iri.into_inner()))
    }

    /// Resolves an absolute IRI.
    fn resolve_absolute_iri(&self, value: &str) -> Result<Option<NamedNode>, IriParseError> {
        Ok(Some(oxrdf::NamedNode::new(value.to_string())?))
    }

    fn safecuri_or_curie_or_iri(&self, value: &str) -> Option<NamedOrBlankNode> {
        match self.resolve_safecuri_or_curie(value) {
            Ok(val) => val, // value or MUST be ignored
            Err(NotCURIE) => {
                // not a CURIE, will try IRI
                match self.resolve_relative_iri(value) {
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
        (self.emit_warning)(
            PGType::Warning,
            format!("Invalid IRI: <{}> ({})", value, iri_err),
        );
    }

    fn term_or_curie_or_absiri(&self, value: &str) -> Option<NamedOrBlankNode> {
        match self.resolve_term(value) {
            Ok(result) => result.map(NamedOrBlankNode::from), // value or MUST be ignored
            Err(NotTERM) => self.curie_or_absiri(value),
        }
    }

    fn many_curie_or_absiri(&self, value: &str) -> Vec<NamedOrBlankNode> {
        value
            .split_ascii_whitespace()
            .filter_map(|v| self.curie_or_absiri(v))
            .collect()
    }

    fn many_term_or_curie_or_absiri(&self, value: &str) -> Vec<NamedOrBlankNode> {
        value
            .split_ascii_whitespace()
            .filter_map(|v| self.term_or_curie_or_absiri(v))
            .collect()
    }

    fn many_safecuri_or_curie_or_iri(&self, value: &str) -> Vec<NamedOrBlankNode> {
        value
            .split_ascii_whitespace()
            .filter_map(|v| self.safecuri_or_curie_or_iri(v))
            .collect()
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

impl EvaluationContext {
    fn new(base: Iri<String>) -> Self {
        let mut iri_mappings = PrefixMapping::default();
        for (prefix, iri) in initial_context_prefixes().mappings() {
            iri_mappings.add_prefix(prefix, iri).unwrap()
        }
        let term_mappings = Rc::new(initial_context_terms().clone());

        Self {
            // generate a random blank node for the empty blank node value
            empty_bnode: oxrdf::BlankNode::default(),
            // resolve the base to remove any fragments
            // so that we can use it directly as the "empty CURIE" value
            base: base.resolve("").unwrap(),
            parent_subject: Rc::new(oxrdf::NamedNode::new_unchecked(base.into_inner()).into()),
            term_mappings,
            parent_object: None,
            language: None,
            default_vocab: None,
            list_mapping: Default::default(),
            incomplete_triples: Default::default(),
            iri_mappings: Rc::new(iri_mappings),
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

    LanguageIdentifierError(icu::locale::ParseError),
}

mod dc_vocab {
    pub static DESCRIPTION: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://purl.org/dc/terms/description");
}

mod xhv_vocab {
    pub static ROLE: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/1999/xhtml/vocab#role");
}

mod rdfa_vocab {
    pub static COPY: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#copy");

    pub static PATTERN: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#Pattern");

    pub static ERROR: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#Error");

    pub static WARNING: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#Warning");

    pub static DOCUMENT_ERROR: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#DocumentError");

    pub static VOCAB_REFERENCE_ERROR: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#VocabReferenceError");

    pub static UNRESOLVED_CURIE: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#UnresolvedCurie");

    pub static UNRESOLVED_TERM: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#UnresolvedTerm");

    pub static PREFIX_REDEFINITION: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#PrefixRedefinition");

    pub static CONTEXT_PROPERTY: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#context");

    pub static USES_VOCABULARY: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#usesVocabulary");
}

struct RDFaProcessor<'o, 'p> {
    output_graph: RefCell<&'o mut oxrdf::Graph>,
    processor_graph: RefCell<&'p mut oxrdf::Graph>,
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
            PGType::Error => rdfa_vocab::ERROR,
            PGType::Warning => rdfa_vocab::WARNING,
            PGType::DocumentError => rdfa_vocab::DOCUMENT_ERROR,
            PGType::VocabReferenceError => rdfa_vocab::VOCAB_REFERENCE_ERROR,
            PGType::UnresolvedCurie => rdfa_vocab::UNRESOLVED_CURIE,
            PGType::UnresolvedTerm => rdfa_vocab::UNRESOLVED_TERM,
            PGType::PrefixRedefinition => rdfa_vocab::PREFIX_REDEFINITION,
        }
    }
}

trait HostLanguage {
    fn default_language(&self) -> Option<LanguageIdentifier>;
    fn default_vocabulary(&self) -> Option<oxrdf::NamedNode>;
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
            (
                "license".to_string(),
                oxrdf::NamedNode::new_unchecked("http://www.w3.org/1999/xhtml/vocab#license"),
            ),
            (
                "role".to_string(),
                oxrdf::NamedNode::new_unchecked("http://www.w3.org/1999/xhtml/vocab#role"),
            ),
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
impl HostLanguage for &HTMLHost {
    // “The default vocabulary URI is undefined.
    fn default_vocabulary(&self) -> Option<oxrdf::NamedNode> {
        None // TODO
    }

    // “The current language can be set using either the @lang
    //  or @xml:lang attributes. When the @lang attribute and
    //  the @xml:lang attribute are specified on the same element,
    //  the @xml:lang attribute takes precedence. When both @lang
    //  and @xml:lang are specified on the same element, they MUST
    //  have the same value. Further details related to setting the
    //  current language can be found in section 3.3 Specifying the
    //  Language for a Literal.
    fn default_language(&self) -> Option<LanguageIdentifier> {
        None
    }

    // “HTML+RDFa uses an additional initial context by default,
    //  http://www.w3.org/2011/rdfa-context/html-rdfa-1.1, which must
    //  be applied after the initial context for [rdfa-core]
    //  (http://www.w3.org/2011/rdfa-context/rdfa-1.1).
    // NB: note that the "additional initial context" is currently empty.
}

fn emit_processor(pg: &mut Graph, pg_type: PGType, msg: &str) {
    let warning_subj: oxrdf::Subject = oxrdf::BlankNode::default().into();
    let pg_type: oxrdf::NamedNodeRef = pg_type.into();
    // new bnode is-a PGClass
    let node = TripleRef::new(&warning_subj, oxrdf::vocab::rdf::TYPE, pg_type);
    // add description
    let desc = TripleRef::new(
        &warning_subj,
        dc_vocab::DESCRIPTION,
        oxrdf::LiteralRef::new_simple_literal(msg),
    );
    trace!("Emitting processor: {node}");
    pg.insert(node);
    trace!("Emitting processor: {desc}");
    pg.insert(desc);
}

impl<'o, 'p> RDFaProcessor<'o, 'p> {
    fn new(output_graph: &'o mut oxrdf::Graph, processor_graph: &'p mut oxrdf::Graph) -> Self {
        Self {
            output_graph: RefCell::new(output_graph),
            processor_graph: RefCell::new(processor_graph),
        }
    }

    fn run(&mut self, eval_context: EvaluationContext, html: Html) -> Result<(), Error> {
        enum S<'a> {
            Child(ElementRef<'a>, Rc<EvaluationContext>),
            OutputList(Rc<NamedOrBlankNode>, Rc<RefCell<ListMapping>>),
        }

        // TODO: we need a marker on the stack to emit list elements
        let mut stack = vec![S::Child(html.root_element(), Rc::new(eval_context))];

        let host = HTMLHost {};

        while let Some(stack_item) = stack.pop() {
            match stack_item {
                S::Child(element, base_ctx) => {
                    let new_ctx = Rc::new(self.process_element(&base_ctx, element, &host)?);
                    if element.has_children() {
                        stack.push(S::OutputList(
                            new_ctx.parent_subject.clone(),
                            new_ctx.list_mapping.clone(),
                        ));
                    }

                    for child in element.children().rev() {
                        if let Some(elref) = ElementRef::wrap(child) {
                            stack.push(S::Child(elref, new_ctx.clone()));
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
                    if let Some(list_mapping) = Rc::try_unwrap(list_mapping).ok() {
                        // 14. (cont)
                        for (iri, list) in list_mapping.into_inner().lists.iter() {
                            // “If there are zero items in the list associated with the IRI, generate the following triple:
                            let mut next: NamedOrBlankNode = vocab::rdf::NIL.into();
                            for item in list.borrow().iter().rev() {
                                let me = oxrdf::BlankNode::default();
                                self.emit_output(TripleRef::new(
                                    &me,
                                    vocab::rdf::FIRST,
                                    item.as_ref(),
                                ));
                                self.emit_output(TripleRef::new(&me, vocab::rdf::REST, &next));
                                next = me.into();
                            }
                            self.emit_output(TripleRef::new(subject.as_ref(), iri, &next));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn emit_output(&self, tr: TripleRef) {
        trace!("- Emitting output triple: {tr}");
        self.output_graph.borrow_mut().insert(tr);
    }

    // TOD: implement https://www.w3.org/TR/html-rdfa/#h-additional-rules
    fn process_element(
        &mut self,
        eval_context: &EvaluationContext,
        element: scraper::ElementRef,
        host: impl HostLanguage,
    ) -> Result<EvaluationContext, Error> {
        let emit_warning = |pgtype: PGType, msg: String| {
            let mut pg = self.processor_graph.borrow_mut();
            emit_processor(&mut pg, pgtype, &msg);
        };

        let el = element.value();

        let attr_iri = |name, proj: &dyn Fn(&str) -> Option<NamedNode>| match el.attr(name) {
            None => Attr::Missing,
            Some(v) => match proj(v) {
                None => Attr::Empty,
                Some(v) => Attr::Value(v),
            },
        };

        let attr1 = |name, proj: &dyn Fn(&str) -> Option<NamedOrBlankNode>| match el.attr(name) {
            None => Attr::Missing,
            Some(v) => match proj(v) {
                None => Attr::Empty,
                Some(v) => Attr::Value(v),
            },
        };

        let attr_many = |name, proj: &dyn Fn(&str) -> Vec<NamedOrBlankNode>| match el.attr(name) {
            None => Attr::Missing,
            Some(v) => match Vec1::try_from_vec(proj(v)) {
                Err(Size0Error) => Attr::Empty,
                Ok(v) => Attr::Value(v),
            },
        };

        let attr_many_pred =
            |name, proj: &dyn Fn(&str) -> Vec<NamedOrBlankNode>| match el.attr(name) {
                None => Attr::Missing,
                Some(v) => {
                    let data = proj(v)
                        .into_iter()
                        .filter_map(|v| self.to_predicate(name, v))
                        .collect();
                    match Vec1::try_from_vec(data) {
                        Err(Size0Error) => Attr::Empty,
                        Ok(v) => Attr::Value(v),
                    }
                }
            };

        if cfg!(debug_assertions) {
            let mut ancestor_stack = Vec::new();
            ancestor_stack.extend(
                element
                    .ancestors()
                    .filter_map(|x| x.value().as_element().map(|x| x.name())),
            );
            ancestor_stack.reverse();
            let attrs = el.attrs().map(|(n, v)| format!("@{}='{}'", n, v)).join(" ");
            trace!(
                "{}{}{} {attrs}",
                ancestor_stack.join(">"),
                ancestor_stack
                    .is_empty()
                    .not()
                    .then_some(">")
                    .unwrap_or_default(),
                el.name()
            );
        }

        let is_root_element = el.name() == "html";
        debug_assert!(is_root_element == eval_context.parent_object.is_none());

        // 1.
        let mut local = LocalScope::new(eval_context, &emit_warning);

        // [rdfa-core] 7.5: 2.
        // > Next the current element is examined for any change to the default vocabulary via @vocab.
        if let Some(vocab) = el.attr("vocab") {
            if vocab.is_empty() {
                trace!("- @vocab is empty, resetting default vocabulary");
                // > If the value is empty, then the local default vocabulary
                // > MUST be reset to the Host Language defined default (if any).
                local.default_vocab = host.default_vocabulary();
            }
            // > If @vocab is present and contains a value,
            else if let Ok(vocab) = local.resolve_relative_iri(vocab) {
                trace!("- default vocabulary is now: {vocab}");
                // > the local default vocabulary is updated according to the
                // > section on CURIE and IRI Processing.
                //
                // > The value of @vocab is used to generate a triple as follows:
                self.emit_output(TripleRef::new(
                    // >   subject = base
                    oxrdf::NamedNodeRef::new_unchecked(&eval_context.base),
                    // >   predicate = http://www.w3.org/ns/rdfa#usesVocabulary
                    rdfa_vocab::USES_VOCABULARY,
                    // >   object = value from @vocab
                    &vocab,
                ));
                local.default_vocab = Some(vocab);
            }
        }

        // 3.
        // “Next, the current element is examined for IRI mappings and these are added to the local list of IRI mappings.
        //  Note that an IRI mapping will simply overwrite any current mapping in the list that has the same name;
        let xmlns_prefixes = el
            .attrs
            .iter()
            .filter(|(qn, _)| qn.prefix.as_deref() == Some("xmlns"))
            .map(|(qn, val)| (qn.local.as_ref(), val.as_ref()))
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
            let mut mappings = Rc::unwrap_or_clone(local.iri_mappings);
            // note that we do not ever set_default,
            // this would define a "no prefix" mapping
            // which is MUST NOT in RDFa

            // add XMLNS (must be first)
            for (prefix, iri) in xmlns_prefixes {
                mappings.add_prefix(prefix, iri).expect("TODO");
            }

            // add others (after XMLNS)
            for (prefix, iri) in prefixes {
                mappings.add_prefix(prefix, iri).expect("TODO");
            }

            local.iri_mappings = Rc::new(mappings);
        }

        // 4. Language
        // “The current element is also parsed for any language information,
        //  and if present, current language is set accordingly;
        if let Some(lang) = el.attr("xml:lang").or(el.attr("lang")) {
            if lang.is_empty() {
                local.current_language = None;
            } else {
                match LanguageIdentifier::from_str(lang) {
                    Ok(lang) => {
                        trace!("- current language is now: {lang}");
                        local.current_language = Some(Rc::new(lang));
                    }
                    Err(e) => {
                        (emit_warning)(
                            PGType::Warning,
                            format!("Invalid language identifier ({lang}): {e}"),
                        );
                    }
                }
            }
        }

        let property: Attr<Vec1<oxrdf::NamedNode>> =
            attr_many_pred("property", &|v| local.many_term_or_curie_or_absiri(v));

        let inlist = el.attr("inlist").is_some();
        let rel: Option<Vec<Relation>>;
        let rev: Option<Vec<Relation>>;

        let rel_dir = if inlist {
            Relation::List
        } else {
            Relation::Forward
        };
        let rev_dir = Relation::Reverse;

        if property.is_present() {
            // [html-rdfa] extension #7
            // > if the @property attribute and the @rel and/or @rev attribute exists
            // > on the same element, the non-CURIE and non-URI @rel and @rev values
            // > are ignored. If, after this, the value of @rel and/or @rev becomes empty,
            // > then the processor MUST act as if the respective attribute is not present.
            rel = match attr_many_pred("rel", &|v| local.many_curie_or_absiri(v)) {
                Attr::Missing | Attr::Empty => None,
                Attr::Value(v) => Some(v.into_iter().map(rel_dir).collect()),
            };
            rev = match attr_many_pred("rev", &|v| local.many_curie_or_absiri(v)) {
                Attr::Missing | Attr::Empty => None,
                Attr::Value(v) => Some(v.into_iter().map(rev_dir).collect()),
            };
        } else {
            rel = match attr_many_pred("rel", &|v| local.many_term_or_curie_or_absiri(v)) {
                Attr::Missing => None,
                Attr::Empty => Some(Vec::new()),
                Attr::Value(v) => Some(v.into_iter().map(rel_dir).collect()),
            };
            rev = match attr_many_pred("rev", &|v| local.many_term_or_curie_or_absiri(v)) {
                Attr::Missing => None,
                Attr::Empty => Some(Vec::new()),
                Attr::Value(v) => Some(v.into_iter().map(rev_dir).collect()),
            };
        }

        let relations = match (rel, rev) {
            (None, None) => None,
            (Some(rel), None) => Some(rel),
            (None, Some(rev)) => Some(rev),
            (Some(rel), Some(rev)) => {
                let mut rel = rel;
                rel.extend(rev);
                Some(rel)
            }
        };

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
                    oxrdf::NamedNode::new(eval_context.base.to_string() + "#" + id)
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
            let role_local = LocalScope {
                default_vocab: Some(oxrdf::NamedNode::new_unchecked(
                    "http://www.w3.org/1999/xhtml/vocab#".to_string(),
                )),
                ..local.clone()
            };

            // > Remember that @role values are defined using the datatype TERMorCURIEorAbsIRIs.
            // > An RDFa Processor will intepret these values using the rules for that that datatype
            // > as defined in [RDFA-CORE].
            for role in role_local.many_term_or_curie_or_absiri(role) {
                // > The predicate is the term `role` in the vocabulary
                // > defined at http://www.w3.org/1999/xhtml/vocab.
                self.emit_output(TripleRef::new(&role_subject, xhv_vocab::ROLE, &role));
            }
        }

        let content = el.attr("content");

        let type_of: Attr<Vec1<NamedOrBlankNode>> =
            attr_many("typeof", &|v| local.many_term_or_curie_or_absiri(v));

        let about: Attr<Rc<NamedOrBlankNode>> =
            attr1("about", &|v| local.safecuri_or_curie_or_iri(v)).map(Rc::new);
        let resource: Attr<Rc<NamedOrBlankNode>> =
            attr1("resource", &|x| local.safecuri_or_curie_or_iri(x)).map(Rc::new);

        let href: Attr<NamedNode> = attr_iri("href", &|v| local.attribute_iri(v));
        let src: Attr<NamedNode> = attr_iri("src", &|v| local.attribute_iri(v));

        let datatype: Attr<oxrdf::NamedOrBlankNode> =
            attr1("datatype", &|v| local.term_or_curie_or_absiri(v));

        // read from the "resource attributes"
        let resource_present = resource.is_present() || href.is_present() || src.is_present();
        let resource_value: Option<Rc<NamedOrBlankNode>> = resource
            .value()
            .cloned()
            .or_else(|| Some(Rc::new(href.into_value()?.into())))
            .or_else(|| Some(Rc::new(src.into_value()?.into())));

        //5.
        // “If the current element contains no @rel or @rev attribute,
        if relations.is_none() {
            // “then the next step is to establish a value for new subject.
            //  This step has two possible alternatives.
            //
            // 5.1
            // “If the current element contains the @property attribute, but does
            //  not contain either the @content or @datatype attributes, then
            if property.is_present() && content.is_none() && !datatype.is_present() {
                // > new subject is set to the resource obtained from the first match from the following rule:
                // >
                // > - by using the resource from @about, if present,
                if let Some(about) = about.value() {
                    trace!("- Using @about as new subject");
                    //  obtained according to the section on CURIE and IRI Processing;
                    local.new_subject = Some(about.clone());
                }
                // > - otherwise, if the element is the root element of the document,
                else if is_root_element {
                    //  then act as if there is an empty @about present,
                    //  and process it according to the rule for @about, above;
                    trace!("- Using empty @about as new subject");
                    local.new_subject = Some(Rc::new(local.empty_curie().into()));
                }
                // > - otherwise, if parent object is present, new subject is set to the value of parent object.
                else if eval_context.parent_object.is_some() {
                    trace!(
                        "- Using parent object as new subject: {}",
                        eval_context.parent_object.as_ref().unwrap()
                    );
                    local.new_subject = eval_context.parent_object.clone();
                }

                // “If @typeof is present then typed resource is set to the resource obtained from
                //  the first match from the following rules:
                if type_of.is_present() {
                    // “by using the resource from @about, if present,
                    if let Some(about) = about.value() {
                        //  obtained according to the section on CURIE and IRI Processing;
                        trace!("- Using @about as typed resource");
                        local.typed_resource = Some(about.clone());
                    }
                    // “otherwise, if the element is the root element of the document,
                    else if is_root_element {
                        // “then act as if there is an empty @about present
                        //  and process it according to the previous rule;
                        local.typed_resource = Some(Rc::new(local.empty_curie().into()));
                    }
                    // “otherwise,
                    else {
                        let typed_resource =
                            // “by using the resource from @resource, if present, obtained according to the section on CURIE and IRI Processing;
                            // “otherwise, by using the IRI from @href, if present, obtained according to the section on CURIE and IRI Processing;
                            // “otherwise, by using the IRI from @src, if present, obtained according to the section on CURIE and IRI Processing;
                            if let Some(resource) = &resource_value {
                                 resource.clone()
                            }
                            // “otherwise, the value of typed resource is set to a newly created bnode.
                            else {
                                trace!("- Using new blank node as typed resource");
                                Rc::new(oxrdf::BlankNode::default().into())
                            };

                        // “The value of the current object resource is then set to the value of typed resource.
                        local.typed_resource = Some(typed_resource.clone());

                        // “The value of the current object resource is then set to the value of typed resource.
                        local.current_object_resource = Some(typed_resource);
                    }
                }
            }
            // 5.2: “otherwise:
            else {
                // [html-rdfa] extension #8
                let is_head_or_body = el.name() == "head" || el.name() == "body";

                // > If the element contains an @about, @href, @src, or @resource attribute,
                // > new subject is set to the resource obtained as follows:
                if about.is_present() || resource_present {
                    if let Some(about) = about.value() {
                        // > by using the resource from @about, if present,
                        // > obtained according to the section on CURIE and IRI Processing;
                        trace!("- Using @about as new subject");
                        local.new_subject = Some(about.clone())
                    } else if let Some(resource) = &resource_value {
                        // “otherwise, by using the resource from @resource, if present, obtained according to the section on CURIE and IRI Processing;
                        // “otherwise, by using the IRI from @href, if present, obtained according to the section on CURIE and IRI Processing;
                        // “otherwise, by using the IRI from @src, if present, obtained according to the section on CURIE and IRI Processing.
                        trace!("- Using @resource/@href/@src as new subject");
                        local.new_subject = Some(resource.clone());
                    }
                }

                // [html-rdfa] extension #8
                if local.new_subject.is_none() && is_head_or_body {
                    // > if no IRI is provided by a resource attribute
                    // > (e.g., @about, @href, @resource, or @src), then
                    // > first check to see if the element is the head or
                    // > body element. If it is, then set new subject to parent object.
                    trace!(
                        "- [head/body] using parent object as new subject: {}",
                        eval_context.parent_object.as_ref().unwrap()
                    );
                    local.new_subject = eval_context.parent_object.clone();
                }

                // > otherwise, if no resource is provided by a resource attribute,
                // > then the first match from the following rules will apply:
                if local.new_subject.is_none() {
                    // > if the element is the root element of the document,
                    if is_root_element {
                        // > then act as if there is an empty @about present,
                        // > and process it according to the rule for @about, above;
                        local.new_subject = Some(Rc::new(local.empty_curie().into()));
                        trace!(
                            "- Using empty CURIE as new subject (root element): {}",
                            local.new_subject.as_ref().unwrap()
                        );
                    }
                    // > otherwise, if @typeof is present,
                    else if type_of.is_present() {
                        // > then new subject is set to be a newly created bnode;
                        local.new_subject = Some(Rc::new(oxrdf::BlankNode::default().into()));
                        trace!(
                            "- Using blank node as new subject (@typeof present): {}",
                            local.new_subject.as_ref().unwrap()
                        );
                    }
                    // > otherwise, if parent object is present,
                    else if eval_context.parent_object.is_some() {
                        // “new subject is set to the value of parent object.
                        local.new_subject = eval_context.parent_object.clone();
                        trace!(
                            "- Using parent object as new subject: {}",
                            local.new_subject.as_ref().unwrap()
                        );

                        // “Additionally, if @property is not present then the skip element flag is set to 'true'.
                        if !property.is_present() {
                            trace!("- Skip element set to 'true' (no @property).");
                            local.skip_element = true;
                        }
                    }

                    debug_assert!(local.new_subject.is_some());
                }

                // “Finally, if @typeof is present, set the typed resource to the value of new subject.
                if type_of.is_present() {
                    local.typed_resource = local.new_subject.clone();
                }
            }
        }
        // 6.
        else {
            // > If the current element does contain a @rel or @rev attribute,
            // > then the next step is to establish both a value for new subject
            // > and a value for current object resource:
            //
            // > new subject is set to the resource obtained from the first match from the following rules:
            // > by using the resource from @about, if present, obtained according to the section on CURIE and IRI Processing;
            if let Some(about) = about.value() {
                trace!("- Using @about as new subject: {:?}", about);
                local.new_subject = Some(about.clone());

                //TODO: check inside/outside parent if
                // “if the @typeof attribute is present, set typed resource to new subject.
                if type_of.is_present() {
                    local.typed_resource = local.new_subject.clone();
                    trace!(
                        "- Using new subject as typed resource (@typeof present): {:?}",
                        local.typed_resource.as_ref()
                    );
                }
            }

            // “If no resource is provided then the first match from the following rules will apply:
            if local.new_subject.is_none() {
                // “if the element is the root element of the document
                if is_root_element {
                    // “then act as if there is an empty @about present,
                    //  and process it according to the rule for @about, above;
                    local.new_subject = Some(Rc::new(local.empty_curie().into()));
                    trace!(
                        "- Using empty CURIE as new subject (root element): {}",
                        local.new_subject.as_ref().unwrap()
                    );
                } else {
                    // ”otherwise, if parent object is present, new subject is set to that.
                    local.new_subject = eval_context.parent_object.clone();
                    trace!(
                        "- Using parent object as new subject: {}",
                        local.new_subject.as_ref().unwrap()
                    );
                }
            }

            // > Then the current object resource is set to the resource obtained from the first match from the following rules:
            if let Some(resource) = resource_value.as_deref() {
                // “by using the resource from @resource, if present, obtained according to the section on CURIE and IRI Processing;
                //  otherwise, by using the IRI from @href, if present, obtained according to the section on CURIE and IRI Processing;
                //  otherwise, by using the IRI from @src, if present, obtained according to the section on CURIE and IRI Processing;
                local.current_object_resource = Some(resource.clone().into());
                trace!(
                    "- Using @resource/@href/@src as current object resource: {:?}",
                    local.current_object_resource
                );
            }
            // “otherwise, if @typeof is present and @about is not, use a newly created bnode.
            else if type_of.is_present() && !about.is_present() {
                local.current_object_resource = Some(Rc::new(oxrdf::BlankNode::default().into()));
                trace!(
                    "- Using blank node as current object resource (@typeof present): {}",
                    local.current_object_resource.as_ref().unwrap()
                );
            }

            // “If @typeof is present and @about is not,
            if type_of.is_present() && !about.is_present() {
                // “set typed resource to current object resource.
                local.typed_resource = local.current_object_resource.clone();
                trace!(
                    "- Using current object resource as typed resource (@typeof but no @about): {}",
                    local.typed_resource.as_ref().unwrap()
                );
            }

            // “Note that final value of the current object resource will either be null
            //  (from initialization) or a full IRI or bnode.
            // NB: this is guaranteed by the types
            debug_assert!(local.new_subject.is_some());
        }

        debug_assert!(local.new_subject.is_some());

        // 7.
        // “If in any of the previous steps a typed resource was set to a non-null value,
        //  it is now used to provide a subject for type values;
        if let Some(typed_resource) = local.typed_resource.as_deref() {
            // “One or more 'types' for the typed resource can be set by using @typeof.
            //  If present, the attribute may contain one or more IRIs, obtained according
            //  to the section on CURIE and IRI Processing, each of which is used to generate
            //  a triple as follows:
            if let Some(type_of) = type_of.value() {
                for type_iri in type_of {
                    self.emit_output(TripleRef::new(
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

        // 8.
        // “If in any of the previous steps a new subject
        //  was set to a non-null value different from the parent object;
        if let Some(ns) = &local.new_subject {
            if Some(ns) != eval_context.parent_object.as_ref() {
                // “The list mapping taken from the evaluation context is set to a new, empty mapping.
                trace!("- Setting new list mapping");
                local.list_mappings = Default::default();
            }
        }

        // 9.
        // “If in any of the previous steps a current object resource was set to a non-null value,
        //  it is now used to generate triples and add entries to the local list mapping:
        if let Some(current_object_resource) = local.current_object_resource.as_deref() {
            trace!("  - current object resource: {}", current_object_resource);
            if let Some(relations) = &relations {
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
                            local
                                .list_mappings
                                .borrow_mut()
                                .insert_value(predicate.clone(), term.clone());
                        }
                        // > If present, @rel may contain one or more resources, obtained according
                        // > to the section on CURIE and IRI Processing each of which is used to
                        // > generate a triple as follows:
                        Relation::Forward(predicate) => {
                            self.emit_output(TripleRef::new(
                                //  subject = new subject
                                local.new_subject.as_deref().unwrap(),
                                //  predicate = full IRI
                                predicate,
                                //  object = current object resource
                                current_object_resource.as_ref(),
                            ));
                        }
                        Relation::Reverse(predicate) => {
                            // “If present, @rev may contain one or more resources,
                            //  obtained according to the section on CURIE and IRI Processing
                            // each of which is used to generate a triple as follows:
                            self.emit_output(TripleRef::new(
                                //  subject = current object resource
                                current_object_resource.as_ref(),
                                //  predicate = full IRI
                                predicate,
                                //  object = new subject
                                local.new_subject.as_deref().unwrap(),
                            ));
                        }
                    }
                }
            }
        }
        // [rdfa-core] 7.5: 10.
        // > If however current object resource was set to null, but there are predicates present,
        // > then they must be stored as incomplete triples, pending the discovery of a subject
        // > that can be used as the object.
        else if let Some(revrel) = &relations {
            debug_assert!(local.current_object_resource.is_none());
            trace!(
                "- current object resource is null, storing incomplete triples against new blank node"
            );
            // > Also, current object resource should be set to a newly created bnode
            // > (so that the incomplete triples have a subject to connect to if they are ultimately turned into triples);
            local.current_object_resource = Some(Rc::new(oxrdf::BlankNode::default().into()));
            for relation in revrel {
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

            trace!("- incomplete triples: {:?}", local.incomplete_triples);
        }

        // 11.
        // “The next step of the iteration is to establish any current property value;
        //  Predicates for the current property value can be set by using @property.
        //  If present, one or more resources are obtained according to the section on
        //  CURIE and IRI Processing, and then the actual literal value is obtained as follows:
        if let Some(properties) = property.into_value() {
            let lang = local.current_language.as_ref().map(|l| l.to_string());
            let mut otherwise_datatype: Option<NamedNodeRef> = None;
            let content_val: Cow<str> = if let Some(content) = content {
                content.into()
            } else {
                // [html-rdfa] extension #9 & #10
                let value = el.attr("datetime").map(Cow::Borrowed).or_else(|| {
                    if el.name() == "time" {
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

            let serialized: String;

            let current_property_value: oxrdf::Term = match &datatype {
                Attr::Empty => {
                    trace!("- Empty datatype, using plain literal");
                    // “otherwise, as a plain literal if @datatype is present but has an empty value
                    //  according to the section on CURIE and IRI Processing. The actual literal is
                    //  either the value of @content (if present) or a string created by concatenating
                    //  the value of all descendant text nodes, of the current element in turn.
                    if let Some(lang) = &lang {
                        oxrdf::LiteralRef::new_language_tagged_literal_unchecked(&content_val, lang)
                            .into()
                    } else {
                        oxrdf::LiteralRef::new_simple_literal(&content_val).into()
                    }
                }
                Attr::Value(NamedOrBlankNode::NamedNode(datatype)) => {
                    if datatype.as_str() == oxrdf::vocab::rdf::XML_LITERAL.as_str() {
                        // “otherwise, as an XML literal if @datatype is present and is set to
                        //  XMLLiteral in the vocabulary http://www.w3.org/1999/02/22-rdf-syntax-ns#.
                        // “The value of the XML literal is a string created by serializing to text,
                        //  all nodes that are descendants of the current element, i.e., not including
                        //  the element itself, and giving it a datatype of XMLLiteral in the vocabulary
                        //  http://www.w3.org/1999/02/22-rdf-syntax-ns#. The format of the resulting
                        //  serialized content is as defined in Exclusive XML Canonicalization Version 1.0 [XML-EXC-C14N].
                        serialized = element.inner_html();
                        oxrdf::LiteralRef::new_typed_literal(&serialized, datatype).into()
                        // TODO: incorrect, needs to be c14n'd
                    } else if datatype.as_str() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#HTML"
                    {
                        serialized = element.inner_html();
                        oxrdf::LiteralRef::new_typed_literal(&serialized, datatype).into()
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
                Attr::Value(NamedOrBlankNode::BlankNode(blank_node)) => todo!(),
                Attr::Missing => {
                    if let Some(otherwise_datatype) = otherwise_datatype {
                        // [html-rdfa] extension #9
                        // “Otherwise, if the value of @datetime lexically matches a valid xsd:date, xsd:time,
                        //  xsd:dateTime, xsd:duration, xsd:gYear, or xsd:gYearMonth a typed literal must be generated,
                        //  with its datatype set to the matching xsd datatype.
                        oxrdf::LiteralRef::new_typed_literal(&content_val, otherwise_datatype)
                            .into()
                    } else {
                        // “otherwise, as a plain literal using the value of @content if @content is present.
                        if let Some(content) = content {
                            if let Some(lang) = &lang {
                                oxrdf::LiteralRef::new_language_tagged_literal_unchecked(
                                    content, lang,
                                )
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
                            && content.is_none()
                            && resource_value.is_some()
                        {
                            resource_value.as_deref().unwrap().clone().into()
                        }
                        // otherwise, if @typeof is present and @about is not, the value of typed resource.
                        else if type_of.is_present() && !about.is_present() {
                            Rc::unwrap_or_clone(local.typed_resource.unwrap()).into()
                        }
                        // otherwise as a plain literal.
                        else if let Some(lang) = &lang {
                            oxrdf::LiteralRef::new_language_tagged_literal_unchecked(
                                &content_val,
                                lang,
                            )
                            .into()
                        } else {
                            oxrdf::LiteralRef::new_simple_literal(&content_val).into()
                        }
                    }
                }
            };

            // “The current property value is then used with each predicate as follows:
            //
            // “If the element also includes the @inlist attribute, the current property
            //  value is added to the local list mapping as follows:
            if inlist {
                // “if the local list mapping does not contain a list associated with the predicate IRI,
                //  instantiate a new list and add to local list mappings
                //
                //  add the current property value to the list associated with the predicate IRI in the local list mapping
                let term: Rc<oxrdf::Term> = Rc::new(current_property_value);
                for property in properties {
                    local
                        .list_mappings
                        .borrow_mut()
                        .insert_value(property, term.clone());
                }
            } else {
                // “Otherwise the current property value is used to generate a triple as follows:
                if let Some(sub) = local.new_subject.as_deref() {
                    for property in properties {
                        self.emit_output(TripleRef::new(
                            // subject = new subject
                            sub,
                            // predicate = full IRI
                            property.as_ref(),
                            // object = current property value
                            &current_property_value,
                        ));
                    }
                }
            };
        }

        // 12.
        // “If the skip element flag is 'false', and new subject was set to a non-null value,
        //  then any incomplete triples within the current context should be completed:
        if !local.skip_element {
            if let Some(new_subject) = &local.new_subject {
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
                            self.emit_output(TripleRef::new(
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
                            self.emit_output(TripleRef::new(
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
        }

        // 13.
        // “Next, all elements that are children of the current element are processed
        //  using the rules described here, using a new evaluation context, initialized as follows:
        //
        //  If the skip element flag is 'true' then the new evaluation context is a copy of the current
        //  context that was passed in to this level of processing, with the language and list of IRI
        //  mappings values replaced with the local values;
        if local.skip_element {
            Ok(EvaluationContext {
                language: local.current_language,
                iri_mappings: local.iri_mappings,
                // ERRATA: this also needs to be copied
                default_vocab: local.default_vocab,
                ..eval_context.clone()
            })
        } else {
            // “ Otherwise, the values are:
            Ok(EvaluationContext {
                // internal
                empty_bnode: eval_context.empty_bnode.clone(),
                // > the base is set to the base value of the current evaluation context;
                base: eval_context.base.clone(),
                // “ the parent subject is set to the value of new subject, if non-null,
                //   or the value of the parent subject of the current evaluation context;
                parent_subject: local
                    .new_subject
                    .clone()
                    .unwrap_or_else(|| eval_context.parent_subject.clone()),
                // “ the parent object is set to value of current object resource, if non-null,
                //   or the value of new subject, if non-null, or the value of the parent subject
                //   of the current evaluation context;
                parent_object: Some(
                    local
                        .current_object_resource
                        .as_ref()
                        .or(local.new_subject.as_ref())
                        .cloned()
                        .unwrap_or_else(|| eval_context.parent_subject.clone()),
                ),
                // “ the list of IRI mappings is set to the local list of IRI mappings;
                iri_mappings: local.iri_mappings,
                // “ the list of incomplete triples is set to the local list of incomplete triples;
                incomplete_triples: local.incomplete_triples,
                // “ the list mapping is set to the local list mapping;
                list_mapping: local.list_mappings,
                // “ language is set to the value of current language.
                language: local.current_language,
                // “ the default vocabulary is set to the value of the local default vocabulary.
                default_vocab: local.default_vocab,
                // ERRATA: undocumented, but assumed
                term_mappings: local.term_mappings,
            })
        }
    }

    fn to_predicate(&self, name: &str, v: NamedOrBlankNode) -> Option<oxrdf::NamedNode> {
        match v {
            NamedOrBlankNode::NamedNode(x) => Some(x),
            NamedOrBlankNode::BlankNode(b) => {
                let mut pg = self.processor_graph.borrow_mut();
                emit_processor(
                    &mut pg,
                    PGType::Warning,
                    &format!("@{} cannot refer to a bnode: [{}]", name, b),
                );
                None
            }
        }
    }
}
