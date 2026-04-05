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

use std::{cell::RefCell, rc::Rc};

use itertools::Itertools;
use oxiri::Iri;
use oxrdf::{Graph, NamedNode, NamedOrBlankNode, NamedOrBlankNodeRef, vocab::rdf};
use tracing::trace;

use crate::{
    algorithms::VocabularyResolver,
    element_processor::{ElementProcessor, IncompleteTriple},
    graphs::{OutputGraph, PGClass, ProcessorGraph},
    host_language::{Document, Element, HostLanguage},
    resolver::{Resolver, ResolverConstantData},
    vocab::xhtml,
};

pub mod algorithms;
pub mod errors;
pub mod graphs;
pub mod host_language;
pub mod initial_context;
pub mod vocab;

mod element_processor;
mod lists;
mod resolver;

use lists::ListMapping;

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
    parse(input, base, options, output_graph, processor_graph)?;
    if prop_copy {
        algorithms::property_copying(output_graph);
    }
    if let Some(resolver) = resolver {
        algorithms::vocabulary_expansion(output_graph, base, resolver.as_ref());
    }
    Ok(())
}

/// Only parses the document and produces triples into the given graphs.
///
/// This never performs property-copying or vocabulary-expansion.
///
/// # Errors
/// This function only returns an error if the host language encounters one
/// during parsing – all other errors or warnings produced are
/// generated as triples into the processor graph.
pub fn parse<H: HostLanguage>(
    input: &str,
    document_iri: Iri<&str>,
    options: Options<H>,
    output_graph: &mut impl OutputGraph,
    processor_graph: &mut impl ProcessorGraph,
) -> Result<(), H::ParseError> {
    let doc = options
        .host_language
        .parse_document(input, document_iri, processor_graph)?;

    let base = match options.host_language.establish_base(&doc) {
        Ok(Some(found_base)) => found_base,
        Ok(None) => {
            trace!(%document_iri, "no base override found in document, using provided document IRI");
            document_iri
        }
        Err(e) => {
            processor_graph.emit_message(
                PGClass::DocumentError,
                &e.to_string(),
                Some(&document_iri),
            );
            document_iri
        }
    };

    let resolver_const = ResolverConstantData::new(base, &options.host_language);

    let eval_context = EvaluationContext::new(&resolver_const);
    let mut proc = RDFaProcessor::new(options, output_graph, processor_graph);
    proc.run(eval_context, &doc);
    Ok(())
}

// > During processing, each rule is applied using information provided by an evaluation context.
// > An initial context is created when processing begins. That context has the following members:
#[derive(Clone)]
struct EvaluationContext<'r> {
    resolver: Resolver<'r>,

    // Spec text:
    // > The parent subject. The initial value will be the same as the initial value of base,
    // > but it will usually change during the course of processing.
    //
    // > The parent object. In some situations the object of a statement becomes the subject
    // > of any nested statements, and this member is used to convey this value. Note that this
    // > value may be a bnode, since in some situations a number of nested statements are grouped
    // > together on one bnode. This means that the bnode must be set in the containing statement and passed down.
    //
    // Here we only need one field which tracks the parent/current subject.
    subject: Rc<oxrdf::NamedOrBlankNode>,

    // > A list of incomplete triples. A triple can be incomplete when no object resource is provided
    // > alongside a predicate that requires a resource (i.e., @rel or @rev). The triples can be completed
    // > when a resource becomes available, which will be when the next subject is specified
    // > (part of the process called chaining).
    incomplete_triples: Vec<IncompleteTriple>,

    // > A list mapping that associates IRIs with lists.
    list_mapping: Rc<RefCell<ListMapping>>,
}

impl<'r> EvaluationContext<'r> {
    fn subject_ref(&self) -> NamedOrBlankNodeRef<'_> {
        self.subject.as_ref().as_ref()
    }

    fn new_subject(
        self: &mut Rc<EvaluationContext<'r>>,
        new_subject: Rc<NamedOrBlankNode>,
        source: &str,
    ) {
        trace!(%new_subject, source, "new subject established");
        let me = Rc::make_mut(self);
        if new_subject != me.subject {
            // > If in any of the previous steps a new subject was set to a non-null
            // > value different from the parent object [=subject], the list mapping taken from
            // > the evaluation context is set to a new, empty mapping.
            me.list_mapping = Rc::default();
        }
        me.subject = new_subject;
    }
}

impl<'r> EvaluationContext<'r> {
    fn new(resolver_data: &'r ResolverConstantData) -> Self {
        // Note that we deviate from the algorithm described in the specification here
        // in that we start parent_object with the same value as parent_subject,
        // instead of parent_object starting off as null.
        //
        // This does not have any effect (as far as I can tell)
        // on the correctness of the algorithm, since after processing
        // the root element, the parent_object is _always_ set,
        // and the algorithm specifies  special handling for the root element
        // which produces the same value as we specify here!
        //
        // There are several other benefits to doing so:
        // - we get tighter types (no Option)
        // - some dead code cases get removed
        // - we can remove/simplify some of the checks for
        //   the root element (in 5.1 and 6)
        let root: Rc<NamedOrBlankNode> =
            Rc::new(NamedNode::from(resolver_data.base.clone()).into());
        Self {
            subject: root,
            list_mapping: Rc::default(),
            incomplete_triples: Vec::default(),
            resolver: Resolver::new(resolver_data),
        }
    }
}

struct RDFaProcessor<'a, H, O, P> {
    _options: Options<H>,
    output: &'a mut O,
    processor: &'a mut P,
}

impl<'a, H: HostLanguage, O: OutputGraph, P: ProcessorGraph> RDFaProcessor<'a, H, O, P> {
    fn new(options: Options<H>, output_graph: &'a mut O, processor_graph: &'a mut P) -> Self {
        Self {
            _options: options,
            output: output_graph,
            processor: processor_graph,
        }
    }

    fn run(&mut self, eval_context: EvaluationContext, html: &H::DocumentType<'_>) {
        enum S<'r, E: host_language::Element> {
            Child(E, Rc<EvaluationContext<'r>>, tracing::Span),
            OutputList(Rc<NamedOrBlankNode>, Rc<RefCell<ListMapping>>),
        }

        let Some(root) = html.root_element() else {
            return;
        };

        let mut stack = vec![S::Child(
            root,
            Rc::new(eval_context),
            tracing::trace_span!("element", name = "html"),
        )];

        while let Some(stack_item) = stack.pop() {
            match stack_item {
                S::Child(element, eval_context, parent_span) => {
                    let _span = parent_span.entered();

                    let new_ctx = self.process_element(eval_context, &element);
                    if element.has_children() {
                        // mark that we will need to output the list later
                        stack.push(S::OutputList(
                            new_ctx.subject.clone(),
                            new_ctx.list_mapping.clone(),
                        ));

                        for child in element.child_elements().rev() {
                            let span = tracing::trace_span!("element", tag_name = child.tag_name());
                            stack.push(S::Child(child, new_ctx.clone(), span));
                        }
                    } else {
                        // otherwise try to output the list now (if any)
                        if let Ok(ctx) = Rc::try_unwrap(new_ctx) {
                            self.emit_lists(ctx.subject, ctx.list_mapping);
                        } else {
                            // we might not be able to unwrap it if the context was shared
                            // if this happens, then it cannot have had a new list_mapping set
                        }
                    }
                }
                S::OutputList(subject, list_mapping) => {
                    self.emit_lists(subject, list_mapping);
                }
            }
        }
    }

    fn emit_lists(
        &mut self,
        subject: Rc<NamedOrBlankNode>,
        list_mapping: Rc<RefCell<ListMapping>>,
    ) {
        // [rdfa-core] 7.5: 14.
        // > Finally, if there is one or more mapping in the local list mapping,
        // > list triples are generated as follows:
        // >
        // > For each IRI in the local list mapping, if the equivalent list does not
        // > exist in the evaluation context, indicating that the list was originally
        // > instantiated on the current element, use the list as follows:
        if let Ok(list_mapping) = Rc::try_unwrap(list_mapping) {
            // NB: if we successfully unwrap it, that means the list mapping
            //     is complete
            for (iri, list) in list_mapping.into_inner().lists {
                // > If there are zero items in the list associated with the IRI,
                // > generate the following triple:
                let mut next: NamedOrBlankNode = rdf::NIL.into();
                for item in list.borrow().iter().rev() {
                    let me = oxrdf::BlankNode::default();
                    self.output
                        .emit(me.as_ref().into(), rdf::FIRST, item.as_ref().into());
                    self.output
                        .emit(me.as_ref().into(), rdf::REST, next.as_ref().into());
                    next = me.into();
                }
                self.output
                    .emit(subject.as_ref().into(), iri.as_ref(), next.as_ref().into());
            }
        }
    }

    /// Updates this resolver for a specific element.
    ///
    /// Applies vocabulary, IRI mapping, and language updates
    /// from the element's attributes.
    ///
    /// Note that this only makes a copy of the evaluation context if it has to change.
    pub fn update_resolver(&mut self, el: &impl Element, eval_context: &mut Rc<EvaluationContext>) {
        // [rdfa-core] 7.5: 2.
        self.update_vocab(el, eval_context);
        // [rdfa-core] 7.5: 3.
        self.update_iri_mappings(el, eval_context);
        // [rdfa-core] 7.5: 4.
        self.update_current_language(el, eval_context);
    }

    fn update_vocab(&mut self, el: &impl Element, eval_context: &mut Rc<EvaluationContext>) {
        // “the local default vocabulary is set to the default vocabulary from the evaluation context.
        // (do nothing)

        // [rdfa-core] 7.5: 2.
        // > Next the current element is examined for any change to the default vocabulary via @vocab.
        if let Some(vocab) = el.attr("vocab") {
            let ctx = Rc::make_mut(eval_context);
            ctx.resolver
                .update_vocab(vocab, self.processor, self.output);
        }
    }

    fn update_iri_mappings(&mut self, el: &impl Element, eval_context: &mut Rc<EvaluationContext>) {
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
        // “2. For each [Attr] in the [Node.attributes] list that has a [namespace prefix] value of null
        //     and a [local name] that starts with @xmlns:, create an [IRI mapping] by storing the [local name]
        //     part with the @xmlns: characters removed as the value to be mapped, and the [Node.nodeValue] as
        //     the value to map.
        let xmlns_prefixes = el.namespaces().collect::<Vec<_>>();

        let prefixes = el
            .attr("prefix")
            .map(|x| {
                x.split_ascii_whitespace()
                    .tuples()
                    .filter_map(|(prefix, value)| {
                        // TODO: produce an error if no ':'
                        prefix.strip_suffix(':').map(|prefix| (prefix, value))
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        if !xmlns_prefixes.is_empty() || !prefixes.is_empty() {
            let ec = Rc::make_mut(eval_context);
            ec.resolver
                .update_iri_mappings(xmlns_prefixes, prefixes, self.processor);
        }
    }

    fn update_current_language(
        &mut self,
        el: &impl Element,
        eval_context: &mut Rc<EvaluationContext>,
    ) {
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
        if let Some(lang) = el.xml_attr("lang").or(el.attr("lang")) {
            let ec = Rc::make_mut(eval_context);
            ec.resolver.update_current_language(lang, self.processor);
        }
    }

    fn process_role(&mut self, el: &impl Element, ctx: &mut Rc<EvaluationContext>) {
        // [role-attribute]
        if let Some(role) = el.attr("role") {
            let role_subject: oxrdf::NamedOrBlankNode = if let Some(id) = el.attr("id") {
                const FRAGMENT_SET: percent_encoding::AsciiSet = percent_encoding::CONTROLS
                    .add(b' ')
                    .add(b'"')
                    .add(b'<')
                    .add(b'>')
                    .add(b'`');
                let encoded_id = percent_encoding::utf8_percent_encode(id, &FRAGMENT_SET);
                oxrdf::NamedNode::new(format!("{}#{}", ctx.resolver.base().as_str(), encoded_id))
                    .unwrap()
                    .into()
            } else {
                oxrdf::BlankNode::default().into()
            };

            let ctx = Rc::make_mut(ctx);
            let old = ctx
                .resolver
                .set_default_vocab(Some(Rc::new(xhtml::VOCAB.into())));

            let (roles, errors) = ctx.resolver.many_term_or_curie_or_absiri(role);
            for role in roles {
                self.output
                    .emit(role_subject.as_ref(), xhtml::ROLE, role.as_ref().into());
            }
            for error in errors {
                self.processor.report_error(&error);
            }
            ctx.resolver.set_default_vocab(old);
        }
    }

    // TOD: implement https://www.w3.org/TR/html-rdfa/#h-additional-rules
    fn process_element<'r>(
        &mut self,
        mut eval_context: Rc<EvaluationContext<'r>>,
        element: &impl Element,
    ) -> Rc<EvaluationContext<'r>> {
        trace!(element = ?element, "processing element");

        // [rdfa-core] 7.5: 2, 3, 4.
        self.update_resolver(element, &mut eval_context);

        // [rdfa-html] this doesn't rely on anything else
        self.process_role(element, &mut eval_context);

        // [rdfa-core] 7.5: 1, 2, 3, 4.
        let mut local = ElementProcessor::new(&mut eval_context, element, self.processor);

        // [rdfa-core] 7.5: 6, 8.
        local.process(self.output);

        // 13.
        // > Next, all elements that are children of the current element are processed
        // > using the rules described here, using a new evaluation context, initialized as follows:
        // >
        // > If the skip element flag is 'true' then the new evaluation context is a copy of the current
        // > context that was passed in to this level of processing, with the language and list of IRI
        // > mappings values replaced with the local values;
        // >
        // > the base is set to the base value of the current evaluation context;
        // > language is set to the value of current language.
        // > the default vocabulary is set to the value of the local default vocabulary.
        // > the list of IRI mappings is set to the local list of IRI mappings;
        // ERRATA: in addition to current_language and iri_mappings,
        // we copy default_vocab and term_mappings (both unspecified)
        // default_vocab at least seems to be required to pass test suite
        // > Otherwise, the values are:
        // > the parent object is set to value of current object resource, if non-null,
        // > or the value of new subject, if non-null, or the value of the parent subject
        // > of the current evaluation context;
        // > the parent subject is set to the value of new subject, if non-null,
        // > or the value of the parent subject of the current evaluation context;
        // > the list of incomplete triples is set to the local list of incomplete triples;
        // > the list mapping is set to the local list mapping;
        eval_context
    }
}
