use std::{borrow::Cow, rc::Rc, str::FromStr};

use mitsein::{EmptyError, iter1::FromIterator1, vec1::Vec1};
use oxilangtag::LanguageTag;
use oxrdf::{
    BlankNode, Literal, LiteralRef, NamedNode, NamedNodeRef, NamedOrBlankNode, NamedOrBlankNodeRef,
    TermRef,
    vocab::{rdf, xsd},
};
use tracing::trace;

use crate::{
    EvaluationContext,
    graphs::{OutputGraph, ProcessorGraph},
    host_language::Element,
    lists::SharedList,
};

/// A type to represent attribute lookups.
/// In several places we must distinguish between
/// an attribute that was present but empty
/// and one that was not present at all.
pub enum Attr<T> {
    Missing,
    Empty,
    Value(T),
}

impl<T> Attr<T> {
    pub fn map_missing_maybe(self, f: impl FnOnce() -> Option<T>) -> Attr<T> {
        match self {
            Attr::Missing => match f() {
                Some(v) => Attr::Value(v),
                None => Attr::Missing,
            },
            Attr::Empty => Attr::Empty,
            Attr::Value(v) => Attr::Value(v),
        }
    }

    pub fn is_present(&self) -> bool {
        !matches!(self, Attr::Missing)
    }

    pub fn into_value_maybe(self) -> Option<T> {
        match self {
            Attr::Value(v) => Some(v),
            Attr::Empty | Attr::Missing => None,
        }
    }
}

impl<T> Attr<mitsein::prelude::Vec1<T>> {
    pub fn into_value(self) -> Option<Vec<T>> {
        match self {
            Attr::Value(v) => Some(v.into()),
            Attr::Empty => Some(Vec::new()),
            Attr::Missing => None,
        }
    }
}

impl Attr<NamedNode> {
    // unfortunately NamedNode does not implement AsRef
    // since AsRef requires an external ref
    pub fn as_ref(&self) -> Attr<NamedNodeRef<'_>> {
        match self {
            Attr::Missing => Attr::Missing,
            Attr::Empty => Attr::Empty,
            Attr::Value(v) => Attr::Value(v.as_ref()),
        }
    }
}

enum Relation {
    Forward(oxrdf::NamedNode),
    Reverse(oxrdf::NamedNode),
    List(oxrdf::NamedNode),
}

#[derive(Clone, Debug)]
pub(crate) enum IncompleteTriple {
    List(Rc<SharedList>),
    Forward {
        subject: Rc<NamedOrBlankNode>,
        predicate: Rc<NamedNode>,
    },
    Reverse {
        predicate: Rc<NamedNode>,
        object: Rc<NamedOrBlankNode>,
    },
}

pub struct ElementProcessor<'e, 'ec, 'r, 'pg, E, PG> {
    ctx: &'ec mut Rc<EvaluationContext<'r>>,
    element: &'e E,
    processor_graph: &'pg mut PG,

    is_root_element: bool,
    in_list: bool,
    datatype: Attr<NamedNode>,
    has_content: bool,
}

// Attribute resolution methods
impl<'e, E: Element, PG: ProcessorGraph> ElementProcessor<'e, '_, '_, '_, E, PG> {
    fn tag_name(&self) -> &str {
        self.element.tag_name()
    }

    fn language(&self) -> Option<&str> {
        self.ctx
            .resolver
            .language
            .as_deref()
            .map(LanguageTag::as_str)
    }

    fn attr_iri(&self, name: &str) -> Attr<NamedNode> {
        match self.element.attr(name) {
            None => Attr::Missing,
            Some(v) => match self.ctx.resolver.resolve_attribute_iri(v) {
                None => Attr::Empty,
                Some(v) => Attr::Value(v),
            },
        }
    }

    fn attr_1_term_or_curie_or_absiri(&mut self, name: &str) -> Attr<NamedNode> {
        match self.element.attr(name) {
            None => Attr::Missing,
            Some(v) => match self.ctx.resolver.term_or_curie_or_absiri(v) {
                Err(err) => {
                    self.processor_graph.report_error(&err);
                    Attr::Empty
                }
                // TODO: produce a warning when blanknodes are dropped
                Ok(NamedOrBlankNode::BlankNode(_)) => Attr::Empty,
                Ok(NamedOrBlankNode::NamedNode(v)) => Attr::Value(v),
            },
        }
    }

    fn attr_1_safecurie_or_curie_or_iri(&mut self, name: &str) -> Attr<NamedOrBlankNode> {
        match self.element.attr(name) {
            None => Attr::Missing,
            Some(v) => match self.ctx.resolver.safecurie_or_curie_or_iri(v) {
                Err(e) => {
                    self.processor_graph.report_error(&e);
                    Attr::Empty
                }
                Ok(v) => Attr::Value(v),
            },
        }
    }

    fn attr_many_term_or_curie_or_absiri(&mut self, name: &str) -> Attr<Vec1<NamedNode>> {
        match self.element.attr(name) {
            None => Attr::Missing,
            Some(v) => {
                let (values, errors) = self.ctx.resolver.many_term_or_curie_or_absiri(v);
                for e in errors {
                    self.processor_graph.report_error(&e);
                }

                match Vec1::try_from_iter(values.into_iter().filter_map(|v| {
                    match v {
                        NamedOrBlankNode::NamedNode(named_node) => Some(named_node),
                        // TODO: produce a warning here when dropping blank nodes
                        NamedOrBlankNode::BlankNode(_) => None,
                    }
                })) {
                    Err(EmptyError { .. }) => Attr::Empty,
                    Ok(v) => Attr::Value(v),
                }
            }
        }
    }

    fn attr_many_curie_or_absiri(&mut self, name: &str) -> Attr<Vec1<NamedNode>> {
        match self.element.attr(name) {
            None => Attr::Missing,
            Some(v) => {
                let (values, errors) = self.ctx.resolver.many_curie_or_absiri(v);
                for e in errors {
                    self.processor_graph.report_error(&e);
                }

                match Vec1::try_from_iter(values.into_iter().filter_map(|v| {
                    match v {
                        NamedOrBlankNode::NamedNode(named_node) => Some(named_node),
                        // TODO: produce a warning here when dropping blank nodes
                        NamedOrBlankNode::BlankNode(_) => None,
                    }
                })) {
                    Err(EmptyError { .. }) => Attr::Empty,
                    Ok(v) => Attr::Value(v),
                }
            }
        }
    }

    fn attr_raw(&self, name: &str) -> Option<&'e str> {
        self.element.attr(name)
    }

    fn text(&self) -> String {
        self.element.text()
    }

    fn inner_html(&self) -> String {
        self.element.inner_html()
    }

    fn inner_xml(&self) -> String {
        self.element.inner_xml(&self.ctx.resolver.iri_mappings)
    }
}

const HTML_LITERAL: NamedNodeRef =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#HTML");

// Construction and RDFa processing steps
impl<'e, 'ec, 'r, 'pg, E: Element, PG: ProcessorGraph> ElementProcessor<'e, 'ec, 'r, 'pg, E, PG> {
    pub fn new(
        eval_context: &'ec mut Rc<EvaluationContext<'r>>,
        element: &'e E,
        processor_graph: &'pg mut PG,
    ) -> ElementProcessor<'e, 'ec, 'r, 'pg, E, PG> {
        let mut ctx = ElementProcessor {
            element,
            processor_graph,
            is_root_element: false,
            datatype: Attr::Missing,
            in_list: false,
            has_content: false,
            ctx: eval_context,
        };

        // TODO: this is not correct for non-HTML implementations
        ctx.is_root_element = ctx.element.tag_name() == "html";
        //debug_assert_eq!(ctx.is_root_element, eval_context.parent_object.is_none(),);

        ctx.datatype = ctx.attr_1_term_or_curie_or_absiri("datatype");
        ctx.in_list = ctx.attr_raw("inlist").is_some();
        ctx.has_content = ctx.attr_raw("content").is_some();

        ctx
    }

    fn get_about(&mut self) -> Attr<NamedOrBlankNode> {
        self.attr_1_safecurie_or_curie_or_iri("about")
    }

    fn get_property(&mut self) -> Option<Vec<NamedNode>> {
        self.attr_many_term_or_curie_or_absiri("property")
            .into_value()
    }

    fn get_typeof(&mut self) -> Option<Vec<NamedNode>> {
        self.attr_many_term_or_curie_or_absiri("typeof")
            .into_value()
    }

    fn get_resource(&mut self) -> Option<NamedOrBlankNode> {
        self.attr_1_safecurie_or_curie_or_iri("resource")
            .into_value_maybe() // if resource is empty (e.g. "[]""), treat it as missing
            .or_else(|| Some(self.attr_iri("href").into_value_maybe()?.into()))
            .or_else(|| Some(self.attr_iri("src").into_value_maybe()?.into()))
    }

    fn extract_relations(&mut self, props: Option<&[NamedNode]>) -> Option<Vec<Relation>> {
        // [rdfa-core] 7.5: 5.
        let rel_dir = if self.in_list {
            Relation::List
        } else {
            Relation::Forward
        };

        let rev_dir = Relation::Reverse;

        let rel: Option<Vec<Relation>>;
        let rev: Option<Vec<Relation>>;
        if props.is_some() {
            // [html-rdfa] extension #7
            // > if the @property attribute and the @rel and/or @rev attribute exists
            // > on the same element, the non-CURIE and non-URI @rel and @rev values
            // > are ignored. If, after this, the value of @rel and/or @rev becomes empty,
            // > then the processor MUST act as if the respective attribute is not present.
            rel = match self.attr_many_curie_or_absiri("rel") {
                Attr::Missing | Attr::Empty => None,
                Attr::Value(v) => Some(v.into_iter().map(rel_dir).collect()),
            };
            rev = match self.attr_many_curie_or_absiri("rev") {
                Attr::Missing | Attr::Empty => None,
                Attr::Value(v) => Some(v.into_iter().map(rev_dir).collect()),
            };
        } else {
            rel = match self.attr_many_term_or_curie_or_absiri("rel") {
                Attr::Missing => None,
                Attr::Empty => Some(Vec::new()),
                Attr::Value(v) => Some(v.into_iter().map(rel_dir).collect()),
            };
            rev = match self.attr_many_term_or_curie_or_absiri("rev") {
                Attr::Missing => None,
                Attr::Empty => Some(Vec::new()),
                Attr::Value(v) => Some(v.into_iter().map(rev_dir).collect()),
            };
        }

        match (rel, rev) {
            (Some(mut rel), Some(rev)) => {
                rel.extend(rev);
                Some(rel)
            }
            (Some(v), _) | (_, Some(v)) => Some(v),
            _ => None,
        }
    }

    fn subject_established(&mut self, output: &mut impl OutputGraph) {
        self.emit_incomplete_triples(output);
    }

    /// Processes the case where we have @rel/@rev.
    fn process_chaining(
        &mut self,
        relations: Vec<Relation>,
        props: Option<&[NamedNode]>,
        output: &mut impl OutputGraph,
    ) {
        // type_of will be taken by either @about (if present), or used for a new bnode
        let mut type_of = self.get_typeof();

        // Figure out the subject:
        if let Attr::Value(about) = self.get_about() {
            Self::type_resource(&mut type_of, about.as_ref(), "@about", output);
            self.ctx.new_subject(Rc::new(about), "@about");
        } else if self.is_root_element {
            Self::type_resource(&mut type_of, self.ctx.subject_ref(), "root element", output);
        }

        self.subject_established(output);

        if let Some(resource) = self.get_resource() {
            let typed = Self::type_resource(&mut type_of, resource.as_ref(), "@resource", output);
            self.emit_props(
                props.unwrap_or_default(),
                typed.then_some(resource.as_ref()),
                output,
            );
            self.emit_relation_triples(resource.as_ref(), relations, output);
            self.ctx.new_subject(Rc::new(resource), "chained @resource");
        } else if let Some(type_of) = type_of.take() {
            let new_node: NamedOrBlankNode = oxrdf::BlankNode::default().into();
            Self::emit_type(&type_of, new_node.as_ref(), "@typeof bnode", output);
            self.emit_relation_triples(new_node.as_ref(), relations, output);
            self.emit_props(props.unwrap_or_default(), Some(new_node.as_ref()), output);
            self.ctx
                .new_subject(Rc::new(new_node), "chained @typeof bnode");
        } else {
            self.store_incomplete_triples(relations);
            // > Also, current object resource should be set to a newly created bnode
            // > (so that the incomplete triples have a subject to connect to if they
            // > are ultimately turned into triples);
            self.emit_props(props.unwrap_or_default(), None, output);
            self.ctx.new_subject(
                Rc::new(BlankNode::default().into()),
                "new bnode for incomplete triples",
            );
        }

        // this case _always_ chains an object
        // (but the object might be the same, so we can't assert that easily)
    }

    pub fn process(&mut self, output: &mut impl OutputGraph) {
        // figure out what kind of element we are dealing with:
        let props = self.get_property();
        // > 6. If the current element does contain a @rel or @rev attribute,
        if let Some(relations) = self.extract_relations(props.as_deref()) {
            self.process_chaining(relations, props.as_deref(), output);
        }
        // > 5. If the current element contains no @rel or @rev attribute,
        // > then the next step is to establish a value for new subject.
        // > This step has two possible alternatives.
        //
        // > 5.1. If the current element contains the @property attribute, but does
        // > not contain either the @content or @datatype attributes, then
        else if let Some(props) = &props
            && !self.has_content
            && !self.datatype.is_present()
        {
            self.process_complex_property(props, output);
        } else {
            // no @property (can be skipped), or @property + (@content | @datatype)
            self.process_simple_property(props.as_deref(), output);
        }
    }

    /// This represents a property which may or may not chain;
    /// it chains if there is no @about, and @typeof
    /// (optionally @resource) is set.
    fn process_complex_property(&mut self, props: &[NamedNode], output: &mut impl OutputGraph) {
        let mut type_of = self.get_typeof();

        let about = self.get_about();
        let about_was_empty = matches!(about, Attr::Empty);

        if let Attr::Value(about) = about {
            Self::type_resource(&mut type_of, about.as_ref(), "@about", output);
            self.ctx.new_subject(Rc::new(about), "@about");
        } else if self.is_root_element {
            Self::type_resource(&mut type_of, self.ctx.subject_ref(), "root element", output);
        }
        self.subject_established(output);

        if let Some(resource) = self.get_resource() {
            self.emit_props(props, Some(resource.as_ref()), output);
            let typed = Self::type_resource(&mut type_of, resource.as_ref(), "@resource", output);
            if typed {
                // @resource is only chained if it had @typeof
                self.ctx.new_subject(Rc::new(resource), "chained @resource");
            }
        } else if let Some(type_of) = std::mem::take(&mut type_of) {
            let typed_resource: NamedOrBlankNode = oxrdf::BlankNode::default().into();
            self.emit_props(
                props,
                // edgecase - if about="[]" then the new typed resource should not be the target
                if about_was_empty {
                    None
                } else {
                    Some(typed_resource.as_ref())
                },
                output,
            );
            Self::emit_type(&type_of, typed_resource.as_ref(), "@typeof bnode", output);
            self.ctx
                .new_subject(Rc::new(typed_resource), "chained @typeof bnode");
        } else {
            self.emit_props(props, None, output);
        }

        // @typeof was consumed, if present
        debug_assert!(type_of.is_none());
    }

    /// This represents the simple case which does not chain.
    fn process_simple_property(
        &mut self,
        props: Option<&[NamedNode]>,
        output: &mut impl OutputGraph,
    ) {
        let type_of = self.get_typeof();

        if let Attr::Value(about) = self.get_about() {
            self.ctx.new_subject(Rc::new(about), "@about");
        } else if let Some(resource) = self.get_resource() {
            self.ctx.new_subject(Rc::new(resource), "@resource");
        } else if self.tag_name() == "head" || self.tag_name() == "body" {
            // [html-rdfa] extension #8
            // > if no IRI is provided by a resource attribute
            // > (e.g., @about, @href, @resource, or @src), then
            // > first check to see if the element is the head or
            // > body element. If it is, then set new subject to parent object.
            // (this is a no-op in this impl)
        }
        // > if the element is the root element of the document,
        // > then act as if there is an empty @about present,
        // > and process it according to the rule for @about, above;
        else if self.is_root_element {
            // at start, new_subject is already the empty IRI
        }
        // > if @typeof is present, then new subject is set to be a newly created bnode;
        else if type_of.is_some() {
            let new_subject = Rc::new(oxrdf::BlankNode::default().into());
            self.ctx.new_subject(new_subject, "@typeof");
        } else {
            // > Additionally, if @property is not present then the skip element flag is set to 'true'.
            if props.is_none() {
                trace!("element skipped (no RDFa attributes present)");
                return;
            }
        }

        self.subject_established(output);

        if let Some(type_of) = type_of {
            Self::emit_type(&type_of, self.ctx.subject_ref(), "simple", output);
        }

        self.emit_props(props.unwrap_or_default(), None, output);
    }

    // output is separate in order to split borrows
    fn emit_prop_values(
        &self,
        properties: &[NamedNode],
        value: TermRef,
        output: &mut impl OutputGraph,
    ) {
        // > The current property value is then used with each predicate as follows:
        //
        // > If the element also includes the @inlist attribute, the current property
        // > value is added to the local list mapping as follows:
        if self.in_list {
            // > if the local list mapping does not contain a list associated with the predicate IRI,
            // > instantiate a new list and add to local list mappings
            //
            // > add the current property value to the list associated with the predicate IRI in the local list mapping
            let term: Rc<oxrdf::Term> = Rc::new(value.into());
            for property in properties {
                self.ctx
                    .list_mapping
                    .borrow_mut()
                    .insert_value(property.clone(), term.clone());
            }
        } else {
            // > Otherwise the current property value isused to generate a triple as follows:
            for property in properties {
                output.emit(
                    // subject = new subject
                    self.ctx.subject.as_ref().as_ref(),
                    // predicate = full IRI
                    property.as_ref(),
                    // object = current property value
                    value,
                );
            }
        }
    }

    fn emit_props(
        &self,
        properties: &[NamedNode],
        fallback_resource: Option<NamedOrBlankNodeRef<'_>>,
        output: &mut impl OutputGraph,
    ) {
        if properties.is_empty() {
            return; // ensure we don't waste time doing work to emit no properties
        }

        let (content_value, data_type) = self.establish_content_value();
        match data_type {
            Attr::Value(data_type) => {
                if data_type == rdf::XML_LITERAL {
                    // > otherwise, as an XML literal if @datatype is present and is set to
                    // > XMLLiteral in the vocabulary http://www.w3.org/1999/02/22-rdf-syntax-ns#.
                    // >
                    // > The value of the XML literal is a string created by serializing to text,
                    // > all nodes that are descendants of the current element, i.e., not including
                    // > the element itself, and giving it a datatype of XMLLiteral in the vocabulary
                    // > http://www.w3.org/1999/02/22-rdf-syntax-ns#. The format of the resulting
                    // > serialized content is as defined in Exclusive XML Canonicalization Version 1.0 [XML-EXC-C14N].
                    //
                    // [HTML-RDFA]
                    // > When generating literals of type XMLLiteral, the processor MUST ensure that the
                    // > output XMLLiteral is a namespace well-formed XML fragment. A namespace well-formed XML
                    // > fragment has the following properties:
                    // >
                    // > - The XML fragment, when placed inside of a single root element, MUST validate as well-formed
                    // >   XML. The normative language that describes a well-formed XML document is specified in
                    // >   Section 2.1 "Well-Formed XML Documents" of the XML specification.
                    // >
                    // > - The XML fragment, when placed inside of a single root element, MUST retain all active
                    // >   namespace information. The currently active attributes declared using @xmlns and @xmlns:
                    // >   that are stored in the RDFa processor's current evaluation context in the IRI mappings
                    // >   MUST be preserved in the generated XMLLiteral. The PREFIX value for @xmlns:PREFIX MUST
                    // >   be entirely transformed into lower-case characters when preserving the value in the
                    // >   XMLLiteral. All active namespaces declared via @xmlns, @xmlns:, and @prefix MUST be
                    // >   placed in each top-level element in the generated XMLLiteral, taking care to not overwrite
                    // >   pre-existing namespace values.
                    let content = self.inner_xml();
                    self.emit_prop_values(
                        properties,
                        LiteralRef::new_typed_literal(&content, rdf::XML_LITERAL).into(),
                        output,
                    );
                } else if data_type == HTML_LITERAL {
                    // [html-rdfa]: 3.1: 11.
                    // > In Section 7.5: Sequence, step 11, immediately after sub-step 2,
                    // > if the @datatype attribute is present and evaluates to
                    // > http://www.w3.org/1999/02/22-rdf-syntax-ns#HTML, the value of the
                    // > HTML Literal is a string created by serializing all child nodes to text.
                    // > This applies to all nodes that are descendants of the current element,
                    // > not including the element itself. The HTML Literal is given a datatype
                    // > of http://www.w3.org/1999/02/22-rdf-syntax-ns#HTML as defined in Section 5.2:
                    // > The rdf:HTML Datatype of [rdf11-concepts]. This feature is non-normative,
                    // > because the equality of the literal values depend on DOM4 [dom4], a specification
                    // > that has not yet reached W3C Recommendation status.
                    // > See [rdf11-concepts] for further details.
                    let content = self.inner_html();
                    self.emit_prop_values(
                        properties,
                        LiteralRef::new_typed_literal(&content, HTML_LITERAL).into(),
                        output,
                    );
                } else {
                    // > as a typed literal if @datatype is present, does not have an empty value according
                    // > to the section on CURIE and IRI Processing, and is not set to XMLLiteral in the
                    // > vocabulary http://www.w3.org/1999/02/22-rdf-syntax-ns#.
                    // > The actual literal is either the value of @content (if present) or
                    // > a string created by concatenating the value of all descendant text nodes,
                    // > of the current element in turn. The final string includes the datatype IRI,
                    // > as described in [RDF-SYNTAX-GRAMMAR], which will have been obtained according
                    // > to the section on CURIE and IRI Processing.
                    let content = content_value.unwrap_or_else(|| self.text().into());
                    self.emit_prop_values(
                        properties,
                        LiteralRef::new_typed_literal(&content, data_type).into(),
                        output,
                    );
                }
            }
            Attr::Empty => {
                trace!("datatype empty, using plain literal");
                // > otherwise, as a plain literal if @datatype is present but has an empty value
                // > according to the section on CURIE and IRI Processing. The actual literal is
                // > either the value of @content (if present) or a string created by concatenating
                // > the value of all descendant text nodes, of the current element in turn.
                let content = content_value.unwrap_or_else(|| self.text().into());
                let term = if let Some(lang) = self.language() {
                    LiteralRef::new_language_tagged_literal_unchecked(&content, lang)
                } else {
                    LiteralRef::new_simple_literal(&content)
                };
                self.emit_prop_values(properties, term.into(), output);
            }
            Attr::Missing => {
                // > otherwise, as a plain literal using the value of @content if @content is present.
                if let Some(content) = content_value {
                    let term = if let Some(lang) = self.language() {
                        LiteralRef::new_language_tagged_literal_unchecked(&content, lang)
                    } else {
                        LiteralRef::new_simple_literal(&content)
                    };
                    self.emit_prop_values(properties, term.into(), output);
                }
                // two cases handled here:
                // > otherwise, if the @rel, @rev, and @content attributes are not present,
                // > as a resource obtained from one of the following:
                // > by using the resource from @resource, if present, ...
                // > otherwise, by using the IRI from @href, if, ...
                // > otherwise, by using the IRI from @src, if present, ...
                // and:
                // > otherwise, if @typeof is present and @about is not, the value of typed resource.
                else if let Some(fallback_resource) = fallback_resource {
                    self.emit_prop_values(properties, fallback_resource.into(), output);
                }
                // otherwise as a plain literal.
                else {
                    // note: @content is None by this point
                    let content_val = self.text();
                    let term: Literal = if let Some(lang) = self.language() {
                        Literal::new_language_tagged_literal_unchecked(content_val, lang)
                    } else {
                        Literal::new_simple_literal(content_val)
                    };
                    self.emit_prop_values(properties, term.as_ref().into(), output);
                }
            }
        }
    }

    fn emit_incomplete_triples(&mut self, output: &mut impl OutputGraph) {
        // > The list of incomplete triples from the current evaluation context
        // > (not the local list of incomplete triples) will contain zero or more predicate
        // > IRIs. This list is iterated over and each of the predicates is used with parent
        // > subject and new subject to generate a triple or add a new element to the local
        // > list mapping. Note that at each level there are two lists of incomplete triples;
        // > one for the current processing level (which is passed to each child element in
        // > the previous step), and one that was received as part of the evaluation context.
        // > It is the latter that is used in processing during this step.
        for incomplete in std::mem::take(&mut Rc::make_mut(self.ctx).incomplete_triples) {
            // > Note that each incomplete triple has a direction value that is used to determine
            // > what will become the subject, and what will become the object, of each generated triple:
            match incomplete {
                // > If direction is 'none',
                // > the new subject is added to the list from the iterated incomplete triple.
                IncompleteTriple::List(list) => {
                    list.borrow_mut()
                        .push(Rc::new((*self.ctx.subject).clone().into()));
                }
                // > If direction is 'forward' then the following triple is generated:
                IncompleteTriple::Forward { subject, predicate } => {
                    output.emit(
                        // subject = parent subject
                        subject.as_ref().into(),
                        // predicate = the predicate from the iterated incomplete triple
                        predicate.as_ref().into(),
                        // object = new subject
                        self.ctx.subject_ref().into(),
                    );
                }
                // > If direction is 'reverse' then this is the triple generated:
                IncompleteTriple::Reverse { predicate, object } => {
                    output.emit(
                        // subject = new subject
                        self.ctx.subject_ref(),
                        // predicate = the predicate from the iterated incomplete triple
                        predicate.as_ref().into(),
                        // object = parent subject
                        object.as_ref().into(),
                    );
                }
            }
        }
    }

    /// the @typeof can only apply to one resource; this lets the first taker win
    /// returns `true` if @typeof was applied (even if empty)
    fn type_resource(
        type_of: &mut Option<Vec<NamedNode>>,
        typed_resource: NamedOrBlankNodeRef<'_>,
        source: &'static str,
        output: &mut impl OutputGraph,
    ) -> bool {
        if let Some(type_of) = std::mem::take(type_of) {
            Self::emit_type(&type_of, typed_resource, source, output);
            true
        } else {
            false
        }
    }

    // [rdfa-core] 7.5: 7.
    fn emit_type(
        type_of: &[NamedNode],
        typed_resource: NamedOrBlankNodeRef<'_>,
        source: &'static str,
        output: &mut impl OutputGraph,
    ) {
        trace!(%typed_resource, source, "specifying rdf:type");
        // > If in any of the previous steps a typed resource was set to a non-null value,
        // > it is now used to provide a subject for type values;
        // > If present, the attribute may contain one or more IRIs, obtained according
        // > to the section on CURIE and IRI Processing, each of which is used to generate
        // > a triple as follows:
        for type_iri in type_of {
            output.emit(
                // subject = typed resource
                typed_resource,
                // predicate = http://www.w3.org/1999/02/22-rdf-syntax-ns#type
                oxrdf::vocab::rdf::TYPE,
                // object = current full IRI of 'type' from typed resource
                type_iri.into(),
            );
        }
    }

    fn emit_relation_triples(
        &mut self,
        current_object_resource: NamedOrBlankNodeRef,
        relations: Vec<Relation>,
        output: &mut impl OutputGraph,
    ) {
        if relations.is_empty() {
            return;
        }

        trace!(%current_object_resource, "generating triples");
        let term: Rc<oxrdf::Term> = Rc::new(current_object_resource.into());
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
                    self.ctx
                        .list_mapping
                        .borrow_mut()
                        .insert_value(predicate, term.clone());
                }
                // > If present, @rel may contain one or more resources, obtained according
                // > to the section on CURIE and IRI Processing each of which is used to
                // > generate a triple as follows:
                Relation::Forward(predicate) => {
                    output.emit(
                        //  subject = new subject
                        self.ctx.subject_ref(),
                        //  predicate = full IRI
                        predicate.as_ref(),
                        //  object = current object resource
                        current_object_resource.into(),
                    );
                }
                Relation::Reverse(predicate) => {
                    // > If present, @rev
                    // > obtained according to the section on CURIE and IRI Processing
                    // > each of which is used to generate a triple as follows:
                    output.emit(
                        // > subject = current object resource
                        current_object_resource,
                        // > predicate = full IRI
                        predicate.as_ref(),
                        // > object = new subject
                        self.ctx.subject_ref().into(),
                    );
                }
            }
        }
    }

    fn store_incomplete_triples(&mut self, relations: Vec<Relation>) {
        let ec = Rc::make_mut(self.ctx);

        // [rdfa-core] 7.5: 10.
        // > If however current object resource was set to null, but there are predicates present,
        // > then they must be stored as incomplete triples, pending the discovery of a subject
        // > that can be used as the object.
        trace!("generating incomplete triples (current object resource null)");
        for relation in relations {
            match relation {
                // > Predicates for incomplete triples can be set by using one or both of the @rel and @rev attributes:
                //
                // > If the element contains the @inlist attribute,
                // > then if the local list mapping does not contain a list associated with the IRI,
                // > instantiate a new list and add to local list mappings.
                Relation::List(p) => {
                    let list = ec.list_mapping.borrow_mut().ensure_list(p);
                    ec.incomplete_triples.push(IncompleteTriple::List(list));
                }
                // > If present, @rel must contain one or more resources,
                // > obtained according to the section  on CURIE and IRI Processing,
                // > each of which is added to the local list of incomplete triples as follows:
                Relation::Forward(predicate) => {
                    // > - predicate = full IRI
                    // > - direction = forward
                    ec.incomplete_triples.push(IncompleteTriple::Forward {
                        subject: ec.subject.clone(),
                        predicate: Rc::new(predicate),
                    });
                }
                // > If present, @rev must contain one or more resources,
                // > obtained according to the section on CURIE and IRI Processing,
                // > each of which is added to the local list of incomplete triples as follows:
                Relation::Reverse(predicate) => {
                    // > - predicate = full IRI
                    // > - direction = reverse
                    ec.incomplete_triples.push(IncompleteTriple::Reverse {
                        predicate: Rc::new(predicate),
                        object: ec.subject.clone(),
                    });
                }
            }
        }

        trace!(triples = ?self.ctx.incomplete_triples, "incomplete triples ");
    }

    /// Figures out what value to use for @content, and its @datatype.
    ///
    /// Note that this does _not_ return the text content of the element (except in the HTML
    /// override cases).
    fn establish_content_value(&self) -> (Option<Cow<'_, str>>, Attr<NamedNodeRef<'_>>) {
        let datatype = self.datatype.as_ref();
        // [html-rdfa] 3.1: 9.
        // > In Section 7.5: Sequence, processing step 11, the HTML5 @datetime attribute MUST be
        // > utilized when generating the current property value, unless @content is also present
        // > on the same element.
        if let Some(content) = self.attr_raw("content") {
            return (Some(content.into()), datatype);
        }

        // > Otherwise, if @datetime is present, the current property value
        // > must be generated as follows. The literal value is the value contained in the @datetime
        // > attribute.
        if let Some(datetime) = self
            .attr_raw("datetime")
            .map(Cow::Borrowed)
            // > In Section 7.5: Sequence, processing step 11, if the element is time, and the element
            // > does not have @datetime or @content attributes, the processor MUST act as if there is
            // > a @datetime attribute containing exactly the element's text value.
            .or_else(|| (self.tag_name() == "time").then_some(self.text().into()))
        {
            // > If @datatype is present, it is to be used as defined in the RDFa Core
            // > [rdfa-core] processing rules. Otherwise, if the value of @datetime lexically matches
            // > a valid xsd:date, xsd:time, xsd:dateTime, xsd:duration, xsd:gYear, or xsd:gYearMonth
            // > a typed literal must be generated, with its datatype set to the matching xsd datatype.
            //
            // > Otherwise, a plain literal MUST be generated, taking into account the current language.
            let inferred_type = datatype.map_missing_maybe(|| determine_xsd_time_type(&datetime));
            return (Some(datetime), inferred_type);
        }

        (None, datatype)
    }
}

fn determine_xsd_time_type(dt: &str) -> Option<NamedNodeRef<'static>> {
    // [html-rdfa] 3.1: 9.
    // > Implementers may want to use the following order of match testing: xsd:duration,
    // > xsd:dateTime, xsd:date, xsd:time, xsd:gYearMonth, and xsd:gYear.
    if oxsdatatypes::Duration::from_str(dt).is_ok() {
        Some(xsd::DURATION)
    } else if oxsdatatypes::DateTime::from_str(dt).is_ok() {
        Some(xsd::DATE_TIME)
    } else if oxsdatatypes::Date::from_str(dt).is_ok() {
        Some(xsd::DATE)
    } else if oxsdatatypes::Time::from_str(dt).is_ok() {
        Some(xsd::TIME)
    } else if oxsdatatypes::GYearMonth::from_str(dt).is_ok() {
        Some(xsd::G_YEAR_MONTH)
    } else if oxsdatatypes::GYear::from_str(dt).is_ok() {
        Some(xsd::G_YEAR)
    } else {
        None
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_determine_xsd_time_type() {
        let d = determine_xsd_time_type;

        assert_eq!(d("abc"), None);
        assert_eq!(d("1234"), Some(xsd::G_YEAR));
        assert_eq!(d("1234-05"), Some(xsd::G_YEAR_MONTH));
        assert_eq!(d("1234-05-06"), Some(xsd::DATE));
        assert_eq!(d("1234-05-06T07:08:09"), Some(xsd::DATE_TIME));
        assert_eq!(d("1234-05-06T07:08:09Z"), Some(xsd::DATE_TIME));
        assert_eq!(d("07:08:09"), Some(xsd::TIME));
        assert_eq!(d("P1Y2M3DT4H5M6S"), Some(xsd::DURATION));
    }
}
