use oxrdf::{LiteralRef, TripleRef, vocab::xsd};
use std::fmt::Write;
use tracing::trace;

use crate::vocab;

/// This trait provides an abstraction over RDFa processor graphs.
///
/// An implementation is provided for `oxrdf::Graph`, but this trait can be implemented for other graph types as needed.
pub trait ProcessorGraph {
    /// Emits a message to the processor graph.
    ///
    /// See [rdfa-core] 7.6.2 ["Processor Graph Terms"](https://www.w3.org/TR/rdfa-core/#h-processor-graph-terms):
    ///
    /// > To ensure interoperability, a core hierarchy of classes is defined for the content of the processor graph.
    /// > Separate errors or warnings are resources (typically blank nodes) of a specific type, with additional properties
    /// > giving more details on the error condition or the warning. This specification defines only the top level classes
    /// > and the ones referring to the error and warning conditions defined explicitly by this document. Other,
    /// > implementation-specific subclasses may be defined by the RDFa Processor [see [`PGClass::Custom`]].
    /// >
    /// > The top level classes are `rdfa:Error` [[`PGClass::Error`]], `rdfa:Warning` [[`PGClass::Warning`]],
    /// > and `rdfa:Info` [[`PGClass::Info`]], defined as part of the RDFa Vocabulary. Furthermore, a single property
    /// > is defined on those classes, namely `rdfa:context`, that provides an extra context for the error,
    /// > e.g., http response, an XPath information, or simply the IRI to the RDFa resource. Usage of this
    /// > property is optional, and more than one triple can be used with this predicate on the same subject. Finally,
    /// > error and warning instances SHOULD use the `dc:description` and `dc:date` properties. `dc:description` should provide
    /// > a short, human readable but implementation dependent description of the error. `dc:date` should give the time when
    /// > the error was found and it is advised to be as precise as possible to allow the detection of,
    /// > for example, possible network errors.
    fn emit_message(&mut self, message_class: PGClass, description: &str, context: Option<&str>);

    /// Convenience method to emit a message from an error.
    ///
    /// This emits the error with a `dc:description` consisting of its [`std::fmt::Display`]
    /// implementation, with any source errors are appended in parentheses.
    ///
    /// The error's [`PGClass`] is used to determine the message class;
    /// if `None` is returned, no message is emitted.
    fn report_error(&mut self, error: &dyn ProcessorGraphError) {
        if let Some(msg_type) = error.message_class() {
            let mut description = String::new();
            let mut err: Option<&dyn std::error::Error> = Some(error);
            let mut count = 0;
            while let Some(e) = err {
                _ = match count {
                    0 => write!(description, "{e}"),
                    1 => write!(description, " ({e}"),
                    _ => write!(description, "; {e}"),
                };
                err = e.source();
                count += 1;
            }

            if count > 1 {
                description.push(')');
            }

            self.emit_message(msg_type, &description, None);
        }
    }
}

#[doc(hidden)]
pub fn emit_oxrdf(
    graph: &mut oxrdf::Graph,
    message_class: PGClass,
    msg: &str,
    context: Option<&str>,
    timestamp: jiff::Timestamp,
) {
    let timestamp = timestamp.to_string();
    let msg_subj: oxrdf::NamedOrBlankNode = oxrdf::BlankNode::default().into();
    let pg_class: oxrdf::NamedNodeRef = message_class.into();

    // new bnode is-a PGClass
    let ty = TripleRef::new(&msg_subj, oxrdf::vocab::rdf::TYPE, pg_class);
    trace!(triple = %ty, "emitting processor triple (type)");
    graph.insert(ty);

    let date = TripleRef::new(
        &msg_subj,
        vocab::dcterms::DATE,
        LiteralRef::new_typed_literal(&timestamp, xsd::DATE_TIME),
    );
    trace!(triple = %date, "emitting processor triple (date)");
    graph.insert(date);

    // add description
    let desc = TripleRef::new(
        &msg_subj,
        vocab::dcterms::DESCRIPTION,
        LiteralRef::new_simple_literal(msg),
    );
    trace!(triple = %desc, "emitting processor triple (description)");
    graph.insert(desc);

    if let Some(ctx) = context {
        let ctx_triple = TripleRef::new(
            &msg_subj,
            vocab::rdfa::CONTEXT_PROPERTY,
            LiteralRef::new_simple_literal(ctx),
        );
        trace!(triple = %ctx_triple, "emitting processor triple (context)");
        graph.insert(ctx_triple);
    }
}

/// Allows use of [`oxrdf::Graph`] as a processor graph.
impl ProcessorGraph for oxrdf::Graph {
    fn emit_message(&mut self, message_class: PGClass, msg: &str, context: Option<&str>) {
        let now = jiff::Timestamp::now();
        emit_oxrdf(self, message_class, msg, context, now);
    }
}

/// Extension to the standard `Error` trait to allow errors to specify a
/// `rdfa:PGClass` for processor graph reporting.
pub trait ProcessorGraphError: std::error::Error {
    fn message_class(&self) -> Option<PGClass>;
}

/// Classes for the _Processor Graph Reporting Vocabulary_, as defined in
/// the [RDFa Vocabulary](https://www.w3.org/ns/rdfa).
///
/// These are designed to be used with [`ProcessorGraph`].
///
pub enum PGClass {
    /// `rdfa:Error` is the class for all error conditions
    Error,
    /// error condition; to be used when the document fails to be fully processed
    /// as a result of non-conformant host language markup
    DocumentError,
    /// `rdfa:Warning` is the class for all warnings
    Warning,
    /// warning; to be used when the value of a @vocab attribute cannot be dereferenced,
    /// hence the vocabulary expansion cannot be completed
    VocabReferenceError,
    /// warning; to be used when a CURIE prefix fails to be resolved
    UnresolvedCurie,
    /// warning; to be used when a Term fails to be resolved
    UnresolvedTerm,
    /// warning; to be used when a prefix, either from the initial context or
    /// inherited from an ancestor node, is redefined in an element
    PrefixRedefinition,
    /// `rdfa:Info` is the class for all informations
    Info,
    /// Allows use of any custom IRI representing a message type.
    Custom(oxrdf::NamedNodeRef<'static>),
}

/// A [`PGClass`] member can be converted to its IRI representation.
impl From<PGClass> for oxrdf::NamedNodeRef<'static> {
    fn from(val: PGClass) -> Self {
        match val {
            PGClass::Error => vocab::rdfa::ERROR,
            PGClass::Warning => vocab::rdfa::WARNING,
            PGClass::Info => vocab::rdfa::INFO,
            PGClass::DocumentError => vocab::rdfa::DOCUMENT_ERROR,
            PGClass::VocabReferenceError => vocab::rdfa::VOCAB_REFERENCE_ERROR,
            PGClass::UnresolvedCurie => vocab::rdfa::UNRESOLVED_CURIE,
            PGClass::UnresolvedTerm => vocab::rdfa::UNRESOLVED_TERM,
            PGClass::PrefixRedefinition => vocab::rdfa::PREFIX_REDEFINITION,
            PGClass::Custom(node) => node,
        }
    }
}
