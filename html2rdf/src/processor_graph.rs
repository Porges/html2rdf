use oxrdf::TripleRef;
use tracing::trace;

use crate::vocabs;

pub(crate) struct ProcessorGraph<'p>(pub &'p mut oxrdf::Graph);

impl<'p> ProcessorGraph<'p> {
    pub(crate) fn emit_message(&mut self, pg_type: MessageType, msg: &str) {
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

pub enum MessageType {
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

impl From<MessageType> for oxrdf::NamedNodeRef<'static> {
    fn from(val: MessageType) -> Self {
        match val {
            MessageType::Error => vocabs::rdfa::ERROR,
            MessageType::Warning => vocabs::rdfa::WARNING,
            MessageType::DocumentError => vocabs::rdfa::DOCUMENT_ERROR,
            MessageType::VocabReferenceError => vocabs::rdfa::VOCAB_REFERENCE_ERROR,
            MessageType::UnresolvedCurie => vocabs::rdfa::UNRESOLVED_CURIE,
            MessageType::UnresolvedTerm => vocabs::rdfa::UNRESOLVED_TERM,
            MessageType::PrefixRedefinition => vocabs::rdfa::PREFIX_REDEFINITION,
        }
    }
}
