use oxrdf::{NamedNodeRef, NamedOrBlankNodeRef, TermRef, TripleRef};
use tracing::trace;

/// A minimal representation of an output graph.
///
/// This only supports emitting triples, not reading or modifying them.
pub trait OutputGraph {
    fn emit(&mut self, subject: NamedOrBlankNodeRef, predicate: NamedNodeRef, object: TermRef);
}

/// Allows use of [`oxrdf::Graph`] as an output graph.
impl OutputGraph for oxrdf::Graph {
    fn emit(&mut self, subject: NamedOrBlankNodeRef, predicate: NamedNodeRef, object: TermRef) {
        trace!(subject = %subject, predicate = %predicate, object = %object, "emitting output triple");
        self.insert(TripleRef::new(subject, predicate, object));
    }
}
