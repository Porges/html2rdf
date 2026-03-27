//! Graph-related abstractions for use in RDFa processing.

mod output;
mod processor;

pub use output::OutputGraph;
pub use processor::{PGClass, ProcessorGraph, ProcessorGraphError, emit_oxrdf};

/// Allows use of `()` to represent a 'null output graph'.
impl OutputGraph for () {
    fn emit(
        &mut self,
        _subject: oxrdf::NamedOrBlankNodeRef,
        _predicate: oxrdf::NamedNodeRef,
        _object: oxrdf::TermRef,
    ) {
    }
}

/// Allows use of `()` to represent a 'null processor graph'.
impl ProcessorGraph for () {
    fn report_error(&mut self, _error: &dyn ProcessorGraphError) {}
    fn emit_message(
        &mut self,
        _message_class: PGClass,
        _description: &str,
        _context: Option<&str>,
    ) {
    }
}
