use std::collections::HashSet;

use oxrdf::{Graph, Term, Triple, TripleRef, vocab::rdf};

use crate::vocab::rdfa;

/// Implements the [_HTML+RDFa 1.1_](https://www.w3.org/TR/rdfa-in-html/) "property copying" algorithm.
///
/// See: <https://www.w3.org/TR/rdfa-in-html/#implementing-property-copying>
pub fn property_copying(graph: &mut Graph) {
    // Step 1: add
    let mut patterns_to_remove = Vec::new();
    {
        let mut copy_triples = graph
            .triples_for_predicate(rdfa::COPY)
            .map(|t| (t.subject.into_owned(), t.object.into_owned()))
            .collect::<Vec<_>>();
        let mut visited_triples = copy_triples.iter().cloned().collect::<HashSet<_>>();
        let mut to_insert = Vec::new();
        while let Some(copy_triple) = copy_triples.pop() {
            let copy_target: oxrdf::NamedOrBlankNode = match copy_triple.1 {
                Term::NamedNode(n) => n.into(),
                Term::BlankNode(n) => n.into(),
                Term::Literal(_) => {
                    continue; // TODO: warning?
                }
            };

            if !graph.contains(TripleRef::new(&copy_target, rdf::TYPE, rdfa::PATTERN)) {
                continue; // TODO: warning?
            }

            patterns_to_remove.push(copy_target.clone());

            for trip in graph.triples_for_subject(&copy_target) {
                if trip.predicate == rdfa::COPY {
                    let new_copy_triple = (copy_triple.0.clone(), trip.object.into_owned());
                    if visited_triples.insert(new_copy_triple.clone()) {
                        copy_triples.push(new_copy_triple);
                    }
                } else if trip.predicate == rdf::TYPE && trip.object == rdfa::PATTERN.into() {
                    // this is not per spec but we need it to stop type "pollution"
                    // skip, don't copy the rdf:type triple
                } else {
                    to_insert.push(Triple::new(
                        copy_triple.0.clone(),
                        trip.predicate,
                        trip.object,
                    ));
                }
            }

            for triple in to_insert.drain(..) {
                graph.insert(&triple);
            }
        }
    }

    // Step 2: remove
    {
        let mut triples_to_remove = Vec::new();
        for pattern in patterns_to_remove {
            for pattern_triple in graph.triples_for_subject(&pattern) {
                triples_to_remove.push(pattern_triple.into_owned());
            }
            for copy_subject in graph.subjects_for_predicate_object(rdfa::COPY, &pattern) {
                triples_to_remove.push(Triple::new(copy_subject, rdfa::COPY, pattern.clone()));
            }
            for triple in triples_to_remove.drain(..) {
                graph.remove(&triple);
            }
        }
    }
}
