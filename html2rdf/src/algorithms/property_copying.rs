use oxrdf::{Graph, TermRef, TripleRef, vocab::rdf};

use crate::vocab::rdfa;

/// Implements the [_HTML+RDFa 1.1_](https://www.w3.org/TR/rdfa-in-html/) "property copying" algorithm.
///
/// See: <https://www.w3.org/TR/rdfa-in-html/#implementing-property-copying>
pub fn property_copying(graph: &mut Graph) {
    // Step 1: add
    {
        let mut added_any = true;
        let mut new_triples = Vec::new();
        while added_any {
            new_triples.clear();
            added_any = false;

            for copy_triple in graph.triples_for_predicate(rdfa::COPY) {
                let copy_target: oxrdf::NamedOrBlankNodeRef = match copy_triple.object {
                    TermRef::NamedNode(n) => n.into(),
                    TermRef::BlankNode(n) => n.into(),
                    TermRef::Literal(_) => {
                        continue; // TODO: warning?
                    }
                };

                if !graph.contains(TripleRef::new(copy_target, rdf::TYPE, rdfa::PATTERN)) {
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
        for copy_triple in graph.triples_for_predicate(rdfa::COPY) {
            triples_to_remove.push(copy_triple.into_owned());
            let copy_target: oxrdf::NamedOrBlankNodeRef = match copy_triple.object {
                TermRef::NamedNode(n) => n.into(),
                TermRef::BlankNode(n) => n.into(),
                TermRef::Literal(_) => continue,
            };

            if !graph.contains(TripleRef::new(copy_target, rdf::TYPE, rdfa::PATTERN)) {
                continue;
            }

            triples_to_remove
                .push(TripleRef::new(copy_triple.subject, rdf::TYPE, rdfa::PATTERN).into_owned());

            for trip in graph.triples_for_subject(copy_target) {
                triples_to_remove.push(trip.into_owned());
            }
        }

        for triple in triples_to_remove {
            graph.remove(&triple);
        }
    }
}

#[cfg(test)]
mod tests {
    use oxrdf::vocab::rdf;
    use oxrdf::*;

    use crate::vocab::rdfa;

    use super::*;

    fn node(iri: &str) -> NamedNode {
        NamedNode::new(iri).unwrap()
    }

    #[test]
    fn basic_copy_from_pattern() {
        let mut graph = Graph::new();

        let subject = node("http://example.com/post1");
        let pattern = node("http://example.com/pattern1");
        let license = node("http://creativecommons.org/licenses/by/3.0/");

        // subject rdfa:copy pattern
        graph.insert(TripleRef::new(&subject, rdfa::COPY, &pattern));
        // pattern is an rdfa:Pattern
        graph.insert(TripleRef::new(&pattern, rdf::TYPE, rdfa::PATTERN));
        // pattern has a property
        graph.insert(TripleRef::new(
            &pattern,
            node("http://example.com/license").as_ref(),
            &license,
        ));

        property_copying(&mut graph);

        // The property should be copied to the subject
        assert!(graph.contains(TripleRef::new(
            &subject,
            node("http://example.com/license").as_ref(),
            &license,
        )));

        // The pattern triples and copy predicate should be removed
        assert!(graph.triples_for_predicate(rdfa::COPY).next().is_none());
        assert!(
            graph
                .subjects_for_predicate_object(rdf::TYPE, rdfa::PATTERN)
                .next()
                .is_none()
        );
        assert!(!graph.contains(TripleRef::new(
            &pattern,
            node("http://example.com/license").as_ref(),
            &license,
        )));
    }

    #[test]
    fn multiple_subjects_copy_same_pattern() {
        let mut graph = Graph::new();

        let post1 = node("http://example.com/post1");
        let post2 = node("http://example.com/post2");
        let pattern = node("http://example.com/pattern1");
        let license = node("http://creativecommons.org/licenses/by/3.0/");
        let license_pred = node("http://example.com/license");

        graph.insert(TripleRef::new(&post1, rdfa::COPY, &pattern));
        graph.insert(TripleRef::new(&post2, rdfa::COPY, &pattern));
        graph.insert(TripleRef::new(&pattern, rdf::TYPE, rdfa::PATTERN));
        graph.insert(TripleRef::new(&pattern, license_pred.as_ref(), &license));

        property_copying(&mut graph);

        // Both subjects should have the copied property
        assert!(graph.contains(TripleRef::new(&post1, license_pred.as_ref(), &license)));
        assert!(graph.contains(TripleRef::new(&post2, license_pred.as_ref(), &license)));

        // Pattern triples should be removed
        assert!(graph.triples_for_predicate(rdfa::COPY).next().is_none());
        assert!(
            graph
                .subjects_for_predicate_object(rdf::TYPE, rdfa::PATTERN)
                .next()
                .is_none()
        );
    }

    #[test]
    fn copy_target_not_a_pattern_is_ignored() {
        let mut graph = Graph::new();

        let subject = node("http://example.com/post1");
        let not_pattern = node("http://example.com/not-a-pattern");
        let value = Literal::new_simple_literal("hello");

        // subject rdfa:copy not_pattern, but not_pattern is NOT typed rdfa:Pattern
        graph.insert(TripleRef::new(&subject, rdfa::COPY, &not_pattern));
        graph.insert(TripleRef::new(
            &not_pattern,
            node("http://example.com/prop").as_ref(),
            &value,
        ));

        property_copying(&mut graph);

        // The property should NOT be copied
        assert!(!graph.contains(TripleRef::new(
            &subject,
            node("http://example.com/prop").as_ref(),
            &value,
        )));

        // The copy predicate should still be removed
        assert!(graph.triples_for_predicate(rdfa::COPY).next().is_none());

        // The non-pattern's own triples should remain (not removed since it's not a pattern)
        assert!(graph.contains(TripleRef::new(
            &not_pattern,
            node("http://example.com/prop").as_ref(),
            &value,
        )));
    }

    #[test]
    fn copy_target_literal_is_ignored() {
        let mut graph = Graph::new();

        let subject = node("http://example.com/post1");
        let literal_value = Literal::new_simple_literal("not-a-node");

        graph.insert(TripleRef::new(&subject, rdfa::COPY, &literal_value));

        property_copying(&mut graph);

        // The copy predicate should be removed
        assert!(graph.triples_for_predicate(rdfa::COPY).next().is_none());
    }

    #[test]
    fn chained_copy() {
        let mut graph = Graph::new();

        let subject = node("http://example.com/post1");
        let pattern_a = node("http://example.com/patternA");
        let pattern_b = node("http://example.com/patternB");
        let value = Literal::new_simple_literal("deep-value");
        let prop = node("http://example.com/prop");

        // subject -> patternA -> patternB (chained via rdfa:copy)
        graph.insert(TripleRef::new(&subject, rdfa::COPY, &pattern_a));
        graph.insert(TripleRef::new(&pattern_a, rdf::TYPE, rdfa::PATTERN));
        graph.insert(TripleRef::new(&pattern_a, rdfa::COPY, &pattern_b));
        graph.insert(TripleRef::new(&pattern_b, rdf::TYPE, rdfa::PATTERN));
        graph.insert(TripleRef::new(&pattern_b, prop.as_ref(), &value));

        property_copying(&mut graph);

        // The deep property should be transitively copied to the subject
        assert!(graph.contains(TripleRef::new(&subject, prop.as_ref(), &value)));

        // All patterns and copy predicates should be cleaned up
        assert!(graph.triples_for_predicate(rdfa::COPY).next().is_none());
        assert!(
            graph
                .subjects_for_predicate_object(rdf::TYPE, rdfa::PATTERN)
                .next()
                .is_none()
        );
    }

    #[test]
    fn pattern_literal_properties_are_copied() {
        let mut graph = Graph::new();

        let subject = node("http://example.com/post1");
        let pattern = node("http://example.com/pattern1");
        let author = Literal::new_simple_literal("Alice");
        let prop = node("http://example.com/author");

        graph.insert(TripleRef::new(&subject, rdfa::COPY, &pattern));
        graph.insert(TripleRef::new(&pattern, rdf::TYPE, rdfa::PATTERN));
        graph.insert(TripleRef::new(&pattern, prop.as_ref(), &author));

        property_copying(&mut graph);

        assert!(graph.contains(TripleRef::new(&subject, prop.as_ref(), &author)));
    }
}
