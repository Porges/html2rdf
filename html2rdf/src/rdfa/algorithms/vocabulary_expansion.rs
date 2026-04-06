use std::borrow::Cow;

use oxiri::Iri;
use oxrdf::{
    Graph, NamedNodeRef, NamedOrBlankNodeRef, TermRef, Triple, TripleRef,
    vocab::{rdf, rdfs},
};

use crate::vocab::{owl, rdfa};

pub trait VocabularyResolver {
    fn resolve(&self, vocab_iri: &str) -> Option<Cow<'_, Graph>>;
}

impl<T: VocabularyResolver> VocabularyResolver for &T {
    fn resolve(&self, vocab_iri: &str) -> Option<Cow<'_, Graph>> {
        (*self).resolve(vocab_iri)
    }
}

/// Allows resolving vocabularies by fetching their target IRIs
/// over HTTP(S).
#[cfg(feature = "vocab-online")]
pub struct OnlineVocabularyResolver {
    client: reqwest::blocking::Client,
}

#[cfg(feature = "vocab-online")]
impl Default for OnlineVocabularyResolver {
    fn default() -> Self {
        Self {
            client: reqwest::blocking::Client::new(),
        }
    }
}

// yo dawg
#[cfg(feature = "html")]
fn vocab_from_html(html: &str, base: Iri<&str>) -> Graph {
    let (o_graph, ()) = crate::doc_to_graphs::<crate::host_language::Html5, ()>(
        html,
        base.as_ref(),
        crate::Options::default(), // we explicitly do _not_ enable expansion on vocabularies
    )
    .unwrap_or_else(|inf| match inf {});
    o_graph
}

#[cfg(feature = "vocab-online")]
impl VocabularyResolver for OnlineVocabularyResolver {
    fn resolve(&self, vocab_iri: &str) -> Option<Cow<'_, Graph>> {
        use tracing::debug;

        debug!(vocab_iri, "Fetching vocabulary");

        let response = self.client.get(vocab_iri).send().ok()?;
        if !response.status().is_success() {
            return None;
        }

        match response.headers().get(reqwest::header::CONTENT_TYPE) {
            Some(content_type) if content_type.to_str().ok()?.starts_with("text/html") => {
                let base = Iri::parse(response.url().to_string()).ok()?;
                let vocab_graph = vocab_from_html(&response.text().ok()?, base.as_ref());
                debug!(
                    triples = vocab_graph.len(),
                    vocab_iri, "Fetched vocabulary successfully"
                );
                Some(Cow::Owned(vocab_graph))
            }
            // TODO: turtle, RDF/XML, etc.
            _ => None,
        }
    }
}

/// Allows resolving vocabularies through a preloaded
/// set of vocabulary documents, for offline processing.
#[derive(Default)]
#[cfg(feature = "html")]
pub struct OfflineVocabularyResolver {
    vocabularies: std::collections::HashMap<String, Graph>,
}

#[cfg(feature = "html")]
impl VocabularyResolver for OfflineVocabularyResolver {
    fn resolve(&self, vocab_iri: &str) -> Option<Cow<'_, Graph>> {
        self.vocabularies.get(vocab_iri).map(Cow::Borrowed)
    }
}

#[cfg(feature = "html")]
impl OfflineVocabularyResolver {
    pub fn insert(&mut self, vocab_iri: String, graph: Graph) {
        self.vocabularies.insert(vocab_iri, graph);
    }

    // TODO: validation of IRIs here
    pub fn insert_from_html(&mut self, vocab_iri: String, html: &str) {
        let base = Iri::parse(vocab_iri.as_str()).unwrap();
        let graph = vocab_from_html(html, base);
        self.insert(vocab_iri, graph);
    }
}

/// Implements the [_RDFa Core_ "vocabulary expansion"][vocab-expansion] algorithm.
///
/// [vocab-expansion]: https://www.w3.org/TR/rdfa-core/#s_vocab_expansion
///
/// Note that this is a very naïve implementation and not very efficient.
/// It should not be used to service large graphs in production services.
pub fn vocabulary_expansion(graph: &mut Graph, base: Iri<&str>, resolver: &dyn VocabularyResolver) {
    let vocab_graph = resolve_vocab_graphs(graph, base, resolver);
    expand(graph, vocab_graph);
}

/// Fetches the vocabularies used by the output graph and merges them into the output graph.
fn resolve_vocab_graphs(
    graph: &Graph,
    base: Iri<&str>,
    resolver: &dyn VocabularyResolver,
) -> Graph {
    let mut vocab_graph = graph.clone();
    for vocab_iri in
        graph.objects_for_subject_predicate(NamedNodeRef::from(base), rdfa::USES_VOCABULARY)
    {
        if let TermRef::NamedNode(vocab_iri) = vocab_iri
            && let Some(expanded_graph) = resolver.resolve(vocab_iri.as_str())
        {
            vocab_graph.extend(&*expanded_graph);
        }
    }
    vocab_graph
}

fn expand(graph: &mut Graph, mut vocab_graph: Graph) {
    let mut modified = true;
    let mut triples_to_add: Vec<Triple> = Vec::new();
    while modified {
        modified = false;
        triples_to_add.clear();

        prp_spo1(&vocab_graph, &mut triples_to_add);
        prp_eqp1_2(&vocab_graph, &mut triples_to_add);
        cax_sco(&vocab_graph, &mut triples_to_add);
        cax_eqc1_2(&vocab_graph, &mut triples_to_add);

        for triple in &triples_to_add {
            modified |= vocab_graph.insert(triple);

            // my _guess_ here is that
            // only triples about stuff that already exists in the output graph should be added
            // TODO: should we also be checking object?
            if graph.triples_for_subject(&triple.subject).next().is_some()
                || graph.triples_for_object(&triple.object).next().is_some()
            {
                modified |= graph.insert(triple);
            }
        }
    }
}

/// `T(?p1, rdfs:subPropertyOf, ?p2) & T(?x, ?p1, ?y) => T(?x, ?p2, ?y)`
fn prp_spo1(graph: &Graph, triples_to_add: &mut Vec<Triple>) {
    for super_prop_rel in graph.triples_for_predicate(rdfs::SUB_PROPERTY_OF) {
        let NamedOrBlankNodeRef::NamedNode(sub_prop) = super_prop_rel.subject else {
            continue;
        };

        let TermRef::NamedNode(super_prop) = super_prop_rel.object else {
            continue;
        };

        for prop in graph.triples_for_predicate(sub_prop) {
            let candidate = TripleRef::new(prop.subject, super_prop, prop.object);
            if !graph.contains(candidate) {
                triples_to_add.push(candidate.into_owned());
            }
        }
    }
}

/// T(?p1, owl:equivalentProperty, ?p2)
/// T(?x, ?p1, ?y)                      T(?x, ?p2, ?y)
/// --------------------------------------------------
/// T(?x, ?p2, ?y)                      T(?x, ?p1, ?y)
fn prp_eqp1_2(graph: &Graph, triples_to_add: &mut Vec<Triple>) {
    for equiv_rel in graph.triples_for_predicate(owl::EQUIVALENT_PROPERTY) {
        let NamedOrBlankNodeRef::NamedNode(p1) = equiv_rel.subject else {
            continue;
        };

        let TermRef::NamedNode(p2) = equiv_rel.object else {
            continue;
        };

        for prop in graph.triples_for_predicate(p1) {
            let candidate = TripleRef::new(prop.subject, p2, prop.object);
            if !graph.contains(candidate) {
                triples_to_add.push(candidate.into_owned());
            }
        }

        for prop in graph.triples_for_predicate(p2) {
            let candidate = TripleRef::new(prop.subject, p1, prop.object);
            if !graph.contains(candidate) {
                triples_to_add.push(candidate.into_owned());
            }
        }
    }
}

// T(?c1, rdfs:subClassOf, ?c2)
// T(?x, rdf:type, ?c1)
// --------------------
// T(?x, rdf:type, ?c2)
fn cax_sco(graph: &Graph, triples_to_add: &mut Vec<Triple>) {
    for class_rel in graph.triples_for_predicate(rdfs::SUB_CLASS_OF) {
        for sub_class in graph.subjects_for_predicate_object(rdf::TYPE, class_rel.subject) {
            let candidate = TripleRef::new(sub_class, rdf::TYPE, class_rel.object);
            if !graph.contains(candidate) {
                triples_to_add.push(candidate.into_owned());
            }
        }
    }
}

/// T(?c1, owl:equivalentClass, ?c2)
/// T(?x, rdf:type, ?c1)                      T(?x, rdf:type, ?c2)
/// --------------------------------------------------------------
/// T(?x, rdf:type, ?c2)                      T(?x, rdf:type, ?c1)
fn cax_eqc1_2(graph: &Graph, triples_to_add: &mut Vec<Triple>) {
    for equiv_rel in graph.triples_for_predicate(owl::EQUIVALENT_CLASS) {
        let NamedOrBlankNodeRef::NamedNode(c1) = equiv_rel.subject else {
            continue;
        };

        let TermRef::NamedNode(c2) = equiv_rel.object else {
            continue;
        };

        for subj in graph.subjects_for_predicate_object(rdf::TYPE, c1) {
            let candidate = TripleRef::new(subj, rdf::TYPE, c2);
            if !graph.contains(candidate) {
                triples_to_add.push(candidate.into_owned());
            }
        }

        for subj in graph.subjects_for_predicate_object(rdf::TYPE, c2) {
            let candidate = TripleRef::new(subj, rdf::TYPE, c1);
            if !graph.contains(candidate) {
                triples_to_add.push(candidate.into_owned());
            }
        }
    }
}

#[cfg(test)]
mod test {
    use oxrdf::{NamedNodeRef, TripleRef};

    use super::*;

    #[test]
    fn test_prp_spo1_basic() {
        let mut graph = Graph::new();
        let prop1 = NamedNodeRef::new_unchecked("http://example.com/prop1");
        let prop2 = NamedNodeRef::new_unchecked("http://example.com/prop2");
        let subject = NamedNodeRef::new_unchecked("http://example.com/subject");
        let object = NamedNodeRef::new_unchecked("http://example.com/object");

        graph.insert(TripleRef::new(prop1, rdfs::SUB_PROPERTY_OF, prop2));
        graph.insert(TripleRef::new(subject, prop1, object));

        let mut triples_to_add = Vec::new();
        prp_spo1(&graph, &mut triples_to_add);

        assert_eq!(triples_to_add.len(), 1);
        assert_eq!(triples_to_add[0], Triple::new(subject, prop2, object));
    }

    #[test]
    fn test_prp_spo1_blank_node_subject() {
        let mut graph = Graph::new();
        let prop1 = NamedNodeRef::new_unchecked("http://example.com/prop1");
        let prop2 = NamedNodeRef::new_unchecked("http://example.com/prop2");
        let blank = oxrdf::BlankNode::default();
        let object = NamedNodeRef::new_unchecked("http://example.com/object");

        graph.insert(TripleRef::new(prop1, rdfs::SUB_PROPERTY_OF, prop2));
        graph.insert(TripleRef::new(&blank, prop1, object));

        let mut triples_to_add = Vec::new();
        prp_spo1(&graph, &mut triples_to_add);

        assert_eq!(triples_to_add.len(), 1);
    }

    #[test]
    fn test_prp_spo1_multiple_properties() {
        let mut graph = Graph::new();
        let prop1 = NamedNodeRef::new_unchecked("http://example.com/prop1");
        let prop2 = NamedNodeRef::new_unchecked("http://example.com/prop2");
        let s1 = NamedNodeRef::new_unchecked("http://example.com/s1");
        let s2 = NamedNodeRef::new_unchecked("http://example.com/s2");
        let o1 = NamedNodeRef::new_unchecked("http://example.com/o1");
        let o2 = NamedNodeRef::new_unchecked("http://example.com/o2");

        graph.insert(TripleRef::new(prop1, rdfs::SUB_PROPERTY_OF, prop2));
        graph.insert(TripleRef::new(s1, prop1, o1));
        graph.insert(TripleRef::new(s2, prop1, o2));

        let mut triples_to_add = Vec::new();
        prp_spo1(&graph, &mut triples_to_add);

        assert_eq!(triples_to_add.len(), 2);
    }

    #[test]
    fn test_prp_spo1_no_matching_properties() {
        let graph = Graph::new();
        let mut triples_to_add = Vec::new();
        prp_spo1(&graph, &mut triples_to_add);

        assert_eq!(triples_to_add.len(), 0);
    }
}
