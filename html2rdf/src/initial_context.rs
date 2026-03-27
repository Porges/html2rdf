//! Provides definitions for data from the
//! [_RDFa Core Initial Context_](https://www.w3.org/2011/rdfa-context/rdfa-1.1).

use std::{collections::BTreeMap, sync::OnceLock};

use curie::PrefixMapping;

use crate::vocab::xhtml;

pub fn terms() -> &'static BTreeMap<String, oxrdf::NamedNode> {
    // https://www.w3.org/2011/rdfa-context/rdfa-1.1
    // Vocabulary terms
    static INITIAL_CONTEXT: OnceLock<BTreeMap<String, oxrdf::NamedNode>> = OnceLock::new();
    INITIAL_CONTEXT.get_or_init(|| {
        [
            (
                "describedBy".to_string(),
                oxrdf::NamedNode::new_unchecked("http://www.w3.org/2007/05/powder-s#describedby"),
            ),
            ("license".to_string(), xhtml::LICENSE.into()),
            ("role".to_string(), xhtml::ROLE.into()),
        ]
        .into_iter()
        .collect()
    })
}

pub fn prefixes() -> &'static PrefixMapping {
    static INITIAL_CONTEXT: OnceLock<PrefixMapping> = OnceLock::new();
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
            mapping.add_prefix(prefix, iri).unwrap(); // UNWRAP: this is tested every time it runs
        }
        mapping
    })
}
