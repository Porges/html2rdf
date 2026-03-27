pub mod dc {
    pub static DESCRIPTION: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://purl.org/dc/terms/description");
}

pub mod xhv {
    pub static ROLE: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/1999/xhtml/vocab#role");

    pub static LICENSE: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/1999/xhtml/vocab#license");
}

pub mod rdfa {
    pub static COPY: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#copy");

    pub static PATTERN: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#Pattern");

    pub static ERROR: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#Error");

    pub static WARNING: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#Warning");

    pub static DOCUMENT_ERROR: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#DocumentError");

    pub static VOCAB_REFERENCE_ERROR: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#VocabReferenceError");

    pub static UNRESOLVED_CURIE: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#UnresolvedCurie");

    pub static UNRESOLVED_TERM: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#UnresolvedTerm");

    pub static PREFIX_REDEFINITION: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#PrefixRedefinition");

    pub static CONTEXT_PROPERTY: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#context");

    pub static USES_VOCABULARY: oxrdf::NamedNodeRef =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/ns/rdfa#usesVocabulary");
}
