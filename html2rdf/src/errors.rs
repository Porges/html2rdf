use curie::ExpansionError;
use oxiri::IriParseError;

use crate::graphs::{PGClass, ProcessorGraphError};

/// Supertype of all Errors that can be produced by this crate.
#[derive(thiserror::Error, Debug)]
#[error(transparent)]
pub enum Error {
    IriError(#[from] IriError),
    CurieError(#[from] CurieError),
    TermError(#[from] InvalidTerm),
}

#[derive(derive_more::From, thiserror::Error, Debug)]
pub enum CurieOrIriError {
    #[error(transparent)]
    Curie(CurieError),
    #[error(transparent)]
    Iri(IriError),
}

#[derive(derive_more::Error, derive_more::Display, Debug)]
pub enum CurieError {
    #[display("Invalid empty CURIE")]
    EmptyCurie,
    #[display("Invalid CURIE: [{value}] (invalid blank node suffix `{suffix}`)")]
    InvalidBlankNodeSuffix { value: String, suffix: String },
    #[display("Expanding CURIE [{value}] resulted in an invalid IRI")]
    InvalidIRI {
        #[error(source)]
        source: IriError,
        value: String,
    },
    #[display("Invalid CURIE: [{value}] ({})",
        match source {
            ExpansionError::MissingDefault => "no default prefix is available in RDFa",
            ExpansionError::Invalid => "no such prefix defined",
        })]
    ExpansionError {
        value: String,
        #[error(not(source))]
        source: curie::ExpansionError,
    },
}

#[derive(derive_more::From, thiserror::Error, Debug)]
pub enum TermOrIriError {
    #[error(transparent)]
    Term(InvalidTerm),
    #[error(transparent)]
    Iri(IriError),
}

#[derive(derive_more::Error, derive_more::Display, Debug)]
#[display("Invalid IRI: <{value}>")]
pub struct IriError {
    pub value: String,
    #[error(source)]
    pub source: IriParseError,
}

impl ProcessorGraphError for IriError {
    fn message_class(&self) -> Option<PGClass> {
        Some(PGClass::Warning)
    }
}

impl ProcessorGraphError for CurieError {
    fn message_class(&self) -> Option<PGClass> {
        match self {
            // > NOTE
            // > ... when the value of an attribute of this datatype is an empty SafeCURIE
            // > (e.g., @about="[]"), that value does not result in an IRI and therefore
            // > the value is ignored.
            CurieError::EmptyCurie => None,
            // we ignore this and don't report
            CurieError::ExpansionError {
                source: ExpansionError::MissingDefault,
                ..
            } => None,
            // this, however, is reported
            CurieError::ExpansionError {
                source: ExpansionError::Invalid,
                ..
            } => Some(PGClass::UnresolvedCurie),
            CurieError::InvalidBlankNodeSuffix { .. } => Some(PGClass::UnresolvedCurie),
            CurieError::InvalidIRI { .. } => Some(PGClass::UnresolvedCurie),
        }
    }
}

impl ProcessorGraphError for CurieOrIriError {
    fn message_class(&self) -> Option<PGClass> {
        match self {
            CurieOrIriError::Curie(err) => err.message_class(),
            CurieOrIriError::Iri(err) => err.message_class(),
        }
    }
}

impl ProcessorGraphError for TermOrIriError {
    fn message_class(&self) -> Option<PGClass> {
        match self {
            TermOrIriError::Term(err) => err.message_class(),
            TermOrIriError::Iri(err) => err.message_class(),
        }
    }
}

#[derive(derive_more::Error, derive_more::Display, Debug)]
#[display("Invalid term: `{term}`")]
pub struct InvalidTerm {
    pub term: String,
}

impl ProcessorGraphError for InvalidTerm {
    fn message_class(&self) -> Option<PGClass> {
        Some(PGClass::UnresolvedTerm)
    }
}
