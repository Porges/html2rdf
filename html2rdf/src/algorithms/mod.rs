//! Supplementary algorithms used in RDFa.

mod property_copying;
mod vocabulary_expansion;

pub use property_copying::property_copying;
pub use vocabulary_expansion::{
    OfflineVocabularyResolver, OnlineVocabularyResolver, VocabularyResolver, vocabulary_expansion,
};
