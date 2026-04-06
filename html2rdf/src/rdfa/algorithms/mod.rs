//! Supplementary algorithms used in RDFa.

mod property_copying;
mod vocabulary_expansion;

pub use property_copying::property_copying;

#[cfg(feature = "html")]
pub use vocabulary_expansion::OfflineVocabularyResolver;

#[cfg(feature = "vocab-online")]
pub use vocabulary_expansion::OnlineVocabularyResolver;

pub use vocabulary_expansion::{VocabularyResolver, vocabulary_expansion};
