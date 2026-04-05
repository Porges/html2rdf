use std::{ffi::OsStr, path::PathBuf, sync::OnceLock};

use rstest::*;

mod utils;

#[rstest]
pub fn ttl_equality(
    #[base_dir = "tests/test-suite/test-cases/"]
    #[files("rdfa1.1*/xhtml5/*.*html")]
    #[files("rdfa1.1*/xhtml5-invalid/*.*html")]
    #[files("rdfa1.1*/html5/*.*html")]
    #[files("rdfa1.1*/html5-invalid/*.*html")]
    // I believe this test is simply wrong
    #[exclude("0319")]
    // these requires c14n and the test suite version is wrong
    #[exclude("0198.xhtml")]
    path: PathBuf,
) {
    let relpath = path
        .as_path()
        .to_string_lossy()
        .replace("\\", "/")
        .split_once("test-suite/test-cases/")
        .unwrap()
        .1
        .to_string();

    // construct the base to match the website version
    let base =
        oxiri::Iri::parse(format!("http://rdfa.info/test-suite/test-cases/{relpath}")).unwrap();

    let input = std::fs::read_to_string(&path)
        .unwrap()
        .replace("\r\n", "\n");

    let manifest_entry = test_manifest()
        .graph
        .iter()
        .find(|entry| base.as_str().contains(&entry.num))
        .unwrap();

    let expand_vocab = manifest_entry
        .query_param
        .as_deref()
        .unwrap_or_default()
        .contains("vocab_expansion=true");

    let target_processor_graph = manifest_entry
        .query_param
        .as_deref()
        .unwrap_or_default()
        .contains("rdfagraph=processor");

    let (output_graph, processor_graph) = utils::parse_html_base(
        &input,
        base.as_ref(),
        expand_vocab,
        path.extension() == Some(OsStr::new("xhtml")),
    );

    // the SPARQL file defines the expected results
    let sparql_path = path.with_extension("sparql");
    let sparql = std::fs::read_to_string(&sparql_path)
        .unwrap()
        .replace("\r\n", "\n");

    let target_graph = if target_processor_graph {
        &processor_graph
    } else {
        &output_graph
    };
    let result = utils::ask(target_graph, &sparql);
    let expected = manifest_entry.expected_results;
    assert_eq!(
        result,
        expected,
        "SPARQL ASK returned {result}, expected {expected}\n\nSPARQL:\n{sparql}\n\nHTML:\n{input}\n\nGraph:\n{}",
        utils::serialize_graph(target_graph, base.as_str()),
    );

    insta::assert_snapshot!(
        format!("{relpath}_processor_graph"),
        utils::serialize_normalized_graph(processor_graph, base.as_str(),)
    );
}

#[derive(serde::Deserialize)]
struct TestManifest {
    #[serde(rename = "@graph")]
    graph: Vec<TestEntry>,
}

#[derive(serde::Deserialize)]
struct TestEntry {
    num: String,
    #[serde(rename = "queryParam")]
    query_param: Option<String>,
    #[serde(rename = "expectedResults")]
    expected_results: bool,
}

fn test_manifest() -> &'static TestManifest {
    static MANIFEST: OnceLock<TestManifest> = OnceLock::new();
    MANIFEST.get_or_init(|| {
        let path =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/test-suite/manifest.jsonld");
        let manifest_str = std::fs::read_to_string(&path).unwrap();
        serde_json::from_str(&manifest_str).unwrap()
    })
}
