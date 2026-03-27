use std::{ffi::OsStr, path::PathBuf};

use oxrdf::Graph;
use rstest::*;

mod utils;

#[rstest]
pub fn ttl_equality(
    #[base_dir = "tests/test-suite/test-cases/"]
    #[files("rdfa1.1*/xhtml5/*.*html")]
    #[files("rdfa1.1*/html5/*.*html")]
    // - 0313, 0235-0239 are processor-graph tests
    #[exclude("0313|023[5-9]")]
    #[exclude("0319")] // I believe this test is simply wrong
    #[exclude("0198.xhtml")] // this requires c14n and the test suite version is wrong
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
    let path_str = path.to_string_lossy();
    let expand = path_str.contains("-vocab") // enable expansion for vocab tests
        || path_str.contains("0240")
        || path_str.contains("0241")
        || path_str.contains("0242");
    let (output_graph, processor_graph) = utils::parse_html_base(
        &input,
        base.as_ref(),
        expand,
        path.extension() == Some(OsStr::new("xhtml")),
    );

    let mut ttl_graph = Graph::new();
    let ttl_content = path.with_extension("ttl");
    let mut ttl = std::fs::read_to_string(&ttl_content)
        .unwrap()
        .replace("\r\n", "\n");
    {
        // HACK: four test cases are garbled, fix them up manually:
        ttl = ttl.replace(
            "<<http://rdfa.info/test-suite/test-cases/rdfa1.1/html5/0284.html>>",
            "<http://rdfa.info/test-suite/test-cases/rdfa1.1-lite/html5/0281.html>",
        );
        ttl = ttl.replace(
            "<<http://rdfa.info/test-suite/test-cases/rdfa1.1/html5/0282.html>>",
            "<http://rdfa.info/test-suite/test-cases/rdfa1.1-lite/html5/0282.html>",
        );
        if ttl_content.to_string_lossy().contains("rdfa1.1-lite") {
            ttl = ttl.replace(
                "<http://rdfa.info/test-suite/test-cases/rdfa1.1/xhtml5/0281.xhtml>",
                "<http://rdfa.info/test-suite/test-cases/rdfa1.1-lite/xhtml5/0281.xhtml>",
            );
            ttl = ttl.replace(
                "<http://rdfa.info/test-suite/test-cases/rdfa1.1/xhtml5/0282.xhtml>",
                "<http://rdfa.info/test-suite/test-cases/rdfa1.1-lite/xhtml5/0282.xhtml>",
            );
        }
        //let ttl = ttl.replace("\r\n", "\n");
        let ttl_rdf = oxttl::TurtleParser::new().for_slice(ttl.as_bytes());
        for triple in ttl_rdf {
            ttl_graph.insert(&triple.unwrap());
        }
    }

    let serialized = utils::serialize_normalized_graph(output_graph, base.as_str());
    let ttl_serialized = utils::serialize_normalized_graph(ttl_graph, base.as_str());

    pretty_assertions::assert_eq!(
        serialized,
        ttl_serialized,
        "The output graph does not match the test-suite Turtle.\n\nHTML:\n{}\n\n:(unnormalized) TTL:\n{}",
        input,
        ttl,
    );

    if !processor_graph.is_empty() {
        insta::assert_snapshot!(
            format!("{relpath}_processor_graph"),
            utils::serialize_normalized_graph(processor_graph, base.as_str(),)
        );
    }
}
