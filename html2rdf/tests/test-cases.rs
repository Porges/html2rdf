use std::path::PathBuf;

use oxrdf::Graph;
use rstest::*;

mod utils;

#[rstest]
pub fn ttl_equality(
    #[base_dir = "tests/test-suite/test-cases/"]
    #[files("rdfa1.1*/*html5/*.*html")]
    // exclude vocabulary expansion tests
    #[exclude("rdfa1\\.1-vocab")]
    // - 0240, 0241, 0242 are also vocab tests
    #[exclude("024[0-2]")]
    // - 0313, 0235-0239 are processor-graph tests
    #[exclude("0313|023[5-9]")]
    // XHTML is not properly supported, yet
    #[exclude("xhtml5/(0198|0319).xhtml")]
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

    let input = std::fs::read_to_string(&path).unwrap();
    let mut output_graph = Graph::new();
    let mut processor_graph = Graph::new();
    html2rdf::process(
        &input,
        base.clone(),
        &mut output_graph,
        &mut processor_graph,
    )
    .unwrap();

    let mut ttl_graph = Graph::new();
    {
        let ttl_content = path.with_extension("ttl");
        let mut ttl = std::fs::read_to_string(&ttl_content).unwrap();
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
        let ttl = ttl.replace("\r\n", "\n");
        let ttl_rdf = oxttl::TurtleParser::new().for_slice(ttl.as_bytes());
        for triple in ttl_rdf {
            ttl_graph.insert(&triple.unwrap());
        }
    }

    let serialized = utils::serialize_graph(output_graph, base.as_str());
    let ttl_serialized = utils::serialize_graph(ttl_graph, base.as_str());

    pretty_assertions::assert_eq!(
        serialized,
        ttl_serialized,
        "The output graph does not match the test-suite Turtle."
    );

    if !processor_graph.is_empty() {
        insta::assert_snapshot!(
            format!("{relpath}_processor_graph"),
            utils::serialize_graph(processor_graph, base.as_str(),)
        );
    }
}
