#![cfg(not(target_os = "windows"))]
// ^ don't want to mess around with getting librdfa to run on Windows

use html2rdf::{Options, host_language::Html5};
use oxrdf::Graph;
use pyo3::prelude::*;

mod utils;

// NB: needs to be run with 'ASAN_OPTIONS=detect_leaks=0'
// NB: this is not really yet functional because of differences that are hard to reconcile

/// This fuzzer compares the output of the library against
/// two existing implementations.
///
/// html2rdf is considered correct if _either_ of them match.
/// This might seem unusual, but the libraries disagree on some
/// key examples, and both seem to be missing some functionality.
///
/// librdfa:
/// - does not implement the content-sniffing used to determine a datatype for time values
/// - does not read @datetime on <time>
#[test]
#[ignore = "not yet working - need to loosen IRI parsing"]
fn fuzz_oracle() {
    Python::initialize();
    let base = oxiri::Iri::parse("https://rdfa.test/").unwrap();
    bolero::check!().for_each(|input: &[u8]| {
        let input_str = String::from_utf8_lossy(input);
        let (my_output, my_processor) =
            html2rdf::doc_to_graphs::<Html5, Graph>(&input_str, base.clone(), Options::default())
                .unwrap();

        // try this first cause it's faster
        let librdfa_output = run_librdfa(&input_str, base.as_str());
        let librdfa_ok = utils::check_graphs_eq(&my_output, &librdfa_output);
        if librdfa_ok {
            return;
        }

        let pyrdfa3_output = run_pyrdfa3(&input_str, base.as_str()).unwrap();
        let pyrdfa3_ok = utils::check_graphs_eq(&my_output, &pyrdfa3_output);
        if pyrdfa3_ok {
            return;
        }

        eprintln!(
            "Processor graph:\n{}",
            utils::serialize_graph(&my_processor, base.as_str())
        );

        // neither matched, so assert against both to get a diff
        // it's okay to be slow here since this is the fail case
        utils::assert_graphs_eq(base, &my_output, &pyrdfa3_output, "pyrdfa3 disagreed");
        utils::assert_graphs_eq(base, &my_output, &librdfa_output, "librdfa disagreed");
    });
}

fn run_librdfa(input: &str, base: &str) -> Graph {
    let mut output = Graph::new();
    let mut callback = |triple: oxrdf::TripleRef| {
        output.insert(triple);
    };
    librdfa_wrapper::run(base, input, &mut callback);
    output
}

fn run_pyrdfa3(input: &str, base: &str) -> Result<Graph, PyErr> {
    let ttl = Python::attach(|py| -> Result<String, PyErr> {
        let fun: Py<PyAny> = PyModule::from_code(
            py,
            cr#"
import rdflib
from rdflib import Graph
from pyRdfa import pyRdfa
from pyRdfa.options import Options
from pyRdfa.host import HostLanguage
from io import StringIO

# this stops rdflib from normalizing e.g. XSD values
rdflib.NORMALIZE_LITERALS = False

def to_rdf(input, base):
    graph = Graph()
    opts = Options()
    opts.host_language = HostLanguage.html5
    processor = pyRdfa(opts, base=base, rdfa_version='1.1')
    processor.graph_from_source(StringIO(input), graph, False)
    return graph.serialize(format='turtle')
"#,
            c"",
            c"",
        )?
        .getattr("to_rdf")?
        .into();

        let ttl: String = fun.call1(py, (input, base))?.extract(py)?;
        Ok(ttl)
    })?;

    let parser = oxttl::TurtleParser::new().for_slice(ttl.as_bytes());
    let mut output = Graph::new();
    for triple in parser {
        output.insert(&triple.unwrap());
    }
    Ok(output)
}
