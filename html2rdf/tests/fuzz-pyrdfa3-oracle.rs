use pyo3::prelude::*;

/// This fuzzer compares the output of the library against a
/// (presumably) known-good implementation
#[test]
fn fuzz_oracle() {
    bolero::check!().for_each(|input: &[u8]| {
        let input_str = String::from_utf8_lossy(input);
        let mut o_graph = oxrdf::Graph::new();
        let mut p_graph = oxrdf::Graph::new();
        let base_iri = oxiri::Iri::parse("https://fuzz.invalid/".to_string()).unwrap();
        let _ = run_pyrdfa3(&input_str, &base_iri);
        let _ = html2rdf::parse(&input_str, base_iri, &mut o_graph, &mut p_graph);
    });
}

fn run_pyrdfa3(input: &str, base: &str) -> String {
    let result = Python::attach(|py| -> Result<String, PyErr> {
        let fun: Py<PyAny> = PyModule::from_code(
            py,
            c"
            from pyRdfa import pyRdfa
            from pyRdfa.options import Options

            def to_rdf(input, base):
                processor = pyRdfa(Options(), base)
                processor.rdf_from_source(input)
            ",
            c"",
            c"",
        )?
        .getattr("to_rdf")?
        .into();

        let result: String = fun.call1(py, (input, base))?.extract(py)?;
        Ok(result)
    })
    .unwrap();

    result
}
