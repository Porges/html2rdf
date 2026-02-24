/// This fuzzer simply tries to crash the parser.
#[test]
fn fuzz_crash() {
    bolero::check!().for_each(|input: &[u8]| {
        let input_str = String::from_utf8_lossy(input);
        let mut o_graph = oxrdf::Graph::new();
        let mut p_graph = oxrdf::Graph::new();
        let base_iri = oxiri::Iri::parse("https://fuzz.invalid/".to_string()).unwrap();
        let _ = html2rdf::parse(&input_str, base_iri, &mut o_graph, &mut p_graph);
    });
}
