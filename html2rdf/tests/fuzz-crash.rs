use html2rdf::host_language::Html5;

/// This fuzzer simply tries to crash the parser.
#[test]
fn fuzz_crash() {
    bolero::check!().for_each(|input: &[u8]| {
        let input_str = String::from_utf8_lossy(input);
        let base_iri = oxiri::Iri::parse("https://fuzz.invalid/").unwrap();
        (_, ()) =
            html2rdf::doc_to_graphs(&input_str, base_iri, html2rdf::Options::<Html5>::default())
                .unwrap_or_else(|inf| match inf {});
    });
}
