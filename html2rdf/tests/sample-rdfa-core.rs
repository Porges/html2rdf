use indoc::indoc;

mod utils;

#[test]
fn vocab_expansion() {
    let input = indoc! {r#"
        <html><body>
        This document is licensed under the
        <a vocab="http://creativecommons.org/ns#"
           rel="license"
           href="http://creativecommons.org/licenses/by-nc-nd/3.0/">
           Creative Commons By-NC-ND License
        </a>.
        </body></html>
        "#};

    let (output_graph, _processor_graph) =
        utils::parse_html_base(input, utils::base(), true, false);

    let expected_ttl = indoc! {r#"
        @base <http://example.test/> .
        @prefix cc:    <http://creativecommons.org/ns#> .
        @prefix rdfa:  <http://www.w3.org/ns/rdfa#> .
        @prefix dc:    <http://purl.org/dc/terms/> .
        <> cc:license          <http://creativecommons.org/licenses/by-nc-nd/3.0/>;
           dc:license          <http://creativecommons.org/licenses/by-nc-nd/3.0/> ;
           <//web.resource.org/cc/license> <http://creativecommons.org/licenses/by-nc-nd/3.0/> ;
           rdfa:usesVocabulary  <http://creativecommons.org/ns#> .
        "#};
    // note that @base and web.resource.org were added

    utils::assert_graph_eq_ttl(utils::base(), output_graph, expected_ttl);
}

// ===================================================================
// Section 2.2: Syntax Overview — Examples
// https://www.w3.org/TR/rdfa-core/#s_syntax_examples
// ===================================================================

/// Example 2: Full IRIs in meta and link elements.
#[test]
fn example_02_meta_link_full_iris() {
    let html = indoc! {r#"
        <html>
          <head>
            <title>My home-page</title>
            <meta property="http://purl.org/dc/terms/creator" content="Mark Birbeck" />
            <link rel="http://xmlns.com/foaf/0.1/topic" href="http://www.example.com/#us" />
          </head>
          <body></body>
        </html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <> dc:creator "Mark Birbeck" ;
           foaf:topic <http://www.example.com/#us> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 3: CURIEs via prefix attribute — same triples as Example 2.
#[test]
fn example_03_prefix_shorthand() {
    let html = indoc! {r#"
        <html
          prefix="foaf: http://xmlns.com/foaf/0.1/
                  dc: http://purl.org/dc/terms/">
          <head>
            <title>My home-page</title>
            <meta property="dc:creator" content="Mark Birbeck" />
            <link rel="foaf:topic" href="http://www.example.com/#us" />
          </head>
          <body></body>
        </html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <> dc:creator "Mark Birbeck" ;
           foaf:topic <http://www.example.com/#us> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 4: @rel on an anchor with a CURIE prefix for cc:license.
#[test]
fn example_04_rel_on_anchor() {
    let html = indoc! {r#"
        <html><body>
        This document is licensed under the
        <a prefix="cc: http://creativecommons.org/ns#"
           rel="cc:license"
           href="http://creativecommons.org/licenses/by-nc-nd/3.0/">Creative Commons By-NC-ND License</a>.
        </body></html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix cc: <http://creativecommons.org/ns#> .
        <> cc:license <http://creativecommons.org/licenses/by-nc-nd/3.0/> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 5: @property with inline text.
#[test]
fn example_05_property_inline_text() {
    let html = indoc! {r#"
        <html prefix="dc: http://purl.org/dc/terms/">
          <head><title>My Home Page</title></head>
          <body>
            <h1 property="dc:title">My home-page</h1>
            <p>Last modified: 16 September 2015</p>
          </body>
        </html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        <> dc:title "My home-page" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 6: @content and @datatype for a typed literal.
#[test]
fn example_06_content_and_datatype() {
    let html = indoc! {r#"
        <html
          prefix="xsd: http://www.w3.org/2001/XMLSchema#
                  dc: http://purl.org/dc/terms/">
          <head><title>My Home Page</title></head>
          <body>
            <h1 property="dc:title">My home-page</h1>
            <p>Last modified: <span property="dc:modified"
                    content="2015-09-16T16:00:00-05:00"
                    datatype="xsd:dateTime">16 September 2015</span>.</p>
          </body>
        </html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <> dc:title "My home-page" ;
           dc:modified "2015-09-16T16:00:00-05:00"^^xsd:dateTime .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 7: @about for statements about external resources.
#[test]
fn example_07_about_external_resources() {
    let html = indoc! {r#"
        <html
          prefix="bibo: http://purl.org/ontology/bibo/
                  dc: http://purl.org/dc/terms/">
          <head><title>Books by Marco Pierre White</title></head>
          <body>
            I think White's book
            '<span about="urn:ISBN:0091808189"
                   property="dc:title">Canteen Cuisine</span>'
            is well worth getting since although it's quite advanced stuff, he
            makes it pretty easy to follow. You might also like
            <span
             about="urn:ISBN:1596913614"
             property="dc:description">White's autobiography</span>.
          </body>
        </html>
        "#};

    let expected = indoc! {r#"
        @prefix dc: <http://purl.org/dc/terms/> .
        <urn:ISBN:0091808189> dc:title "Canteen Cuisine" .
        <urn:ISBN:1596913614> dc:description "White's autobiography" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 8: @typeof to set rdf:type.
#[test]
fn example_08_typeof() {
    let html = indoc! {r#"
        <html
          prefix="bibo: http://purl.org/ontology/bibo/
                  dc: http://purl.org/dc/terms/">
          <head><title>Books by Marco Pierre White</title></head>
          <body>
            I think White's book
            '<span about="urn:ISBN:0091808189" typeof="bibo:Book"
                   property="dc:title">Canteen Cuisine</span>'
            is well worth getting since although it's quite advanced stuff, he
            makes it pretty easy to follow. You might also like
            <span
             about="urn:ISBN:1596913614"
             typeof="bibo:Book"
             property="dc:description">White's autobiography</span>.
          </body>
        </html>
        "#};

    let expected = indoc! {r#"
        @prefix dc: <http://purl.org/dc/terms/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix bibo: <http://purl.org/ontology/bibo/> .
        <urn:ISBN:0091808189> rdf:type bibo:Book ;
                              dc:title "Canteen Cuisine" .
        <urn:ISBN:1596913614> rdf:type bibo:Book ;
                              dc:description "White's autobiography" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 9: Full IRIs instead of CURIEs — same triples as Example 8.
#[test]
fn example_09_full_iris() {
    let html = indoc! {r#"
        <html>
          <head><title>Books by Marco Pierre White</title></head>
          <body>
            I think White's book
            '<span
             about="urn:ISBN:0091808189"
             typeof="http://purl.org/ontology/bibo/Book"
             property="http://purl.org/dc/terms/title">Canteen Cuisine</span>'
            is well worth getting since although it's quite advanced stuff, he
            makes it pretty easy to follow. You might also like
            <span
             about="urn:ISBN:1596913614"
             typeof="http://purl.org/ontology/bibo/Book"
             property="http://purl.org/dc/terms/description">White's autobiography</span>.
          </body>
        </html>
        "#};

    let expected = indoc! {r#"
        @prefix dc: <http://purl.org/dc/terms/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix bibo: <http://purl.org/ontology/bibo/> .
        <urn:ISBN:0091808189> rdf:type bibo:Book ;
                              dc:title "Canteen Cuisine" .
        <urn:ISBN:1596913614> rdf:type bibo:Book ;
                              dc:description "White's autobiography" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 10/11: @vocab to define a default vocabulary.
#[test]
fn example_10_11_vocab() {
    let html = indoc! {r##"
        <html><body>
        <div vocab="http://xmlns.com/foaf/0.1/" about="#me">
           My name is <span property="name">John Doe</span> and my blog is called
           <a rel="homepage" href="http://example.org/blog/">Understanding Semantics</a>.
        </div>
        </body></html>
        "##};

    let expected = indoc! {r##"
        @base <http://example.test/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix rdfa: <http://www.w3.org/ns/rdfa#> .
        <> rdfa:usesVocabulary foaf: .
        <#me> foaf:name "John Doe" ;
              foaf:homepage <http://example.org/blog/> .
        "##};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 12: @property used in place of @rel (when no @rel, @datatype, @content).
#[test]
fn example_12_property_as_rel() {
    let html = indoc! {r##"
        <html><body>
        <div vocab="http://xmlns.com/foaf/0.1/" about="#me">
           My name is <span property="name">John Doe</span> and my blog is called
           <a property="homepage" href="http://example.org/blog/">Understanding Semantics</a>.
        </div>
        </body></html>
        "##};

    let expected = indoc! {r##"
        @base <http://example.test/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix rdfa: <http://www.w3.org/ns/rdfa#> .
        <> rdfa:usesVocabulary foaf: .
        <#me> foaf:name "John Doe" ;
              foaf:homepage <http://example.org/blog/> .
        "##};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 7.3: Chaining
// https://www.w3.org/TR/rdfa-core/#s_chaining
// ===================================================================

/// Example 24: Statement chaining — @rel creates a new subject for children.
#[test]
fn example_24_chaining() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/"><body>
        <div about="http://dbpedia.org/resource/Albert_Einstein">
          <span property="foaf:name">Albert Einstein</span>
          <span property="dbp:dateOfBirth" datatype="xsd:date">1879-03-14</span>
          <div rel="dbp:birthPlace" resource="http://dbpedia.org/resource/German_Empire">
            <span property="dbp:conventionalLongName">the German Empire</span>
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <http://dbpedia.org/resource/Albert_Einstein>
            foaf:name "Albert Einstein" ;
            dbp:dateOfBirth "1879-03-14"^^xsd:date ;
            dbp:birthPlace <http://dbpedia.org/resource/German_Empire> .
        <http://dbpedia.org/resource/German_Empire>
            dbp:conventionalLongName "the German Empire" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 26/27: Incomplete triples completed by child @about values.
#[test]
fn example_26_27_incomplete_triples() {
    let html = indoc! {r#"
        <html prefix="dbp-owl: http://dbpedia.org/ontology/"><body>
        <div about="http://dbpedia.org/resource/Albert_Einstein"
             rel="dbp-owl:residence">
          <span about="http://dbpedia.org/resource/German_Empire"></span>
          <span about="http://dbpedia.org/resource/Switzerland"></span>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix dbp-owl: <http://dbpedia.org/ontology/> .
        <http://dbpedia.org/resource/Albert_Einstein>
            dbp-owl:residence <http://dbpedia.org/resource/German_Empire> ;
            dbp-owl:residence <http://dbpedia.org/resource/Switzerland> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 28: Chaining with an img element using @src.
#[test]
fn example_28_img_chaining() {
    let html = indoc! {r#"
        <html><body>
        <div about="http://dbpedia.org/resource/Albert_Einstein">
          <div rel="foaf:depiction">
            <img src="http://en.wikipedia.org/wiki/Image:Albert_Einstein_Head.jpg" />
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <http://dbpedia.org/resource/Albert_Einstein>
            foaf:depiction <http://en.wikipedia.org/wiki/Image:Albert_Einstein_Head.jpg> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 30: @property does NOT chain — child subject remains the parent.
#[test]
fn example_30_property_no_chaining() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/"><body>
        <div about="http://dbpedia.org/resource/Albert_Einstein">
          <span property="foaf:name">Albert Einstein</span>
          <span property="dbp:dateOfBirth" datatype="xsd:date">1879-03-14</span>
          <div property="dbp:birthPlace" resource="http://dbpedia.org/resource/German_Empire">
            <span property="dbp:conventionalLongName">the German Empire</span>
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <http://dbpedia.org/resource/Albert_Einstein>
            foaf:name "Albert Einstein" ;
            dbp:dateOfBirth "1879-03-14"^^xsd:date ;
            dbp:birthPlace <http://dbpedia.org/resource/German_Empire> ;
            dbp:conventionalLongName "the German Empire" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 7.4: CURIE and IRI Processing
// https://www.w3.org/TR/rdfa-core/#s_curies
// ===================================================================

/// Examples 41/42: A term and a CURIE both resolve to the same IRI.
#[test]
fn example_41_42_term_vs_curie() {
    let html = indoc! {r#"
        <html><body>
        <link rel="license" href="http://example.org/license.html" />
        </body></html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix xhv: <http://www.w3.org/1999/xhtml/vocab#> .
        <> xhv:license <http://example.org/license.html> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 41/42 variant: CURIE xhv:license produces the same triple.
#[test]
fn example_41_42_curie_variant() {
    let html = indoc! {r#"
        <html><body>
        <link rel="xhv:license" href="http://example.org/license.html" />
        </body></html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix xhv: <http://www.w3.org/1999/xhtml/vocab#> .
        <> xhv:license <http://example.org/license.html> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 43/44: Explicit blank node references via _:name CURIEs.
#[test]
fn example_43_44_blank_node_references() {
    let html = indoc! {r#"
        <html><body>
        <link about="_:john" rel="foaf:mbox" href="mailto:john@example.org" />
        <link about="_:sue" rel="foaf:mbox" href="mailto:sue@example.org" />
        <link about="_:john" rel="foaf:knows" resource="_:sue" />
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        _:john foaf:mbox <mailto:john@example.org> .
        _:sue foaf:mbox <mailto:sue@example.org> .
        _:john foaf:knows _:sue .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 8.1.1.1: The current document
// https://www.w3.org/TR/rdfa-core/#h_current_document
// ===================================================================

/// Examples 47/48: Default subject is the document IRI; metadata in head.
#[test]
fn example_47_48_current_document_subject() {
    let html = indoc! {r##"
        <html>
          <head>
            <title>Jo's Friends and Family Blog</title>
            <link rel="foaf:primaryTopic" href="#bbq" />
            <meta property="dc:creator" content="Jo" />
          </head>
          <body></body>
        </html>
        "##};

    let expected = indoc! {r##"
        @base <http://example.test/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        <> foaf:primaryTopic <#bbq> ;
           dc:creator "Jo" .
        "##};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 49/50: Property in body still refers to the document.
#[test]
fn example_49_50_property_in_body() {
    let html = indoc! {r#"
        <html>
          <head><title>Jo's Blog</title></head>
          <body>
            <h1><span property="dc:creator">Jo</span>'s blog</h1>
            <p>Welcome to my blog.</p>
          </body>
        </html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        <> dc:creator "Jo" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 8.1.1.2: Using @about
// https://www.w3.org/TR/rdfa-core/#h_about
// ===================================================================

/// Examples 53/54: @about sets a new subject; @typeof sets rdf:type.
#[test]
fn example_53_54_about_typeof() {
    let html = indoc! {r##"
        <html prefix="cal: http://www.w3.org/2002/12/cal/ical#">
          <head>
            <title>Jo's Friends and Family Blog</title>
            <link rel="foaf:primaryTopic" href="#bbq" />
            <meta property="dc:creator" content="Jo" />
          </head>
          <body>
            <p about="#bbq" typeof="cal:Vevent">
              I'm holding
              <span property="cal:summary">one last summer barbecue</span>,
              on
              <span property="cal:dtstart" content="2015-09-16T16:00:00-05:00"
                    datatype="xsd:dateTime">September 16th at 4pm</span>.
            </p>
          </body>
        </html>
        "##};

    let expected = indoc! {r##"
        @base <http://example.test/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        @prefix cal: <http://www.w3.org/2002/12/cal/ical#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <> foaf:primaryTopic <#bbq> ;
           dc:creator "Jo" .
        <#bbq> rdf:type cal:Vevent ;
               cal:summary "one last summer barbecue" ;
               cal:dtstart "2015-09-16T16:00:00-05:00"^^xsd:dateTime .
        "##};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 55/56: mailto: IRIs as subjects.
#[test]
fn example_55_56_mailto_subjects() {
    let html = indoc! {r#"
        <html><body>
        John knows
        <a about="mailto:john@example.org"
           rel="foaf:knows" href="mailto:sue@example.org">Sue</a>.

        Sue knows
        <a about="mailto:sue@example.org"
           rel="foaf:knows" href="mailto:jim@example.org">Jim</a>.
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <mailto:john@example.org> foaf:knows <mailto:sue@example.org> .
        <mailto:sue@example.org> foaf:knows <mailto:jim@example.org> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 57/58: @about with a relative IRI for stating facts about an image.
#[test]
fn example_57_58_about_photo() {
    let html = indoc! {r#"
        <html><body>
        <div about="photo1.jpg">
          this photo was taken by
          <span property="dc:creator">Mark Birbeck</span>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        <photo1.jpg> dc:creator "Mark Birbeck" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 8.1.1.3: Typing resources with @typeof
// https://www.w3.org/TR/rdfa-core/#h_typeof
// ===================================================================

/// Examples 59/60: @typeof with @about sets rdf:type on the @about resource.
#[test]
fn example_59_60_typeof_with_about() {
    let html = indoc! {r#"
        <html><body>
        <div about="http://dbpedia.org/resource/Albert_Einstein" typeof="foaf:Person">
          <span property="foaf:name">Albert Einstein</span>
          <span property="foaf:givenName">Albert</span>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        <http://dbpedia.org/resource/Albert_Einstein>
            rdf:type foaf:Person ;
            foaf:name "Albert Einstein" ;
            foaf:givenName "Albert" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 61/62: @rel + @resource + @typeof — type is assigned to the object.
#[test]
fn example_61_62_rel_resource_typeof() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/"><body>
        <div about="http://dbpedia.org/resource/Albert_Einstein">
          <div rel="dbp:birthPlace"
               resource="http://dbpedia.org/resource/German_Empire"
               typeof="http://schema.org/Country">
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        <http://dbpedia.org/resource/Albert_Einstein>
            dbp:birthPlace <http://dbpedia.org/resource/German_Empire> .
        <http://dbpedia.org/resource/German_Empire>
            rdf:type <http://schema.org/Country> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 63/64: @typeof without @about creates a bnode subject.
#[test]
fn example_63_64_typeof_bnode() {
    let html = indoc! {r#"
        <html><body>
        <div typeof="foaf:Person">
          <span property="foaf:name">Albert Einstein</span>
          <span property="foaf:givenName">Albert</span>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        _:a rdf:type foaf:Person ;
            foaf:name "Albert Einstein" ;
            foaf:givenName "Albert" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 66/67: @rel + @typeof (no @about) — bnode typed and used as object.
#[test]
fn example_66_67_rel_typeof_bnode() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/"><body>
        <div about="http://dbpedia.org/resource/Albert_Einstein">
          <div rel="dbp:birthPlace" typeof="http://schema.org/Country">
            <span property="dbp:conventionalLongName">the German Empire</span>
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        <http://dbpedia.org/resource/Albert_Einstein>
            dbp:birthPlace _:a .
        _:a rdf:type <http://schema.org/Country> ;
            dbp:conventionalLongName "the German Empire" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 68: @property + @typeof induces chaining (like @rel).
#[test]
fn example_68_property_typeof_chaining() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/"><body>
        <div about="http://dbpedia.org/resource/Albert_Einstein">
          <div property="dbp:birthPlace" typeof="http://schema.org/Country">
            <span property="dbp:conventionalLongName">the German Empire</span>
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        <http://dbpedia.org/resource/Albert_Einstein>
            dbp:birthPlace _:a .
        _:a rdf:type <http://schema.org/Country> ;
            dbp:conventionalLongName "the German Empire" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 8.1.1.4: Inheriting subject
// https://www.w3.org/TR/rdfa-core/#h_no_about_no_typeof
// ===================================================================

/// Examples 72-74: Subject inherited from parent @resource.
#[test]
fn example_72_74_inheriting_from_resource() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/
                       dbp-owl: http://dbpedia.org/ontology/"><body>
        <div about="http://dbpedia.org/resource/Albert_Einstein">
          <span property="foaf:name">Albert Einstein</span>
          <span property="dbp:dateOfBirth" datatype="xsd:date">1879-03-14</span>
          <div rel="dbp:birthPlace" resource="http://dbpedia.org/resource/German_Empire">
            <span property="dbp:conventionalLongName">the German Empire</span>
            <span rel="dbp-owl:capital" resource="http://dbpedia.org/resource/Berlin" />
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix dbp-owl: <http://dbpedia.org/ontology/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <http://dbpedia.org/resource/Albert_Einstein>
            foaf:name "Albert Einstein" ;
            dbp:dateOfBirth "1879-03-14"^^xsd:date ;
            dbp:birthPlace <http://dbpedia.org/resource/German_Empire> .
        <http://dbpedia.org/resource/German_Empire>
            dbp:conventionalLongName "the German Empire" ;
            dbp-owl:capital <http://dbpedia.org/resource/Berlin> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 75/76: @about in child completes parent's @rel.
#[test]
fn example_75_76_about_completing_rel() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/
                       dbp-owl: http://dbpedia.org/ontology/"><body>
        <div about="http://dbpedia.org/resource/Baruch_Spinoza" rel="dbp-owl:influenced">
          <div about="http://dbpedia.org/resource/Albert_Einstein">
            <span property="foaf:name">Albert Einstein</span>
            <span property="dbp:dateOfBirth" datatype="xsd:date">1879-03-14</span>
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix dbp-owl: <http://dbpedia.org/ontology/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <http://dbpedia.org/resource/Baruch_Spinoza>
            dbp-owl:influenced <http://dbpedia.org/resource/Albert_Einstein> .
        <http://dbpedia.org/resource/Albert_Einstein>
            foaf:name "Albert Einstein" ;
            dbp:dateOfBirth "1879-03-14"^^xsd:date .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 77/78: Anonymous (bnode) inherited subject.
#[test]
fn example_77_78_anonymous_inherited_subject() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/
                       dbp-owl: http://dbpedia.org/ontology/"><body>
        <div about="http://dbpedia.org/resource/Baruch_Spinoza" rel="dbp-owl:influenced">
          <div>
            <span property="foaf:name">Albert Einstein</span>
            <span property="dbp:dateOfBirth" datatype="xsd:date">1879-03-14</span>
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix dbp-owl: <http://dbpedia.org/ontology/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <http://dbpedia.org/resource/Baruch_Spinoza>
            dbp-owl:influenced _:a .
        _:a foaf:name "Albert Einstein" ;
            dbp:dateOfBirth "1879-03-14"^^xsd:date .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 79: Same as 77/78 but without the intermediate div.
#[test]
fn example_79_no_intermediate_div() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/
                       dbp-owl: http://dbpedia.org/ontology/"><body>
        <div about="http://dbpedia.org/resource/Baruch_Spinoza" rel="dbp-owl:influenced">
          <span property="foaf:name">Albert Einstein</span>
          <span property="dbp:dateOfBirth" datatype="xsd:date">1879-03-14</span>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix dbp-owl: <http://dbpedia.org/ontology/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <http://dbpedia.org/resource/Baruch_Spinoza>
            dbp-owl:influenced _:a .
        _:a foaf:name "Albert Einstein" ;
            dbp:dateOfBirth "1879-03-14"^^xsd:date .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 80: Alternative pattern — @rel on an inner div.
#[test]
fn example_80_alternative_rel_pattern() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/
                       dbp-owl: http://dbpedia.org/ontology/"><body>
        <div about="http://dbpedia.org/resource/Baruch_Spinoza">
          <div rel="dbp-owl:influenced">
            <span property="foaf:name">Albert Einstein</span>
            <span property="dbp:dateOfBirth" datatype="xsd:date">1879-03-14</span>
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix dbp-owl: <http://dbpedia.org/ontology/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <http://dbpedia.org/resource/Baruch_Spinoza>
            dbp-owl:influenced _:a .
        _:a foaf:name "Albert Einstein" ;
            dbp:dateOfBirth "1879-03-14"^^xsd:date .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 8.2: Completing incomplete triples
// https://www.w3.org/TR/rdfa-core/#s_completing_incomplete_triples
// ===================================================================

/// Examples 84/86/87: Hanging @rel completed by a nested @about.
#[test]
fn example_84_86_87_hanging_rel() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/"><body>
        <div about="http://dbpedia.org/resource/Albert_Einstein">
          <span property="foaf:name">Albert Einstein</span>
          <span property="dbp:dateOfBirth" datatype="xsd:date">1879-03-14</span>
          <div rel="dbp:birthPlace">
            <span about="http://dbpedia.org/resource/German_Empire"
                  property="dbp:conventionalLongName">the German Empire</span>
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <http://dbpedia.org/resource/Albert_Einstein>
            foaf:name "Albert Einstein" ;
            dbp:dateOfBirth "1879-03-14"^^xsd:date ;
            dbp:birthPlace <http://dbpedia.org/resource/German_Empire> .
        <http://dbpedia.org/resource/German_Empire>
            dbp:conventionalLongName "the German Empire" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 88/90: Multiple @about values completing one hanging @rel.
#[test]
fn example_88_90_multiple_about_completing() {
    let html = indoc! {r#"
        <html prefix="dbp-owl: http://dbpedia.org/ontology/"><body>
        <div about="http://dbpedia.org/resource/Albert_Einstein" rel="dbp-owl:residence">
          <span about="http://dbpedia.org/resource/German_Empire"></span>
          <span about="http://dbpedia.org/resource/Switzerland"></span>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix dbp-owl: <http://dbpedia.org/ontology/> .
        <http://dbpedia.org/resource/Albert_Einstein>
            dbp-owl:residence <http://dbpedia.org/resource/German_Empire> ;
            dbp-owl:residence <http://dbpedia.org/resource/Switzerland> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 91/93: @typeof (bnode) completing a hanging @rel.
#[test]
fn example_91_93_typeof_completing_rel() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/
                       dbp-owl: http://dbpedia.org/ontology/"><body>
        <div about="http://dbpedia.org/resource/Baruch_Spinoza">
          <div rel="dbp-owl:influenced">
            <div typeof="foaf:Person">
              <span property="foaf:name">Albert Einstein</span>
              <span property="dbp:dateOfBirth" datatype="xsd:date">1879-03-14</span>
            </div>
            <div typeof="foaf:Person">
              <span property="foaf:name">Arthur Schopenhauer</span>
              <span property="dbp:dateOfBirth" datatype="xsd:date">1788-02-22</span>
            </div>
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix dbp-owl: <http://dbpedia.org/ontology/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <http://dbpedia.org/resource/Baruch_Spinoza>
            dbp-owl:influenced _:a ;
            dbp-owl:influenced _:b .
        _:a rdf:type foaf:Person ;
            foaf:name "Albert Einstein" ;
            dbp:dateOfBirth "1879-03-14"^^xsd:date .
        _:b rdf:type foaf:Person ;
            foaf:name "Arthur Schopenhauer" ;
            dbp:dateOfBirth "1788-02-22"^^xsd:date .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 94/100: Nested hanging rels — complex completion scenario.
#[test]
fn example_94_100_nested_hanging_rels() {
    let html = indoc! {r#"
        <html prefix="dbp: http://dbpedia.org/property/
                       dbp-owl: http://dbpedia.org/ontology/"><body>
        <div about="http://dbpedia.org/resource/Baruch_Spinoza" rel="dbp-owl:influenced">
          <span property="foaf:name">Albert Einstein</span>
          <span property="dbp:dateOfBirth" datatype="xsd:date">1879-03-14</span>
          <div rel="dbp-owl:residence">
            <span about="http://dbpedia.org/resource/German_Empire"></span>
            <span about="http://dbpedia.org/resource/Switzerland"></span>
          </div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dbp: <http://dbpedia.org/property/> .
        @prefix dbp-owl: <http://dbpedia.org/ontology/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <http://dbpedia.org/resource/Baruch_Spinoza>
            dbp-owl:influenced _:a .
        _:a foaf:name "Albert Einstein" ;
            dbp:dateOfBirth "1879-03-14"^^xsd:date ;
            dbp-owl:residence <http://dbpedia.org/resource/German_Empire> ;
            dbp-owl:residence <http://dbpedia.org/resource/Switzerland> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 8.3.1: Object resolution for @property
// https://www.w3.org/TR/rdfa-core/#h_literal_objects
// ===================================================================

/// Examples 101/103: Plain literal from @content.
#[test]
fn example_101_103_plain_literal_content() {
    let html = indoc! {r#"
        <html><body>
        <meta about="http://internet-apps.blogspot.com/"
              property="dc:creator" content="Mark Birbeck" />
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix dc: <http://purl.org/dc/terms/> .
        <http://internet-apps.blogspot.com/> dc:creator "Mark Birbeck" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 102/103: Plain literal from inline text.
#[test]
fn example_102_103_plain_literal_inline() {
    let html = indoc! {r#"
        <html><body>
        <span about="http://internet-apps.blogspot.com/"
              property="dc:creator">Mark Birbeck</span>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix dc: <http://purl.org/dc/terms/> .
        <http://internet-apps.blogspot.com/> dc:creator "Mark Birbeck" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 104: @content takes precedence over inline text.
#[test]
fn example_104_content_precedence() {
    let html = indoc! {r#"
        <html><body>
        <span about="http://internet-apps.blogspot.com/"
              property="dc:creator" content="Mark Birbeck">John Doe</span>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix dc: <http://purl.org/dc/terms/> .
        <http://internet-apps.blogspot.com/> dc:creator "Mark Birbeck" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 106: Language tag on a literal (adapted for HTML5 using lang=).
#[test]
fn example_106_language_tag() {
    let html = indoc! {r#"
        <html prefix="ex: http://example.org/"><body>
        <meta about="http://example.org/node"
              property="ex:property" lang="fr" content="chat" />
        </body></html>
        "#};

    let expected = indoc! {r#"
        <http://example.org/node> <http://example.org/property> "chat"@fr .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 107: Language inheritance (adapted for HTML5 using lang=).
#[test]
fn example_107_language_inheritance() {
    let html = indoc! {r#"
        <html prefix="ex: http://example.org/" lang="fr">
          <head>
            <title lang="en">Example</title>
            <meta about="http://example.org/node"
                  property="ex:property" content="chat" />
          </head>
          <body></body>
        </html>
        "#};

    let expected = indoc! {r#"
        <http://example.org/node> <http://example.org/property> "chat"@fr .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 108/109: Typed literal via @datatype.
#[test]
fn example_108_109_typed_literal() {
    let html = indoc! {r#"
        <html prefix="cal: http://www.w3.org/2002/12/cal/ical#"><body>
        <span property="cal:dtstart" content="2015-09-16T16:00:00-05:00"
              datatype="xsd:dateTime">September 16th at 4pm</span>.
        </body></html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix cal: <http://www.w3.org/2002/12/cal/ical#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        <> cal:dtstart "2015-09-16T16:00:00-05:00"^^xsd:dateTime .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 113/114: Empty @datatype forces a plain literal (strips XML markup).
#[test]
fn example_113_114_empty_datatype() {
    let html = indoc! {r#"
        <html><body>
        <p>You searched for <strong>Einstein</strong>:</p>
        <p about="http://dbpedia.org/resource/Albert_Einstein">
          <span property="foaf:name" datatype="">Albert <strong>Einstein</strong></span>
          (b. March 14, 1879, d. April 18, 1955) was a German-born theoretical physicist.
        </p>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <http://dbpedia.org/resource/Albert_Einstein> foaf:name "Albert Einstein" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 8.3.2: IRI object resolution
// https://www.w3.org/TR/rdfa-core/#h_IRI_objects
// ===================================================================

/// Examples 115/116: @resource to set the object of a relationship.
#[test]
fn example_115_116_resource_object() {
    let html = indoc! {r##"
        <html>
          <head>
            <title>On Crime and Punishment</title>
            <base href="http://www.example.com/candp.xhtml" />
          </head>
          <body>
            <blockquote about="#q1" rel="dc:source" resource="urn:ISBN:0140449132">
              <p id="q1">
                Rodion Romanovitch! My dear friend! If you go on in this way
                you will go mad, I am positive! Drink, pray, if only a few drops!
              </p>
            </blockquote>
          </body>
        </html>
        "##};

    let expected = indoc! {r#"
        @prefix dc: <http://purl.org/dc/terms/> .
        <http://www.example.com/candp.xhtml#q1>
            dc:source <urn:ISBN:0140449132> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 117: @href sets the object when no @resource is present.
#[test]
fn example_117_href_object() {
    let html = indoc! {r#"
        <html><body>
        <link about="mailto:john@example.org"
              rel="foaf:knows" href="mailto:sue@example.org" />
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <mailto:john@example.org> foaf:knows <mailto:sue@example.org> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 118/119: @src and @rev with @rel — bi-directional relationships.
#[test]
fn example_118_119_src_rev_rel() {
    let html = indoc! {r#"
        <html><body>
        <img about="http://www.blogger.com/profile/1109404"
             src="photo1.jpg" rev="dc:creator" rel="foaf:img"/>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <photo1.jpg> dc:creator <http://www.blogger.com/profile/1109404> .
        <http://www.blogger.com/profile/1109404> foaf:img <photo1.jpg> .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 8.4: List Generation
// https://www.w3.org/TR/rdfa-core/#h_sequence
// ===================================================================

/// Example 123: @inlist to generate an ordered RDF list.
#[test]
fn example_123_list_generation() {
    let html = indoc! {r#"
        <html><body>
        <p prefix="bibo: http://purl.org/ontology/bibo/ dc: http://purl.org/dc/terms/"
           typeof="bibo:Chapter">
          "<span property="dc:title">Semantic Annotation and Retrieval</span>" by
           <a inlist="" property="dc:creator"
                        href="http://ben.adida.net/#me">Ben Adida</a>,
           <a inlist="" property="dc:creator"
                        href="http://twitter.com/markbirbeck">Mark Birbeck</a>, and
           <a inlist="" property="dc:creator"
                        href="http://www.ivan-herman.net/foaf#me">Ivan Herman</a>.
        </p>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix bibo: <http://purl.org/ontology/bibo/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        _:chapter rdf:type bibo:Chapter ;
                  dc:title "Semantic Annotation and Retrieval" ;
                  dc:creator ( <http://ben.adida.net/#me>
                               <http://twitter.com/markbirbeck>
                               <http://www.ivan-herman.net/foaf#me> ) .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 124/125: @inlist with a mix of resources and literals.
#[test]
fn example_124_125_list_mixed() {
    let html = indoc! {r#"
        <html><body>
        <p prefix="bibo: http://purl.org/ontology/bibo/ dc: http://purl.org/dc/terms/"
           typeof="bibo:Chapter">
          "<span property="dc:title">Semantic Annotation and Retrieval</span>", by
          <span inlist="" property="dc:creator" resource="http://ben.adida.net/#me">Ben Adida</span>,
          <span inlist="" property="dc:creator">Mark Birbeck</span>, and
          <span inlist="" property="dc:creator" resource="http://www.ivan-herman.net/foaf#me">Ivan Herman</span>.
        </p>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix bibo: <http://purl.org/ontology/bibo/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        _:chapter rdf:type bibo:Chapter ;
                  dc:title "Semantic Annotation and Retrieval" ;
                  dc:creator ( <http://ben.adida.net/#me>
                               "Mark Birbeck"
                               <http://www.ivan-herman.net/foaf#me> ) .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Examples 127/128: @inlist with @rel and incomplete triples.
#[test]
fn example_127_128_list_incomplete_triples() {
    let html = indoc! {r#"
        <html><body>
        <p prefix="bibo: http://purl.org/ontology/bibo/ dc: http://purl.org/dc/terms/"
           typeof="bibo:Chapter">
          "<span property="dc:title">Semantic Annotation and Retrieval</span>", by
           <span rel="dc:creator" inlist="">
             <a href="http://ben.adida.net/#me">Ben Adida</a>,
             <a href="http://internet-apps.blogspot.com/2008/03/my-profile.html#me">Mark Birbeck</a>, and
             <a href="http://www.ivan-herman.net/foaf#me">Ivan Herman</a>.
           </span>
        </p>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix bibo: <http://purl.org/ontology/bibo/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        _:chapter rdf:type bibo:Chapter ;
                  dc:title "Semantic Annotation and Retrieval" ;
                  dc:creator ( <http://ben.adida.net/#me>
                               <http://internet-apps.blogspot.com/2008/03/my-profile.html#me>
                               <http://www.ivan-herman.net/foaf#me> ) .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 129: Empty list expressed via rdf:nil.
#[test]
fn example_129_empty_list() {
    let html = indoc! {r#"
        <html prefix="ex: http://example.org/"><body>
        <span rel="ex:prop" resource="rdf:nil"/>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        <> <http://example.org/prop> rdf:nil .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 7.4.1: Scoping of Prefix Mappings
// https://www.w3.org/TR/rdfa-core/#s_curies_scoping
// ===================================================================

/// Example 35: Prefix mappings are scoped — same prefix, different IRIs.
#[test]
fn example_35_prefix_scoping() {
    let html = indoc! {r#"
        <html><body>
        <div prefix="dbr: http://dbpedia.org/resource/">
          <div about="dbr:Albert_Einstein" property="foaf:name">Albert</div>
        </div>
        <div prefix="dbr: http://someotherdb.org/resource/">
          <div about="dbr:Albert_Einstein" property="foaf:name">Other Albert</div>
        </div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <http://dbpedia.org/resource/Albert_Einstein> foaf:name "Albert" .
        <http://someotherdb.org/resource/Albert_Einstein> foaf:name "Other Albert" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 7.4.2: General Use of CURIEs — edge cases
// https://www.w3.org/TR/rdfa-core/#s_curies_general
// ===================================================================

/// Example 39: Invalid safe CURIE — value is ignored, no new subject.
#[test]
fn example_39_invalid_safe_curie() {
    let html = indoc! {r#"
        <html><body>
        <div about="[Albert_Einstein]" property="foaf:name">Albert</div>
        </body></html>
        "#};

    // The [Albert_Einstein] is an invalid safe CURIE (no prefix).
    // about is ignored; subject falls back to parent (document).
    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <> foaf:name "Albert" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

/// Example 40: Bare relative IRI in @about is valid.
#[test]
fn example_40_relative_iri_in_about() {
    let html = indoc! {r#"
        <html><body>
        <div about="Albert_Einstein" property="foaf:name">Albert</div>
        </body></html>
        "#};

    let expected = indoc! {r#"
        @base <http://example.test/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        <Albert_Einstein> foaf:name "Albert" .
        "#};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 8.1.1.1: base element
// https://www.w3.org/TR/rdfa-core/#h_current_document
// ===================================================================

/// Examples 51/52: The base element overrides the document IRI.
#[test]
fn example_51_52_base_element() {
    let html = indoc! {r##"
        <html>
          <head>
            <base href="http://www.example.org/jo/blog" />
            <title>Jo's Friends and Family Blog</title>
            <link rel="foaf:primaryTopic" href="#bbq" />
            <meta property="dc:creator" content="Jo" />
          </head>
          <body></body>
        </html>
        "##};

    let expected = indoc! {r##"
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix dc: <http://purl.org/dc/terms/> .
        <http://www.example.org/jo/blog> foaf:primaryTopic <http://www.example.org/jo/blog#bbq> ;
                                         dc:creator "Jo" .
        "##};

    utils::assert_ttl(utils::base(), html, expected);
}

// ===================================================================
// Section 8.3.1.3: XML Literals
// https://www.w3.org/TR/rdfa-core/#h_xml_literals
// ===================================================================

/// Examples 111/112: XML literal via `datatype="rdf:XMLLiteral"`.
#[test]
fn example_111_112_xml_literal() {
    let html = indoc! {r#"
        <html><body>
        <h2 property="dc:title" datatype="rdf:XMLLiteral">E = mc<sup>2</sup>: The Most Urgent Problem of Our Time</h2>
        </body></html>
        "#};

    let (output_graph, _processor_graph) = utils::parse_html(html);

    // Verify the triple exists with the correct datatype
    let dc_title = oxrdf::NamedNodeRef::new_unchecked("http://purl.org/dc/terms/title");
    let rdf_xmlliteral =
        oxrdf::NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral");
    let base_node = oxrdf::NamedNodeRef::new_unchecked("http://example.test/");

    let matching: Vec<_> = output_graph
        .triples_for_subject(base_node)
        .filter(|t| t.predicate == dc_title)
        .collect();
    assert_eq!(matching.len(), 1, "Expected exactly one dc:title triple");

    if let oxrdf::TermRef::Literal(lit) = matching[0].object {
        assert_eq!(
            lit.datatype(),
            rdf_xmlliteral,
            "Expected XMLLiteral datatype"
        );
        assert!(
            lit.value().contains("<sup>"),
            "Expected XML markup in literal, got: {}",
            lit.value()
        );
    } else {
        panic!("Expected literal object for dc:title");
    }
}

// ===================================================================
// Section 10: Vocabulary Expansion (Examples 130-132)
// Already covered by the vocab_expansion test above.
// ===================================================================
