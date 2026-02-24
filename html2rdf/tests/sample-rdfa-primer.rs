// Samples from the RDFa 1.1 Primer: https://www.w3.org/TR/rdfa-primer/
use html2rdf::process;
use oxiri::Iri;
use oxrdf::Graph;

mod utils;

static BASE: &str = "http://example.com/alice/posts/trouble_with_bob";

fn process_html(html: &str) -> (Graph, Graph) {
    let base_iri = Iri::parse(BASE.to_string()).unwrap();
    let mut output_graph = Graph::new();
    let mut processor_graph = Graph::new();
    process(html, base_iri, &mut output_graph, &mut processor_graph).unwrap();
    (output_graph, processor_graph)
}

#[test]
fn example_02_hints_on_social_networking_sites() {
    let html = r#"<html>
    <head>...</head>
    <body>
      <h2 property="http://purl.org/dc/terms/title">The Trouble with Bob</h2>
      <p>Date: <span property="http://purl.org/dc/terms/created">2011-09-10</span></p>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix dc: <//purl.org/dc/terms/> .
    <> dc:created "2011-09-10" ;
    	dc:title "The Trouble with Bob" .
    "#);
}

#[test]
fn example_04_links_with_flavor() {
    let html = r#"<html>
    <head>...</head>
    <body>
      <p>All content on this site is licensed under
         <a property="http://creativecommons.org/ns#license"
            href="http://creativecommons.org/licenses/by/3.0/">
           a Creative Commons License</a>. ©2011 Alice Birpemswick.</p>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix cc: <//creativecommons.org/ns#> .
    <> cc:license <//creativecommons.org/licenses/by/3.0/> .
    ");
}

#[test]
fn example_06_default_vocabulary() {
    let html = r#"<html>
    <head>...</head>
    <body vocab="http://purl.org/dc/terms/">
      <h2 property="title">The Trouble with Bob</h2>
      <p>Date: <span property="created">2011-09-10</span></p>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <> dc:created "2011-09-10" ;
    	dc:title "The Trouble with Bob" ;
    	rdfa:usesVocabulary dc: .
    "#);
}

#[test]
fn example_07_vocab_mixed_with_full_uri() {
    let html = r#"<html>
    <head>...</head>
    <body vocab="http://purl.org/dc/terms/">
      <h2 property="title">The Trouble with Bob</h2>
      <p>Date: <span property="http://purl.org/dc/terms/created">2011-09-10</span></p>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <> dc:created "2011-09-10" ;
    	dc:title "The Trouble with Bob" ;
    	rdfa:usesVocabulary dc: .
    "#);
}

#[test]
fn example_08_vocab_mixed_with_full_uri() {
    let html = r#"<html>
    <head>...</head>
    <body vocab="http://purl.org/dc/terms/">
      <h2 property="title">The Trouble with Bob</h2>
      <p>Date: <span property="created">2011-09-10</span></p>
      <p>All content on this site is licensed under
        <a property="http://creativecommons.org/ns#license"
           href="http://creativecommons.org/licenses/by/3.0/">
          a Creative Commons License</a>. ©2011 Alice Birpemswick.</p>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix cc: <//creativecommons.org/ns#> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <> cc:license <//creativecommons.org/licenses/by/3.0/> ;
    	dc:created "2011-09-10" ;
    	dc:title "The Trouble with Bob" ;
    	rdfa:usesVocabulary dc: .
    "#);
}

#[test]
fn example_09_nested_vocab_override() {
    let html = r#"<html>
    <head>...</head>
    <body vocab="http://purl.org/dc/terms/">
      <h2 property="title">The Trouble with Bob</h2>
      <p>Date: <span property="created">2011-09-10</span></p>
      <p vocab="http://creativecommons.org/ns#">All content on this site is licensed under
        <a property="license" href="http://creativecommons.org/licenses/by/3.0/">
          a Creative Commons License</a>. ©2011 Alice Birpemswick.</p>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix cc: <//creativecommons.org/ns#> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <> cc:license <//creativecommons.org/licenses/by/3.0/> ;
    	dc:created "2011-09-10" ;
    	dc:title "The Trouble with Bob" ;
    	rdfa:usesVocabulary cc: , dc: .
    "#);
}

#[test]
fn example_10_multiple_items_per_page() {
    let html = r#"<html>
    <head>...</head>
    <body vocab="http://purl.org/dc/terms/">
       <div resource="/alice/posts/trouble_with_bob">
          <h2 property="title">The trouble with Bob</h2>
          <p>Date: <span property="created">2011-09-10</span></p>
          <h3 property="creator">Alice</h3>
       </div>
       <div resource="/alice/posts/jos_barbecue">
          <h2 property="title">Jo's Barbecue</h2>
          <p>Date: <span property="created">2011-09-14</span></p>
          <h3 property="creator">Eve</h3>
       </div>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <jos_barbecue> dc:created "2011-09-14" ;
    	dc:creator "Eve" ;
    	dc:title "Jo's Barbecue" .
    <> dc:created "2011-09-10" ;
    	dc:creator "Alice" ;
    	dc:title "The trouble with Bob" ;
    	rdfa:usesVocabulary dc: .
    "#);
}

#[test]
fn example_11_nested_resource() {
    let html = r#"<html>
    <head>...</head>
    <body vocab="http://purl.org/dc/terms/">
      <div resource="/alice/posts/trouble_with_bob">
        <h2 property="title">The trouble with Bob</h2>
        <div resource="http://example.com/bob/photos/sunset.jpg">
          <img src="http://example.com/bob/photos/sunset.jpg" />
          <span property="title">Beautiful Sunset</span>
          by <span property="creator">Bob</span>.
        </div>
      </div>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <> dc:title "The trouble with Bob" ;
    	rdfa:usesVocabulary dc: .
    </bob/photos/sunset.jpg> dc:creator "Bob" ;
    	dc:title "Beautiful Sunset" .
    "#);
}

#[test]
fn example_15_contact_info_typeof() {
    let html = r#"<html>
    <head>...</head>
    <body>
      <div vocab="http://xmlns.com/foaf/0.1/" typeof="Person">
        <p>
          <span property="name">Alice Birpemswick</span>,
          Email: <a property="mbox" href="mailto:alice@example.com">alice@example.com</a>,
          Phone: <a property="phone" href="tel:+1-617-555-7332">+1 617.555.7332</a>
        </p>
      </div>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix foaf: <//xmlns.com/foaf/0.1/> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    <> rdfa:usesVocabulary foaf: .
    _:c14n0 a foaf:Person ;
    	foaf:mbox <mailto:alice@example.com> ;
    	foaf:name "Alice Birpemswick" ;
    	foaf:phone <tel:+1-617-555-7332> .
    "#);
}

#[test]
fn example_19_friends_homepage_and_name() {
    let html = r#"<html>
    <head>...</head>
    <body>
      <div vocab="http://xmlns.com/foaf/0.1/">
        <ul>
          <li typeof="Person">
            <a property="homepage" href="http://example.com/bob/"><span property="name">Bob</span></a>
          </li>
          <li typeof="Person">
            <a property="homepage" href="http://example.com/eve/"><span property="name">Eve</span></a>
          </li>
          <li typeof="Person">
            <a property="homepage" href="http://example.com/manu/"><span property="name">Manu</span></a>
          </li>
        </ul>
      </div>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix foaf: <//xmlns.com/foaf/0.1/> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    <> rdfa:usesVocabulary foaf: .
    _:c14n0 a foaf:Person ;
    	foaf:homepage </manu/> ;
    	foaf:name "Manu" .
    _:c14n1 a foaf:Person ;
    	foaf:homepage </bob/> ;
    	foaf:name "Bob" .
    _:c14n2 a foaf:Person ;
    	foaf:homepage </eve/> ;
    	foaf:name "Eve" .
    "#);
}

#[test]
fn example_20_social_network() {
    let html = r#"<html>
    <head>...</head>
    <body>
      <div vocab="http://xmlns.com/foaf/0.1/" typeof="Person">
        <p>
          <span property="name">Alice Birpemswick</span>,
          Email: <a property="mbox" href="mailto:alice@example.com">alice@example.com</a>,
          Phone: <a property="phone" href="tel:+1-617-555-7332">+1 617.555.7332</a>
        </p>
        <ul>
          <li property="knows" typeof="Person">
            <a property="homepage" href="http://example.com/bob/"><span property="name">Bob</span></a>
          </li>
          <li property="knows" typeof="Person">
            <a property="homepage" href="http://example.com/eve/"><span property="name">Eve</span></a>
          </li>
          <li property="knows" typeof="Person">
            <a property="homepage" href="http://example.com/manu/"><span property="name">Manu</span></a>
          </li>
        </ul>
      </div>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix foaf: <//xmlns.com/foaf/0.1/> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    <> rdfa:usesVocabulary foaf: .
    _:c14n0 a foaf:Person ;
    	foaf:homepage </manu/> ;
    	foaf:name "Manu" .
    _:c14n1 a foaf:Person ;
    	foaf:knows _:c14n0 , _:c14n2 , _:c14n3 ;
    	foaf:mbox <mailto:alice@example.com> ;
    	foaf:name "Alice Birpemswick" ;
    	foaf:phone <tel:+1-617-555-7332> .
    _:c14n2 a foaf:Person ;
    	foaf:homepage </bob/> ;
    	foaf:name "Bob" .
    _:c14n3 a foaf:Person ;
    	foaf:homepage </eve/> ;
    	foaf:name "Eve" .
    "#);
}

#[test]
fn example_22_property_copying() {
    let html = r##"<html>
    <head>...</head>
    <body vocab="http://purl.org/dc/terms/">
       <div resource="/alice/posts/trouble_with_bob">
          <h2 property="title">The trouble with Bob</h2>
          <p>Date: <span property="created">2011-09-10</span></p>
          <h3 property="creator">Alice</h3>
          <link property="rdfa:copy" href="#ccpattern"/>
       </div>
       <div resource="/alice/posts/jims_concert">
          <h2 property="title">I was at Jim's concert the other day</h2>
          <p>Date: <span property="created">2011-10-22</span></p>
          <h3 property="creator">Alice</h3>
          <link property="rdfa:copy" href="#ccpattern"/>
       </div>
       <div resource="#ccpattern" typeof="rdfa:Pattern">
          <p vocab="http://creativecommons.org/ns#">All content on this blog item is licensed under
            <a property="license" href="http://creativecommons.org/licenses/by/3.0/">
              a Creative Commons License</a>.
            <span property="attributionName">©2011 Alice Birpemswick</span>.</p>
       </div>
    </body>
    </html>"##;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix cc: <//creativecommons.org/ns#> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <jims_concert> cc:attributionName "©2011 Alice Birpemswick" ;
    	cc:license <//creativecommons.org/licenses/by/3.0/> ;
    	dc:created "2011-10-22" ;
    	dc:creator "Alice" ;
    	dc:title "I was at Jim's concert the other day" .
    <> cc:attributionName "©2011 Alice Birpemswick" ;
    	cc:license <//creativecommons.org/licenses/by/3.0/> ;
    	dc:created "2011-09-10" ;
    	dc:creator "Alice" ;
    	dc:title "The trouble with Bob" ;
    	rdfa:usesVocabulary cc: , dc: .
    "#);
}

#[test]
fn example_25_internal_references() {
    let html = r##"<html>
    <head>...</head>
    <body>
      <div vocab="http://purl.org/dc/terms/">
        <div resource="/alice/posts/trouble_with_bob">
          <h2 property="title">The trouble with Bob</h2>
          <h3 property="creator" resource="#me">Alice</h3>
        </div>
      </div>
      <div class="sidebar" vocab="http://xmlns.com/foaf/0.1/" resource="#me" typeof="Person">
        <p>
          <span property="name">Alice Birpemswick</span>,
          Email: <a property="mbox" href="mailto:alice@example.com">alice@example.com</a>,
          Phone: <a property="phone" href="tel:+1-617-555-7332">+1 617.555.7332</a>
        </p>
      </div>
    </body>
    </html>"##;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix foaf: <//xmlns.com/foaf/0.1/> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <#me> a foaf:Person ;
    	foaf:mbox <mailto:alice@example.com> ;
    	foaf:name "Alice Birpemswick" ;
    	foaf:phone <tel:+1-617-555-7332> .
    <> dc:creator <#me> ;
    	dc:title "The trouble with Bob" ;
    	rdfa:usesVocabulary dc: , foaf: .
    "#);
}

#[test]
fn example_28_prefix_attribute() {
    let html = r##"<html>
    <head>...</head>
    <body prefix="dc: http://purl.org/dc/terms/ schema: http://schema.org/">
       <div resource="/alice/posts/trouble_with_bob" typeof="schema:BlogPosting">
          <h2 property="dc:title">The trouble with Bob</h2>
          <h3 property="dc:creator" resource="#me">Alice</h3>
          <div property="schema:articleBody">
            <p>The trouble with Bob is that he takes much better photos than I do:</p>
          </div>
       </div>
    </body>
    </html>"##;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix dc: <//purl.org/dc/terms/> .
    @prefix schema: <//schema.org/> .
    <> a schema:BlogPosting ;
    	dc:creator <#me> ;
    	dc:title "The trouble with Bob" ;
    	schema:articleBody "\n            The trouble with Bob is that he takes much better photos than I do:\n          " .
    "#);
}

#[test]
fn example_29_vocab_and_prefix_mixed() {
    let html = r##"<html>
    <head>...</head>
    <body vocab="http://purl.org/dc/terms/" prefix="schema: http://schema.org/">
       <div resource="/alice/posts/trouble_with_bob" typeof="schema:BlogPosting">
          <h2 property="title">The trouble with Bob</h2>
          ...
          <h3 property="creator" resource="#me">Alice</h3>
          <div property="schema:articleBody">
            <p>The trouble with Bob is that he takes much better photos than I do:</p>
          </div>
          ...
       </div>
    </body>
    </html>"##;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    @prefix schema: <//schema.org/> .
    <> a schema:BlogPosting ;
    	dc:creator <#me> ;
    	dc:title "The trouble with Bob" ;
    	schema:articleBody "\n            The trouble with Bob is that he takes much better photos than I do:\n          " ;
    	rdfa:usesVocabulary dc: .
    "#);
}

#[test]
fn example_31_multiple_property_values() {
    let html = r##"<html>
    <head>...</head>
    <body prefix="dc: http://purl.org/dc/terms/ schema: http://schema.org/">
       <div resource="/alice/posts/trouble_with_bob" typeof="schema:BlogPosting">
          <h2 property="dc:title">The trouble with Bob</h2>
          ..
          <h3 property="dc:creator schema:creator" resource="#me">Alice</h3>
          <div property="schema:articleBody">
            <p>The trouble with Bob is that he takes much better photos than I do:</p>
          </div>
          ...
       </div>
    </body>
    </html>"##;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix dc: <//purl.org/dc/terms/> .
    @prefix schema: <//schema.org/> .
    <> a schema:BlogPosting ;
    	dc:creator <#me> ;
    	dc:title "The trouble with Bob" ;
    	schema:articleBody "\n            The trouble with Bob is that he takes much better photos than I do:\n          " ;
    	schema:creator <#me> .
    "#);
}

#[test]
fn example_32_multiple_typeof_values() {
    let html = r##"<html>
    <head>...</head>
    <body>
      <div class="sidebar"
           prefix="foaf: http://xmlns.com/foaf/0.1/ schema: http://schema.org/"
           resource="#me" typeof="foaf:Person schema:Person">
        <p>
          <span property="foaf:name">Alice Birpemswick</span>,
          Email: <a property="foaf:mbox" href="mailto:alice@example.com">alice@example.com</a>,
          Phone: <a property="foaf:phone" href="tel:+1-617-555-7332">+1 617.555.7332</a>
        </p>
        ...
      </div>
    </body>
    </html>"##;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix foaf: <//xmlns.com/foaf/0.1/> .
    @prefix schema: <//schema.org/> .
    <#me> a schema:Person , foaf:Person ;
    	foaf:mbox <mailto:alice@example.com> ;
    	foaf:name "Alice Birpemswick" ;
    	foaf:phone <tel:+1-617-555-7332> .
    "#);
}

#[test]
fn example_33_default_prefixes() {
    let html = r##"<html>
    <head>...</head>
    <body>
       <div>
          <h2 property="dc:title">The trouble with Bob</h2>
          <h3 property="dc:creator" resource="#me">Alice</h3>
       </div>
    </body>
    </html>"##;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix dc: <//purl.org/dc/terms/> .
    <> dc:creator <#me> ;
    	dc:title "The trouble with Bob" .
    "#);
}

#[test]
fn example_35_content_attribute() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head>...</head>
    <body>
       <h2 property="http://purl.org/dc/terms/title">The Trouble with Bob</h2>
       <p>Date: <span property="http://purl.org/dc/terms/created"
                       content="2011-09-10">10th of September, 2011</span></p>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix dc: <//purl.org/dc/terms/> .
    <> dc:created "2011-09-10" ;
    	dc:title "The Trouble with Bob" .
    "#);
}

#[test]
fn example_36_meta_in_head_ogp() {
    let html = r#"<!DOCTYPE html>
    <html>
    <head prefix="og: http://ogp.me/ns#">
       <meta property="og:title" content="The Trouble with Bob" />
       <meta property="og:type"  content="text" />
       <meta property="og:image" content="http://example.com/alice/bob-ugly.jpg" />
    </head>
    <body>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix og: <//ogp.me/ns#> .
    <> og:image "http://example.com/alice/bob-ugly.jpg" ;
    	og:title "The Trouble with Bob" ;
    	og:type "text" .
    "#);
}

#[test]
fn example_39_datatype() {
    let html = r#"<html>
    <head>...</head>
    <body prefix="dc: http://purl.org/dc/terms/">
      <p>All content on this site is licensed under
         <a property="http://creativecommons.org/ns#license"
            href="http://creativecommons.org/licenses/by/3.0/">
           a Creative Commons License</a>.
        ©<span property="dc:date" datatype="xsd:gYear">2011</span> Alice Birpemswick.</p>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix xsd: <//www.w3.org/2001/XMLSchema#> .
    @prefix cc: <//creativecommons.org/ns#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <> cc:license <//creativecommons.org/licenses/by/3.0/> ;
    	dc:date "2011"^^xsd:gYear .
    "#);
}

// Example 43: about attribute — alternative for setting context
#[test]
fn example_43_about_attribute() {
    let html = r#"<html>
    <head>...</head>
    <body vocab="http://purl.org/dc/terms/">
      <ul>
        <li about="/alice/posts/trouble_with_bob" property="title">The trouble with Bob</li>
        <li about="/alice/posts/jos_barbecue" property="title">Jo's Barbecue</li>
      </ul>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <jos_barbecue> dc:title "Jo's Barbecue" .
    <> dc:title "The trouble with Bob" ;
    	rdfa:usesVocabulary dc: .
    "#);
}

#[test]
fn example_46_rel_chaining() {
    let html = r##"<html>
    <head>...</head>
    <body>
      <div vocab="http://xmlns.com/foaf/0.1/" resource="#me">
        <ul rel="knows">
          <li resource="http://example.com/bob/#me" typeof="Person">
            <a property="homepage" href="http://example.com/bob/"><span property="name">Bob</span></a>
          </li>
          <li resource="http://example.com/eve/#me" typeof="Person">
            <a property="homepage" href="http://example.com/eve/"><span property="name">Eve</span></a>
          </li>
          <li resource="http://example.com/manu/#me" typeof="Person">
            <a property="homepage" href="http://example.com/manu/"><span property="name">Manu</span></a>
          </li>
        </ul>
      </div>
    </body>
    </html>"##;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix foaf: <//xmlns.com/foaf/0.1/> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    <#me> foaf:knows </bob/#me> , </eve/#me> , </manu/#me> .
    <> rdfa:usesVocabulary foaf: .
    </bob/#me> a foaf:Person ;
    	foaf:homepage </bob/> ;
    	foaf:name "Bob" .
    </eve/#me> a foaf:Person ;
    	foaf:homepage </eve/> ;
    	foaf:name "Eve" .
    </manu/#me> a foaf:Person ;
    	foaf:homepage </manu/> ;
    	foaf:name "Manu" .
    "#);
}

#[test]
fn example_26_multiple_blogs_same_creator() {
    let html = r##"<html>
    <head>...</head>
    <body>
      <div vocab="http://purl.org/dc/terms/">
        <div resource="/alice/posts/trouble_with_bob">
          <h2 property="title">The trouble with Bob</h2>
          <h3 property="creator" resource="#me">Alice</h3>
        </div>
      </div>
      <div vocab="http://purl.org/dc/terms/">
        <div resource="/alice/posts/my_photos">
          <h2 property="title">I will post my photos nevertheless…</h2>
          <h3 property="creator" resource="#me">Alice</h3>
        </div>
      </div>
      <div class="sidebar" vocab="http://xmlns.com/foaf/0.1/" resource="#me" typeof="Person">
        <p>
          <span property="name">Alice Birpemswick</span>,
          Email: <a property="mbox" href="mailto:alice@example.com">alice@example.com</a>,
          Phone: <a property="phone" href="tel:+1-617-555-7332">+1 617.555.7332</a>
        </p>
      </div>
    </body>
    </html>"##;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix foaf: <//xmlns.com/foaf/0.1/> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <my_photos> dc:creator <#me> ;
    	dc:title "I will post my photos nevertheless…" .
    <#me> a foaf:Person ;
    	foaf:mbox <mailto:alice@example.com> ;
    	foaf:name "Alice Birpemswick" ;
    	foaf:phone <tel:+1-617-555-7332> .
    <> dc:creator <#me> ;
    	dc:title "The trouble with Bob" ;
    	rdfa:usesVocabulary dc: , foaf: .
    "#);
}

#[test]
fn example_23_inline_foaf_mixed_vocab() {
    let html = r#"<html>
    <head>...</head>
    <body>
      <div vocab="http://purl.org/dc/terms/">
        <div resource="/alice/posts/trouble_with_bob">
          <h2 property="title">The trouble with Bob</h2>
          <h3 vocab="http://xmlns.com/foaf/0.1/"
              property="http://purl.org/dc/terms/creator" typeof="Person">
            <span property="name">Alice Birpemswick</span>,
            Email: <a property="mbox" href="mailto:alice@example.com">alice@example.com</a>,
            Phone: <a property="phone" href="tel:+1-617-555-7332">+1 617.555.7332</a>
          </h3>
        </div>
      </div>
    </body>
    </html>"#;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix foaf: <//xmlns.com/foaf/0.1/> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    <> dc:creator _:c14n0 ;
    	dc:title "The trouble with Bob" ;
    	rdfa:usesVocabulary dc: , foaf: .
    _:c14n0 a foaf:Person ;
    	foaf:mbox <mailto:alice@example.com> ;
    	foaf:name "Alice Birpemswick" ;
    	foaf:phone <tel:+1-617-555-7332> .
    "#);
}

#[test]
fn example_27_vocab_with_full_uri_fallback() {
    let html = r##"<html>
    <head>...</head>
    <body vocab="http://schema.org/">
       <div resource="/alice/posts/trouble_with_bob" typeof="BlogPosting">
          <h2 property="http://purl.org/dc/terms/title">The trouble with Bob</h2>
          <h3 property="http://purl.org/dc/terms/creator" resource="#me">Alice</h3>
          <div property="articleBody">
            <p>The trouble with Bob is that he takes much better photos than I do:</p>
          </div>
       </div>
    </body>
    </html>"##;

    let (output, _) = process_html(html);
    insta::assert_snapshot!(utils::serialize_graph(output, BASE), @r#"
    @base <http://example.com/alice/posts/trouble_with_bob> .
    @prefix rdf: <//www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix rdfa: <//www.w3.org/ns/rdfa#> .
    @prefix dc: <//purl.org/dc/terms/> .
    @prefix schema: <//schema.org/> .
    <> a schema:BlogPosting ;
    	dc:creator <#me> ;
    	dc:title "The trouble with Bob" ;
    	schema:articleBody "\n            The trouble with Bob is that he takes much better photos than I do:\n          " ;
    	rdfa:usesVocabulary schema: .
    "#);
}
