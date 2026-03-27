use indoc::indoc;
mod utils;

// RDFa defers to html5 for lang tags
// https://html.spec.whatwg.org/#the-lang-and-xml:lang-attributes
// according to which, @xml:lang must take precedence over @lang
// we _should_ be nice and produce a warning if they don't match

const XML_LANG: &str = indoc! {r#"
    <?xml version="1.0"?>
    <html xmlns="http://www.w3.org/1999/xhtml">
        <head>
            <title>Test</title>
        </head>
        <body xml:lang="mi">
            <p property="dc:description">hui</p>
        </body>
    </html>
    "#};

#[test]
pub fn html_xml_lang() {
    let (g, _) = utils::parse_html(XML_LANG);
    utils::assert_graph_eq_ttl(
        utils::base(),
        g,
        indoc! {r#"
            @base <http://example.test/> .
            @prefix dc: <//purl.org/dc/terms/> .
            <> dc:description "hui"@mi .
        "#},
    );
}

#[test]
pub fn xhtml_xml_lang() {
    let (g, _) = utils::parse_xhtml(XML_LANG);
    utils::assert_graph_eq_ttl(
        utils::base(),
        g,
        indoc! {r#"
            @base <http://example.test/> .
            @prefix dc: <//purl.org/dc/terms/> .
            <> dc:description "hui"@mi .
        "#},
    );
}

const HTML_LANG: &str = indoc! {r#"
    <?xml version="1.0"?>
    <html xmlns="http://www.w3.org/1999/xhtml">
        <head>
            <title>Test</title>
        </head>
        <body lang="cmn">
            <p property="dc:description">会</p>
        </body>
    </html>
    "#};

#[test]
pub fn html_html_lang() {
    let (g, _) = utils::parse_html(HTML_LANG);
    utils::assert_graph_eq_ttl(
        utils::base(),
        g,
        indoc! {r#"
            @base <http://example.test/> .
            @prefix dc: <//purl.org/dc/terms/> .
            <> dc:description "会"@cmn .
        "#},
    );
}

#[test]
pub fn xhtml_html_lang() {
    let (g, _) = utils::parse_xhtml(HTML_LANG);
    utils::assert_graph_eq_ttl(
        utils::base(),
        g,
        indoc! {r#"
            @base <http://example.test/> .
            @prefix dc: <//purl.org/dc/terms/> .
            <> dc:description "会"@cmn .
        "#},
    );
}

const HTML_AND_XML_LANG: &str = indoc! {r#"
    <?xml version="1.0"?>
    <html xmlns="http://www.w3.org/1999/xhtml">
        <head>
            <title>Test</title>
        </head>
        <body lang="mi" xml:lang="cmn">
            <p property="dc:description">会</p>
        </body>
    </html>
    "#};

#[test]
pub fn html_html_and_xml_lang() {
    let (g, _) = utils::parse_html(HTML_AND_XML_LANG);
    utils::assert_graph_eq_ttl(
        utils::base(),
        g,
        indoc! {r#"
            @base <http://example.test/> .
            @prefix dc: <//purl.org/dc/terms/> .
            <> dc:description "会"@cmn .
        "#},
    );
}

#[test]
pub fn xhtml_html_and_xml_lang() {
    let (g, _) = utils::parse_xhtml(HTML_AND_XML_LANG);
    utils::assert_graph_eq_ttl(
        utils::base(),
        g,
        indoc! {r#"
            @base <http://example.test/> .
            @prefix dc: <//purl.org/dc/terms/> .
            <> dc:description "会"@cmn .
        "#},
    );
}
