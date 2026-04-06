# html2rdf

A Rust library & CLI for reading RDFa data from HTML.

### Installation

```console
$ cargo install html2rdf-cli
```

### CLI usage

```console
$ html2rdf http://games.porg.es/games/tic-tac-toe
```

## Goals & Support

Currently, html2rdf supports (nearly all of?) RDFa 1.1. RDFa 1.0 is not supported.

Known missing features are:
- XMLLiteral output in HTML5 host language is not canonicalized at all
- `xml:base` attributes are not processed at all

Eventually, html2rdf will also implement Microdata (and Microdata-to-RDF conversion)
and JSON-LD, but this has not been started at all.
