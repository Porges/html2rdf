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

Currently, html2rdf supports (most of) RDFa 1.1. It may support some of RDFa 1.0 but this has not been properly tested.

Missing features are:
- XML output is not canonicalized at all
- (optional) RDF entailment is not performed

Eventually, html2rdf will also implement Microdata and JSON-LD, but this has not been started at all.
