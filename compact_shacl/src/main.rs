use std::collections::HashMap;

use miette::IntoDiagnostic;
use oxrdf::{BlankNode, BlankNodeRef, NamedNode, NamedNodeRef, TripleRef};
use pest::{Parser, error::Error, iterators::Pair};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct CompactSHACLParser;

fn main() -> miette::Result<()> {
    let input = r#"BASE <http://example.com/ns>

IMPORTS <http://example.com/person-ontology>

PREFIX ex: <http://example.com/ns#>

shape ex:PersonShape -> ex:Person {
	closed=true ignoredProperties=[rdf:type] .

	ex:ssn       xsd:string [0..1] pattern="^\\d{3}-\\d{2}-\\d{4}$" .
	ex:worksFor  IRI ex:Company [0..*] .
	ex:address   BlankNode [0..1] {
		ex:city xsd:string [1..1] .
		ex:postalCode xsd:integer|xsd:string [1..1] maxLength=5 .
	} .
}
"#;
    let file = CompactSHACLParser::parse(Rule::shaclDoc, input).map_err(Error::into_miette)?;
    let mut ttl = oxttl::TurtleSerializer::new()
        .with_prefix("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        .into_diagnostic()?
        .with_prefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
        .into_diagnostic()?
        .with_prefix("sh", "http://www.w3.org/ns/shacl#")
        .into_diagnostic()?
        .with_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
        .into_diagnostic()?;

    let mut shapes = Vec::new();
    let mut prefixes = HashMap::new();
    for pair in file {
        ttl = complete_ttl(ttl, pair, &mut prefixes, &mut shapes)?;
    }

    let locked_out = std::io::stdout().lock();
    let mut output = ttl.for_writer(locked_out);

    for pair in shapes {
        emit(&mut output, &prefixes, pair)?;
    }

    let _ = output.finish().into_diagnostic()?;

    Ok(())
}

fn emit_nodeShapeBody(pair: Pair<'_, Rule>, context_shape: &str) {
    debug_assert_eq!(pair.as_rule(), Rule::nodeShapeBody);

    let mut inner = pair.into_inner();
    let indent = "  ";
    for pair in inner {
        match pair.as_rule() {
            Rule::nodeOr => {}
            Rule::propertyShape => {}
            _ => unreachable!(),
        }
    }
}

fn complete_ttl<'a>(
    ttl: oxttl::TurtleSerializer,
    pair: Pair<'a, Rule>,
    prefixes: &mut HashMap<&'a str, &'a str>,
    shapes: &mut Vec<Pair<'a, Rule>>,
) -> miette::Result<oxttl::TurtleSerializer> {
    match pair.as_rule() {
        Rule::baseDecl => {
            let mut inner = pair.into_inner();
            let base = inner.next().unwrap().as_str();
            debug_assert!(inner.next().is_none());
            ttl.with_base_iri(base).into_diagnostic()
        }
        Rule::prefixDecl => {
            let mut inner = pair.into_inner();
            let prefix = inner.next().unwrap().as_str();
            let iri = inner.next().unwrap().as_str();
            debug_assert!(inner.next().is_none());
            prefixes.insert(prefix, iri);
            ttl.with_prefix(prefix, iri).into_diagnostic()
        }
        _r => {
            shapes.push(pair);
            Ok(ttl)
        }
    }
}

fn pair_as_node(prefixes: &HashMap<&str, &str>, pair: Pair<'_, Rule>) -> miette::Result<NamedNode> {
    match pair.as_rule() {
        Rule::iri => NamedNode::new(pair.as_str()).into_diagnostic(),
        Rule::prefixedName => {
            let (prefix, local) = pair.as_str().split_once(':').unwrap();
            NamedNode::new(format!(
                "{}{}",
                prefixes.get(prefix).expect("TODO: prefix not found"),
                local
            ))
            .into_diagnostic()
        }
        r => unreachable!("rule unexpected: {r:?}"),
    }
}

pub fn emit<W: std::io::Write>(
    ttl: &mut oxttl::turtle::WriterTurtleSerializer<W>,
    prefixes: &HashMap<&str, &str>,
    pair: Pair<'_, Rule>,
) -> miette::Result<()> {
    match pair.as_rule() {
        Rule::nodeShape => {
            let mut inner = pair.into_inner();
            let shape_target = inner.next().unwrap();
            let shape_target_node = pair_as_node(prefixes, shape_target)?;
            let node_shape_iri = NamedNodeRef::new("http://www.w3.org/ns/shacl#NodeShape").unwrap();
            let target_class_iri =
                NamedNodeRef::new("http://www.w3.org/ns/shacl#targetClass").unwrap();
            let a_iri =
                NamedNodeRef::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type").unwrap();
            ttl.serialize_triple(TripleRef::new(&shape_target_node, a_iri, node_shape_iri))
                .into_diagnostic()?;
            for pair in inner {
                match pair.as_rule() {
                    Rule::targetClass => {
                        let target_class = pair.into_inner().next().unwrap();
                        let target_class_node = pair_as_node(prefixes, target_class)?;
                        ttl.serialize_triple(TripleRef::new(
                            &shape_target_node,
                            target_class_iri,
                            &target_class_node,
                        ))
                        .into_diagnostic()?;
                    }
                    Rule::nodeShapeBody => {
                        //emit_nodeShapeBody(pair, shape_target);
                    }
                    _ => unreachable!(),
                }
            }
            println!(".");
        }
        Rule::shapeClass => {
            let mut inner = pair.into_inner();
            let shape = inner.next().unwrap().as_str();
            println!("{shape}");
            let indent = "  ";
            println!("{indent}a sh:NodeShape, rdfs:Class ;");
            for pair in inner {
                match pair.as_rule() {
                    Rule::nodeShapeBody => {
                        emit_nodeShapeBody(pair, shape);
                    }
                    _ => unreachable!(),
                }
            }
            println!(".");
        }
        _ => {} //unreachable!("Unhandled rule: {:?}", pair),
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;

    #[test]
    pub fn test_parser() {
        let input = indoc! { r#"
            BASE <http://example.com/ns>
            IMPORTS <http://example.com/person-ontology>
            PREFIX ex: <http://example.com/ns#>

            shape ex:PersonShape -> ex:Person {
                closed=true ignoredProperties=[rdf:type] .

                ex:ssn       xsd:string [0..1] pattern="^\\d{3}-\\d{2}-\\d{4}$" .
                ex:worksFor  IRI ex:Company [0..*] .
                ex:address   BlankNode [0..1] {
                    ex:city xsd:string [1..1] .
                    ex:postalCode xsd:integer|xsd:string [1..1] maxLength=5 .
                } .
            }
            "# };

        let parsed = CompactSHACLParser::parse(Rule::shaclDoc, input).unwrap();
        insta::assert_snapshot!(parsed.to_json());
    }
}
