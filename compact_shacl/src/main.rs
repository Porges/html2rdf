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
    println!("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .");
    println!("@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .");
    println!("@prefix sh: <http://www.w3.org/ns/shacl#> .");
    println!("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .");

    for pair in file {
        emit(pair);
    }

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

pub fn emit(pair: Pair<'_, Rule>) {
    match pair.as_rule() {
        Rule::baseDecl => {
            let mut inner = pair.into_inner();
            let base = inner.next().unwrap().as_str();
            println!("@base <{base}> .");
        }
        Rule::prefixDecl => {
            let mut inner = pair.into_inner();
            let prefix = inner.next().unwrap().as_str();
            let iri = inner.next().unwrap().as_str();
            println!("@prefix {prefix}: <{iri}> .");
        }
        Rule::nodeShape => {
            let mut inner = pair.into_inner();
            let shape = inner.next().unwrap().as_str();
            println!("{shape}");
            let indent = "  ";
            println!("{indent}a sh:NodeShape ;");
            for pair in inner {
                match pair.as_rule() {
                    Rule::targetClass => {
                        println!(
                            "{indent}sh:targetClass {target_class} ;",
                            target_class = pair.into_inner().next().unwrap().as_str()
                        );
                    }
                    Rule::nodeShapeBody => {
                        emit_nodeShapeBody(pair, shape);
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
        insta::assert_snapshot!(parsed.to_json(), @r#"
        {
          "pos": [
            0,
            454
          ],
          "pairs": [
            {
              "pos": [
                0,
                28
              ],
              "rule": "baseDecl",
              "inner": {
                "pos": [
                  6,
                  27
                ],
                "pairs": [
                  {
                    "pos": [
                      6,
                      27
                    ],
                    "rule": "iri_value",
                    "inner": "http://example.com/ns"
                  }
                ]
              }
            },
            {
              "pos": [
                29,
                73
              ],
              "rule": "importsDecl",
              "inner": {
                "pos": [
                  38,
                  72
                ],
                "pairs": [
                  {
                    "pos": [
                      38,
                      72
                    ],
                    "rule": "iri_value",
                    "inner": "http://example.com/person-ontology"
                  }
                ]
              }
            },
            {
              "pos": [
                74,
                109
              ],
              "rule": "prefixDecl",
              "inner": {
                "pos": [
                  81,
                  108
                ],
                "pairs": [
                  {
                    "pos": [
                      81,
                      83
                    ],
                    "rule": "PN_PREFIX",
                    "inner": "ex"
                  },
                  {
                    "pos": [
                      86,
                      108
                    ],
                    "rule": "iri_value",
                    "inner": "http://example.com/ns#"
                  }
                ]
              }
            },
            {
              "pos": [
                111,
                453
              ],
              "rule": "nodeShape",
              "inner": {
                "pos": [
                  117,
                  453
                ],
                "pairs": [
                  {
                    "pos": [
                      117,
                      131
                    ],
                    "rule": "prefixedName",
                    "inner": "ex:PersonShape"
                  },
                  {
                    "pos": [
                      132,
                      145
                    ],
                    "rule": "targetClass",
                    "inner": {
                      "pos": [
                        135,
                        144
                      ],
                      "pairs": [
                        {
                          "pos": [
                            135,
                            144
                          ],
                          "rule": "prefixedName",
                          "inner": "ex:Person"
                        }
                      ]
                    }
                  },
                  {
                    "pos": [
                      145,
                      453
                    ],
                    "rule": "nodeShapeBody",
                    "inner": {
                      "pos": [
                        151,
                        450
                      ],
                      "pairs": [
                        {
                          "pos": [
                            151,
                            163
                          ],
                          "rule": "nodeOr",
                          "inner": {
                            "pos": [
                              151,
                              162
                            ],
                            "pairs": [
                              {
                                "pos": [
                                  151,
                                  162
                                ],
                                "rule": "nodeNot",
                                "inner": {
                                  "pos": [
                                    151,
                                    162
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        151,
                                        162
                                      ],
                                      "rule": "nodeValue",
                                      "inner": {
                                        "pos": [
                                          151,
                                          162
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              151,
                                              157
                                            ],
                                            "rule": "nodeParam",
                                            "inner": "closed"
                                          },
                                          {
                                            "pos": [
                                              158,
                                              162
                                            ],
                                            "rule": "iriOrLiteralOrArray",
                                            "inner": {
                                              "pos": [
                                                158,
                                                162
                                              ],
                                              "pairs": [
                                                {
                                                  "pos": [
                                                    158,
                                                    162
                                                  ],
                                                  "rule": "iriOrLiteral",
                                                  "inner": {
                                                    "pos": [
                                                      158,
                                                      162
                                                    ],
                                                    "pairs": [
                                                      {
                                                        "pos": [
                                                          158,
                                                          162
                                                        ],
                                                        "rule": "literal",
                                                        "inner": {
                                                          "pos": [
                                                            158,
                                                            162
                                                          ],
                                                          "pairs": [
                                                            {
                                                              "pos": [
                                                                158,
                                                                162
                                                              ],
                                                              "rule": "booleanLiteral",
                                                              "inner": "true"
                                                            }
                                                          ]
                                                        }
                                                      }
                                                    ]
                                                  }
                                                }
                                              ]
                                            }
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              }
                            ]
                          }
                        },
                        {
                          "pos": [
                            163,
                            192
                          ],
                          "rule": "nodeOr",
                          "inner": {
                            "pos": [
                              163,
                              191
                            ],
                            "pairs": [
                              {
                                "pos": [
                                  163,
                                  191
                                ],
                                "rule": "nodeNot",
                                "inner": {
                                  "pos": [
                                    163,
                                    191
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        163,
                                        191
                                      ],
                                      "rule": "nodeValue",
                                      "inner": {
                                        "pos": [
                                          163,
                                          191
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              163,
                                              180
                                            ],
                                            "rule": "nodeParam",
                                            "inner": "ignoredProperties"
                                          },
                                          {
                                            "pos": [
                                              181,
                                              191
                                            ],
                                            "rule": "iriOrLiteralOrArray",
                                            "inner": {
                                              "pos": [
                                                181,
                                                191
                                              ],
                                              "pairs": [
                                                {
                                                  "pos": [
                                                    181,
                                                    191
                                                  ],
                                                  "rule": "array",
                                                  "inner": {
                                                    "pos": [
                                                      182,
                                                      190
                                                    ],
                                                    "pairs": [
                                                      {
                                                        "pos": [
                                                          182,
                                                          190
                                                        ],
                                                        "rule": "iriOrLiteral",
                                                        "inner": {
                                                          "pos": [
                                                            182,
                                                            190
                                                          ],
                                                          "pairs": [
                                                            {
                                                              "pos": [
                                                                182,
                                                                190
                                                              ],
                                                              "rule": "prefixedName",
                                                              "inner": "rdf:type"
                                                            }
                                                          ]
                                                        }
                                                      }
                                                    ]
                                                  }
                                                }
                                              ]
                                            }
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              }
                            ]
                          }
                        },
                        {
                          "pos": [
                            199,
                            263
                          ],
                          "rule": "propertyShape",
                          "inner": {
                            "pos": [
                              199,
                              263
                            ],
                            "pairs": [
                              {
                                "pos": [
                                  199,
                                  212
                                ],
                                "rule": "path",
                                "inner": {
                                  "pos": [
                                    199,
                                    212
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        199,
                                        212
                                      ],
                                      "rule": "pathAlternative",
                                      "inner": {
                                        "pos": [
                                          199,
                                          212
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              199,
                                              212
                                            ],
                                            "rule": "pathSequence",
                                            "inner": {
                                              "pos": [
                                                199,
                                                212
                                              ],
                                              "pairs": [
                                                {
                                                  "pos": [
                                                    199,
                                                    212
                                                  ],
                                                  "rule": "pathEltOrInverse",
                                                  "inner": {
                                                    "pos": [
                                                      199,
                                                      212
                                                    ],
                                                    "pairs": [
                                                      {
                                                        "pos": [
                                                          199,
                                                          212
                                                        ],
                                                        "rule": "pathElt",
                                                        "inner": {
                                                          "pos": [
                                                            199,
                                                            205
                                                          ],
                                                          "pairs": [
                                                            {
                                                              "pos": [
                                                                199,
                                                                205
                                                              ],
                                                              "rule": "pathPrimary",
                                                              "inner": {
                                                                "pos": [
                                                                  199,
                                                                  205
                                                                ],
                                                                "pairs": [
                                                                  {
                                                                    "pos": [
                                                                      199,
                                                                      205
                                                                    ],
                                                                    "rule": "prefixedName",
                                                                    "inner": "ex:ssn"
                                                                  }
                                                                ]
                                                              }
                                                            }
                                                          ]
                                                        }
                                                      }
                                                    ]
                                                  }
                                                }
                                              ]
                                            }
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              },
                              {
                                "pos": [
                                  212,
                                  223
                                ],
                                "rule": "propertyOr",
                                "inner": {
                                  "pos": [
                                    212,
                                    222
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        212,
                                        222
                                      ],
                                      "rule": "propertyNot",
                                      "inner": {
                                        "pos": [
                                          212,
                                          222
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              212,
                                              222
                                            ],
                                            "rule": "propertyAtom",
                                            "inner": {
                                              "pos": [
                                                212,
                                                222
                                              ],
                                              "pairs": [
                                                {
                                                  "pos": [
                                                    212,
                                                    222
                                                  ],
                                                  "rule": "prefixedName",
                                                  "inner": "xsd:string"
                                                }
                                              ]
                                            }
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              },
                              {
                                "pos": [
                                  223,
                                  229
                                ],
                                "rule": "propertyCount",
                                "inner": {
                                  "pos": [
                                    224,
                                    228
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        224,
                                        225
                                      ],
                                      "rule": "propertyMinCount",
                                      "inner": {
                                        "pos": [
                                          224,
                                          225
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              224,
                                              225
                                            ],
                                            "rule": "INTEGER",
                                            "inner": "0"
                                          }
                                        ]
                                      }
                                    },
                                    {
                                      "pos": [
                                        227,
                                        228
                                      ],
                                      "rule": "propertyMaxCount",
                                      "inner": {
                                        "pos": [
                                          227,
                                          228
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              227,
                                              228
                                            ],
                                            "rule": "INTEGER",
                                            "inner": "1"
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              },
                              {
                                "pos": [
                                  230,
                                  263
                                ],
                                "rule": "propertyOr",
                                "inner": {
                                  "pos": [
                                    230,
                                    262
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        230,
                                        262
                                      ],
                                      "rule": "propertyNot",
                                      "inner": {
                                        "pos": [
                                          230,
                                          262
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              230,
                                              262
                                            ],
                                            "rule": "propertyAtom",
                                            "inner": {
                                              "pos": [
                                                230,
                                                262
                                              ],
                                              "pairs": [
                                                {
                                                  "pos": [
                                                    230,
                                                    262
                                                  ],
                                                  "rule": "propertyValue",
                                                  "inner": {
                                                    "pos": [
                                                      230,
                                                      262
                                                    ],
                                                    "pairs": [
                                                      {
                                                        "pos": [
                                                          230,
                                                          237
                                                        ],
                                                        "rule": "propertyParam",
                                                        "inner": "pattern"
                                                      },
                                                      {
                                                        "pos": [
                                                          238,
                                                          262
                                                        ],
                                                        "rule": "iriOrLiteralOrArray",
                                                        "inner": {
                                                          "pos": [
                                                            238,
                                                            262
                                                          ],
                                                          "pairs": [
                                                            {
                                                              "pos": [
                                                                238,
                                                                262
                                                              ],
                                                              "rule": "iriOrLiteral",
                                                              "inner": {
                                                                "pos": [
                                                                  238,
                                                                  262
                                                                ],
                                                                "pairs": [
                                                                  {
                                                                    "pos": [
                                                                      238,
                                                                      262
                                                                    ],
                                                                    "rule": "literal",
                                                                    "inner": {
                                                                      "pos": [
                                                                        238,
                                                                        262
                                                                      ],
                                                                      "pairs": [
                                                                        {
                                                                          "pos": [
                                                                            238,
                                                                            262
                                                                          ],
                                                                          "rule": "rdfLiteral",
                                                                          "inner": {
                                                                            "pos": [
                                                                              238,
                                                                              262
                                                                            ],
                                                                            "pairs": [
                                                                              {
                                                                                "pos": [
                                                                                  238,
                                                                                  262
                                                                                ],
                                                                                "rule": "string",
                                                                                "inner": {
                                                                                  "pos": [
                                                                                    238,
                                                                                    262
                                                                                  ],
                                                                                  "pairs": [
                                                                                    {
                                                                                      "pos": [
                                                                                        238,
                                                                                        262
                                                                                      ],
                                                                                      "rule": "STRING_LITERAL",
                                                                                      "inner": {
                                                                                        "pos": [
                                                                                          239,
                                                                                          261
                                                                                        ],
                                                                                        "pairs": [
                                                                                          {
                                                                                            "pos": [
                                                                                              239,
                                                                                              261
                                                                                            ],
                                                                                            "rule": "string_inner",
                                                                                            "inner": {
                                                                                              "pos": [
                                                                                                240,
                                                                                                256
                                                                                              ],
                                                                                              "pairs": [
                                                                                                {
                                                                                                  "pos": [
                                                                                                    240,
                                                                                                    242
                                                                                                  ],
                                                                                                  "rule": "ECHAR",
                                                                                                  "inner": {
                                                                                                    "pos": [
                                                                                                      241,
                                                                                                      242
                                                                                                    ],
                                                                                                    "pairs": [
                                                                                                      {
                                                                                                        "pos": [
                                                                                                          241,
                                                                                                          242
                                                                                                        ],
                                                                                                        "rule": "einner",
                                                                                                        "inner": "\\"
                                                                                                      }
                                                                                                    ]
                                                                                                  }
                                                                                                },
                                                                                                {
                                                                                                  "pos": [
                                                                                                    247,
                                                                                                    249
                                                                                                  ],
                                                                                                  "rule": "ECHAR",
                                                                                                  "inner": {
                                                                                                    "pos": [
                                                                                                      248,
                                                                                                      249
                                                                                                    ],
                                                                                                    "pairs": [
                                                                                                      {
                                                                                                        "pos": [
                                                                                                          248,
                                                                                                          249
                                                                                                        ],
                                                                                                        "rule": "einner",
                                                                                                        "inner": "\\"
                                                                                                      }
                                                                                                    ]
                                                                                                  }
                                                                                                },
                                                                                                {
                                                                                                  "pos": [
                                                                                                    254,
                                                                                                    256
                                                                                                  ],
                                                                                                  "rule": "ECHAR",
                                                                                                  "inner": {
                                                                                                    "pos": [
                                                                                                      255,
                                                                                                      256
                                                                                                    ],
                                                                                                    "pairs": [
                                                                                                      {
                                                                                                        "pos": [
                                                                                                          255,
                                                                                                          256
                                                                                                        ],
                                                                                                        "rule": "einner",
                                                                                                        "inner": "\\"
                                                                                                      }
                                                                                                    ]
                                                                                                  }
                                                                                                }
                                                                                              ]
                                                                                            }
                                                                                          }
                                                                                        ]
                                                                                      }
                                                                                    }
                                                                                  ]
                                                                                }
                                                                              }
                                                                            ]
                                                                          }
                                                                        }
                                                                      ]
                                                                    }
                                                                  }
                                                                ]
                                                              }
                                                            }
                                                          ]
                                                        }
                                                      }
                                                    ]
                                                  }
                                                }
                                              ]
                                            }
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              }
                            ]
                          }
                        },
                        {
                          "pos": [
                            269,
                            303
                          ],
                          "rule": "propertyShape",
                          "inner": {
                            "pos": [
                              269,
                              303
                            ],
                            "pairs": [
                              {
                                "pos": [
                                  269,
                                  282
                                ],
                                "rule": "path",
                                "inner": {
                                  "pos": [
                                    269,
                                    282
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        269,
                                        282
                                      ],
                                      "rule": "pathAlternative",
                                      "inner": {
                                        "pos": [
                                          269,
                                          282
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              269,
                                              282
                                            ],
                                            "rule": "pathSequence",
                                            "inner": {
                                              "pos": [
                                                269,
                                                282
                                              ],
                                              "pairs": [
                                                {
                                                  "pos": [
                                                    269,
                                                    282
                                                  ],
                                                  "rule": "pathEltOrInverse",
                                                  "inner": {
                                                    "pos": [
                                                      269,
                                                      282
                                                    ],
                                                    "pairs": [
                                                      {
                                                        "pos": [
                                                          269,
                                                          282
                                                        ],
                                                        "rule": "pathElt",
                                                        "inner": {
                                                          "pos": [
                                                            269,
                                                            280
                                                          ],
                                                          "pairs": [
                                                            {
                                                              "pos": [
                                                                269,
                                                                280
                                                              ],
                                                              "rule": "pathPrimary",
                                                              "inner": {
                                                                "pos": [
                                                                  269,
                                                                  280
                                                                ],
                                                                "pairs": [
                                                                  {
                                                                    "pos": [
                                                                      269,
                                                                      280
                                                                    ],
                                                                    "rule": "prefixedName",
                                                                    "inner": "ex:worksFor"
                                                                  }
                                                                ]
                                                              }
                                                            }
                                                          ]
                                                        }
                                                      }
                                                    ]
                                                  }
                                                }
                                              ]
                                            }
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              },
                              {
                                "pos": [
                                  282,
                                  286
                                ],
                                "rule": "propertyOr",
                                "inner": {
                                  "pos": [
                                    282,
                                    285
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        282,
                                        285
                                      ],
                                      "rule": "propertyNot",
                                      "inner": {
                                        "pos": [
                                          282,
                                          285
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              282,
                                              285
                                            ],
                                            "rule": "propertyAtom",
                                            "inner": {
                                              "pos": [
                                                282,
                                                285
                                              ],
                                              "pairs": [
                                                {
                                                  "pos": [
                                                    282,
                                                    285
                                                  ],
                                                  "rule": "nodeKind",
                                                  "inner": "IRI"
                                                }
                                              ]
                                            }
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              },
                              {
                                "pos": [
                                  286,
                                  297
                                ],
                                "rule": "propertyOr",
                                "inner": {
                                  "pos": [
                                    286,
                                    296
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        286,
                                        296
                                      ],
                                      "rule": "propertyNot",
                                      "inner": {
                                        "pos": [
                                          286,
                                          296
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              286,
                                              296
                                            ],
                                            "rule": "propertyAtom",
                                            "inner": {
                                              "pos": [
                                                286,
                                                296
                                              ],
                                              "pairs": [
                                                {
                                                  "pos": [
                                                    286,
                                                    296
                                                  ],
                                                  "rule": "prefixedName",
                                                  "inner": "ex:Company"
                                                }
                                              ]
                                            }
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              },
                              {
                                "pos": [
                                  297,
                                  303
                                ],
                                "rule": "propertyCount",
                                "inner": {
                                  "pos": [
                                    298,
                                    302
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        298,
                                        299
                                      ],
                                      "rule": "propertyMinCount",
                                      "inner": {
                                        "pos": [
                                          298,
                                          299
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              298,
                                              299
                                            ],
                                            "rule": "INTEGER",
                                            "inner": "0"
                                          }
                                        ]
                                      }
                                    },
                                    {
                                      "pos": [
                                        301,
                                        302
                                      ],
                                      "rule": "propertyMaxCount",
                                      "inner": "*"
                                    }
                                  ]
                                }
                              }
                            ]
                          }
                        },
                        {
                          "pos": [
                            310,
                            450
                          ],
                          "rule": "propertyShape",
                          "inner": {
                            "pos": [
                              310,
                              450
                            ],
                            "pairs": [
                              {
                                "pos": [
                                  310,
                                  323
                                ],
                                "rule": "path",
                                "inner": {
                                  "pos": [
                                    310,
                                    323
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        310,
                                        323
                                      ],
                                      "rule": "pathAlternative",
                                      "inner": {
                                        "pos": [
                                          310,
                                          323
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              310,
                                              323
                                            ],
                                            "rule": "pathSequence",
                                            "inner": {
                                              "pos": [
                                                310,
                                                323
                                              ],
                                              "pairs": [
                                                {
                                                  "pos": [
                                                    310,
                                                    323
                                                  ],
                                                  "rule": "pathEltOrInverse",
                                                  "inner": {
                                                    "pos": [
                                                      310,
                                                      323
                                                    ],
                                                    "pairs": [
                                                      {
                                                        "pos": [
                                                          310,
                                                          323
                                                        ],
                                                        "rule": "pathElt",
                                                        "inner": {
                                                          "pos": [
                                                            310,
                                                            320
                                                          ],
                                                          "pairs": [
                                                            {
                                                              "pos": [
                                                                310,
                                                                320
                                                              ],
                                                              "rule": "pathPrimary",
                                                              "inner": {
                                                                "pos": [
                                                                  310,
                                                                  320
                                                                ],
                                                                "pairs": [
                                                                  {
                                                                    "pos": [
                                                                      310,
                                                                      320
                                                                    ],
                                                                    "rule": "prefixedName",
                                                                    "inner": "ex:address"
                                                                  }
                                                                ]
                                                              }
                                                            }
                                                          ]
                                                        }
                                                      }
                                                    ]
                                                  }
                                                }
                                              ]
                                            }
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              },
                              {
                                "pos": [
                                  323,
                                  333
                                ],
                                "rule": "propertyOr",
                                "inner": {
                                  "pos": [
                                    323,
                                    332
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        323,
                                        332
                                      ],
                                      "rule": "propertyNot",
                                      "inner": {
                                        "pos": [
                                          323,
                                          332
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              323,
                                              332
                                            ],
                                            "rule": "propertyAtom",
                                            "inner": {
                                              "pos": [
                                                323,
                                                332
                                              ],
                                              "pairs": [
                                                {
                                                  "pos": [
                                                    323,
                                                    332
                                                  ],
                                                  "rule": "nodeKind",
                                                  "inner": "BlankNode"
                                                }
                                              ]
                                            }
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              },
                              {
                                "pos": [
                                  333,
                                  339
                                ],
                                "rule": "propertyCount",
                                "inner": {
                                  "pos": [
                                    334,
                                    338
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        334,
                                        335
                                      ],
                                      "rule": "propertyMinCount",
                                      "inner": {
                                        "pos": [
                                          334,
                                          335
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              334,
                                              335
                                            ],
                                            "rule": "INTEGER",
                                            "inner": "0"
                                          }
                                        ]
                                      }
                                    },
                                    {
                                      "pos": [
                                        337,
                                        338
                                      ],
                                      "rule": "propertyMaxCount",
                                      "inner": {
                                        "pos": [
                                          337,
                                          338
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              337,
                                              338
                                            ],
                                            "rule": "INTEGER",
                                            "inner": "1"
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              },
                              {
                                "pos": [
                                  340,
                                  450
                                ],
                                "rule": "propertyOr",
                                "inner": {
                                  "pos": [
                                    340,
                                    449
                                  ],
                                  "pairs": [
                                    {
                                      "pos": [
                                        340,
                                        449
                                      ],
                                      "rule": "propertyNot",
                                      "inner": {
                                        "pos": [
                                          340,
                                          449
                                        ],
                                        "pairs": [
                                          {
                                            "pos": [
                                              340,
                                              449
                                            ],
                                            "rule": "propertyAtom",
                                            "inner": {
                                              "pos": [
                                                340,
                                                449
                                              ],
                                              "pairs": [
                                                {
                                                  "pos": [
                                                    340,
                                                    449
                                                  ],
                                                  "rule": "nodeShapeBody",
                                                  "inner": {
                                                    "pos": [
                                                      350,
                                                      442
                                                    ],
                                                    "pairs": [
                                                      {
                                                        "pos": [
                                                          350,
                                                          375
                                                        ],
                                                        "rule": "propertyShape",
                                                        "inner": {
                                                          "pos": [
                                                            350,
                                                            375
                                                          ],
                                                          "pairs": [
                                                            {
                                                              "pos": [
                                                                350,
                                                                358
                                                              ],
                                                              "rule": "path",
                                                              "inner": {
                                                                "pos": [
                                                                  350,
                                                                  358
                                                                ],
                                                                "pairs": [
                                                                  {
                                                                    "pos": [
                                                                      350,
                                                                      358
                                                                    ],
                                                                    "rule": "pathAlternative",
                                                                    "inner": {
                                                                      "pos": [
                                                                        350,
                                                                        358
                                                                      ],
                                                                      "pairs": [
                                                                        {
                                                                          "pos": [
                                                                            350,
                                                                            358
                                                                          ],
                                                                          "rule": "pathSequence",
                                                                          "inner": {
                                                                            "pos": [
                                                                              350,
                                                                              358
                                                                            ],
                                                                            "pairs": [
                                                                              {
                                                                                "pos": [
                                                                                  350,
                                                                                  358
                                                                                ],
                                                                                "rule": "pathEltOrInverse",
                                                                                "inner": {
                                                                                  "pos": [
                                                                                    350,
                                                                                    358
                                                                                  ],
                                                                                  "pairs": [
                                                                                    {
                                                                                      "pos": [
                                                                                        350,
                                                                                        358
                                                                                      ],
                                                                                      "rule": "pathElt",
                                                                                      "inner": {
                                                                                        "pos": [
                                                                                          350,
                                                                                          357
                                                                                        ],
                                                                                        "pairs": [
                                                                                          {
                                                                                            "pos": [
                                                                                              350,
                                                                                              357
                                                                                            ],
                                                                                            "rule": "pathPrimary",
                                                                                            "inner": {
                                                                                              "pos": [
                                                                                                350,
                                                                                                357
                                                                                              ],
                                                                                              "pairs": [
                                                                                                {
                                                                                                  "pos": [
                                                                                                    350,
                                                                                                    357
                                                                                                  ],
                                                                                                  "rule": "prefixedName",
                                                                                                  "inner": "ex:city"
                                                                                                }
                                                                                              ]
                                                                                            }
                                                                                          }
                                                                                        ]
                                                                                      }
                                                                                    }
                                                                                  ]
                                                                                }
                                                                              }
                                                                            ]
                                                                          }
                                                                        }
                                                                      ]
                                                                    }
                                                                  }
                                                                ]
                                                              }
                                                            },
                                                            {
                                                              "pos": [
                                                                358,
                                                                369
                                                              ],
                                                              "rule": "propertyOr",
                                                              "inner": {
                                                                "pos": [
                                                                  358,
                                                                  368
                                                                ],
                                                                "pairs": [
                                                                  {
                                                                    "pos": [
                                                                      358,
                                                                      368
                                                                    ],
                                                                    "rule": "propertyNot",
                                                                    "inner": {
                                                                      "pos": [
                                                                        358,
                                                                        368
                                                                      ],
                                                                      "pairs": [
                                                                        {
                                                                          "pos": [
                                                                            358,
                                                                            368
                                                                          ],
                                                                          "rule": "propertyAtom",
                                                                          "inner": {
                                                                            "pos": [
                                                                              358,
                                                                              368
                                                                            ],
                                                                            "pairs": [
                                                                              {
                                                                                "pos": [
                                                                                  358,
                                                                                  368
                                                                                ],
                                                                                "rule": "prefixedName",
                                                                                "inner": "xsd:string"
                                                                              }
                                                                            ]
                                                                          }
                                                                        }
                                                                      ]
                                                                    }
                                                                  }
                                                                ]
                                                              }
                                                            },
                                                            {
                                                              "pos": [
                                                                369,
                                                                375
                                                              ],
                                                              "rule": "propertyCount",
                                                              "inner": {
                                                                "pos": [
                                                                  370,
                                                                  374
                                                                ],
                                                                "pairs": [
                                                                  {
                                                                    "pos": [
                                                                      370,
                                                                      371
                                                                    ],
                                                                    "rule": "propertyMinCount",
                                                                    "inner": {
                                                                      "pos": [
                                                                        370,
                                                                        371
                                                                      ],
                                                                      "pairs": [
                                                                        {
                                                                          "pos": [
                                                                            370,
                                                                            371
                                                                          ],
                                                                          "rule": "INTEGER",
                                                                          "inner": "1"
                                                                        }
                                                                      ]
                                                                    }
                                                                  },
                                                                  {
                                                                    "pos": [
                                                                      373,
                                                                      374
                                                                    ],
                                                                    "rule": "propertyMaxCount",
                                                                    "inner": {
                                                                      "pos": [
                                                                        373,
                                                                        374
                                                                      ],
                                                                      "pairs": [
                                                                        {
                                                                          "pos": [
                                                                            373,
                                                                            374
                                                                          ],
                                                                          "rule": "INTEGER",
                                                                          "inner": "1"
                                                                        }
                                                                      ]
                                                                    }
                                                                  }
                                                                ]
                                                              }
                                                            }
                                                          ]
                                                        }
                                                      },
                                                      {
                                                        "pos": [
                                                          386,
                                                          442
                                                        ],
                                                        "rule": "propertyShape",
                                                        "inner": {
                                                          "pos": [
                                                            386,
                                                            442
                                                          ],
                                                          "pairs": [
                                                            {
                                                              "pos": [
                                                                386,
                                                                400
                                                              ],
                                                              "rule": "path",
                                                              "inner": {
                                                                "pos": [
                                                                  386,
                                                                  400
                                                                ],
                                                                "pairs": [
                                                                  {
                                                                    "pos": [
                                                                      386,
                                                                      400
                                                                    ],
                                                                    "rule": "pathAlternative",
                                                                    "inner": {
                                                                      "pos": [
                                                                        386,
                                                                        400
                                                                      ],
                                                                      "pairs": [
                                                                        {
                                                                          "pos": [
                                                                            386,
                                                                            400
                                                                          ],
                                                                          "rule": "pathSequence",
                                                                          "inner": {
                                                                            "pos": [
                                                                              386,
                                                                              400
                                                                            ],
                                                                            "pairs": [
                                                                              {
                                                                                "pos": [
                                                                                  386,
                                                                                  400
                                                                                ],
                                                                                "rule": "pathEltOrInverse",
                                                                                "inner": {
                                                                                  "pos": [
                                                                                    386,
                                                                                    400
                                                                                  ],
                                                                                  "pairs": [
                                                                                    {
                                                                                      "pos": [
                                                                                        386,
                                                                                        400
                                                                                      ],
                                                                                      "rule": "pathElt",
                                                                                      "inner": {
                                                                                        "pos": [
                                                                                          386,
                                                                                          399
                                                                                        ],
                                                                                        "pairs": [
                                                                                          {
                                                                                            "pos": [
                                                                                              386,
                                                                                              399
                                                                                            ],
                                                                                            "rule": "pathPrimary",
                                                                                            "inner": {
                                                                                              "pos": [
                                                                                                386,
                                                                                                399
                                                                                              ],
                                                                                              "pairs": [
                                                                                                {
                                                                                                  "pos": [
                                                                                                    386,
                                                                                                    399
                                                                                                  ],
                                                                                                  "rule": "prefixedName",
                                                                                                  "inner": "ex:postalCode"
                                                                                                }
                                                                                              ]
                                                                                            }
                                                                                          }
                                                                                        ]
                                                                                      }
                                                                                    }
                                                                                  ]
                                                                                }
                                                                              }
                                                                            ]
                                                                          }
                                                                        }
                                                                      ]
                                                                    }
                                                                  }
                                                                ]
                                                              }
                                                            },
                                                            {
                                                              "pos": [
                                                                400,
                                                                422
                                                              ],
                                                              "rule": "propertyOr",
                                                              "inner": {
                                                                "pos": [
                                                                  400,
                                                                  422
                                                                ],
                                                                "pairs": [
                                                                  {
                                                                    "pos": [
                                                                      400,
                                                                      411
                                                                    ],
                                                                    "rule": "propertyNot",
                                                                    "inner": {
                                                                      "pos": [
                                                                        400,
                                                                        411
                                                                      ],
                                                                      "pairs": [
                                                                        {
                                                                          "pos": [
                                                                            400,
                                                                            411
                                                                          ],
                                                                          "rule": "propertyAtom",
                                                                          "inner": {
                                                                            "pos": [
                                                                              400,
                                                                              411
                                                                            ],
                                                                            "pairs": [
                                                                              {
                                                                                "pos": [
                                                                                  400,
                                                                                  411
                                                                                ],
                                                                                "rule": "prefixedName",
                                                                                "inner": "xsd:integer"
                                                                              }
                                                                            ]
                                                                          }
                                                                        }
                                                                      ]
                                                                    }
                                                                  },
                                                                  {
                                                                    "pos": [
                                                                      412,
                                                                      422
                                                                    ],
                                                                    "rule": "propertyNot",
                                                                    "inner": {
                                                                      "pos": [
                                                                        412,
                                                                        422
                                                                      ],
                                                                      "pairs": [
                                                                        {
                                                                          "pos": [
                                                                            412,
                                                                            422
                                                                          ],
                                                                          "rule": "propertyAtom",
                                                                          "inner": {
                                                                            "pos": [
                                                                              412,
                                                                              422
                                                                            ],
                                                                            "pairs": [
                                                                              {
                                                                                "pos": [
                                                                                  412,
                                                                                  422
                                                                                ],
                                                                                "rule": "prefixedName",
                                                                                "inner": "xsd:string"
                                                                              }
                                                                            ]
                                                                          }
                                                                        }
                                                                      ]
                                                                    }
                                                                  }
                                                                ]
                                                              }
                                                            },
                                                            {
                                                              "pos": [
                                                                423,
                                                                429
                                                              ],
                                                              "rule": "propertyCount",
                                                              "inner": {
                                                                "pos": [
                                                                  424,
                                                                  428
                                                                ],
                                                                "pairs": [
                                                                  {
                                                                    "pos": [
                                                                      424,
                                                                      425
                                                                    ],
                                                                    "rule": "propertyMinCount",
                                                                    "inner": {
                                                                      "pos": [
                                                                        424,
                                                                        425
                                                                      ],
                                                                      "pairs": [
                                                                        {
                                                                          "pos": [
                                                                            424,
                                                                            425
                                                                          ],
                                                                          "rule": "INTEGER",
                                                                          "inner": "1"
                                                                        }
                                                                      ]
                                                                    }
                                                                  },
                                                                  {
                                                                    "pos": [
                                                                      427,
                                                                      428
                                                                    ],
                                                                    "rule": "propertyMaxCount",
                                                                    "inner": {
                                                                      "pos": [
                                                                        427,
                                                                        428
                                                                      ],
                                                                      "pairs": [
                                                                        {
                                                                          "pos": [
                                                                            427,
                                                                            428
                                                                          ],
                                                                          "rule": "INTEGER",
                                                                          "inner": "1"
                                                                        }
                                                                      ]
                                                                    }
                                                                  }
                                                                ]
                                                              }
                                                            },
                                                            {
                                                              "pos": [
                                                                430,
                                                                442
                                                              ],
                                                              "rule": "propertyOr",
                                                              "inner": {
                                                                "pos": [
                                                                  430,
                                                                  441
                                                                ],
                                                                "pairs": [
                                                                  {
                                                                    "pos": [
                                                                      430,
                                                                      441
                                                                    ],
                                                                    "rule": "propertyNot",
                                                                    "inner": {
                                                                      "pos": [
                                                                        430,
                                                                        441
                                                                      ],
                                                                      "pairs": [
                                                                        {
                                                                          "pos": [
                                                                            430,
                                                                            441
                                                                          ],
                                                                          "rule": "propertyAtom",
                                                                          "inner": {
                                                                            "pos": [
                                                                              430,
                                                                              441
                                                                            ],
                                                                            "pairs": [
                                                                              {
                                                                                "pos": [
                                                                                  430,
                                                                                  441
                                                                                ],
                                                                                "rule": "propertyValue",
                                                                                "inner": {
                                                                                  "pos": [
                                                                                    430,
                                                                                    441
                                                                                  ],
                                                                                  "pairs": [
                                                                                    {
                                                                                      "pos": [
                                                                                        430,
                                                                                        439
                                                                                      ],
                                                                                      "rule": "propertyParam",
                                                                                      "inner": "maxLength"
                                                                                    },
                                                                                    {
                                                                                      "pos": [
                                                                                        440,
                                                                                        441
                                                                                      ],
                                                                                      "rule": "iriOrLiteralOrArray",
                                                                                      "inner": {
                                                                                        "pos": [
                                                                                          440,
                                                                                          441
                                                                                        ],
                                                                                        "pairs": [
                                                                                          {
                                                                                            "pos": [
                                                                                              440,
                                                                                              441
                                                                                            ],
                                                                                            "rule": "iriOrLiteral",
                                                                                            "inner": {
                                                                                              "pos": [
                                                                                                440,
                                                                                                441
                                                                                              ],
                                                                                              "pairs": [
                                                                                                {
                                                                                                  "pos": [
                                                                                                    440,
                                                                                                    441
                                                                                                  ],
                                                                                                  "rule": "literal",
                                                                                                  "inner": {
                                                                                                    "pos": [
                                                                                                      440,
                                                                                                      441
                                                                                                    ],
                                                                                                    "pairs": [
                                                                                                      {
                                                                                                        "pos": [
                                                                                                          440,
                                                                                                          441
                                                                                                        ],
                                                                                                        "rule": "numericLiteral",
                                                                                                        "inner": "5"
                                                                                                      }
                                                                                                    ]
                                                                                                  }
                                                                                                }
                                                                                              ]
                                                                                            }
                                                                                          }
                                                                                        ]
                                                                                      }
                                                                                    }
                                                                                  ]
                                                                                }
                                                                              }
                                                                            ]
                                                                          }
                                                                        }
                                                                      ]
                                                                    }
                                                                  }
                                                                ]
                                                              }
                                                            }
                                                          ]
                                                        }
                                                      }
                                                    ]
                                                  }
                                                }
                                              ]
                                            }
                                          }
                                        ]
                                      }
                                    }
                                  ]
                                }
                              }
                            ]
                          }
                        }
                      ]
                    }
                  }
                ]
              }
            },
            {
              "pos": [
                454,
                454
              ],
              "rule": "EOI",
              "inner": ""
            }
          ]
        }
        "#);
    }
}
