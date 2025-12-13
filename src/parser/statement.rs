use crate::ast::{BlockStmt, ExprStmt, Stmt};
use crate::parser::lexer::TokenKind;
use crate::parser::{ParseError, Parser, parselets};

/// Parse manta statements
pub fn parse_statement(parser: &mut Parser) -> Result<Stmt, ParseError> {
    // need to peek here because we don't know if this is an expression or a statement yet
    let token = parser.lookahead(0)?;
    let token_kind = token.kind;

    let parselet = parser.prefix_stmt_parselets.get(&token_kind);
    if let Some(parselet) = parselet {
        let parselet = parselet.clone();
        let token = parser.consume()?;
        return parselet.parse(parser, token);
    };

    // if we failed to match a statment, parse this as expression instead
    let expr = parser.parse_expression()?;

    let token = parser.lookahead(0)?;
    let token_kind = token.kind;

    // check if this expression is actually the left hand side of a statement
    let parselet = parser.infix_stmt_parselets.get(&token_kind);
    if let Some(parselet) = parselet {
        let parselet = parselet.clone();
        let token = parser.consume()?;
        Ok(parselet.parse(parser, expr, token)?)
    } else {
        Ok(Stmt::Expr(ExprStmt { expr }))
    }
}

pub fn parse_block(parser: &mut Parser) -> Result<BlockStmt, ParseError> {
    let mut statements = vec![];

    loop {
        let matched = parser.match_token(TokenKind::CloseBrace)?;
        if matched {
            break;
        }

        let matches = parser.match_token(TokenKind::Eof)?;
        if matches {
            return Err(ParseError::UnexpectedToken(
                "missing closing '}' in block".to_string(),
            ));
        }

        let statement = parse_statement(parser)?;
        statements.push(statement);
    }

    Ok(BlockStmt { statements })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{
        AssignStmt, BinaryExpr, BinaryOp, BlockStmt, DeferStmt, Expr, FieldAccessExpr, FreeExpr,
        IdentifierExpr, IfStmt, IndexExpr, NewExpr, ReturnStmt, ShortLetStmt, Stmt,
    };
    use crate::ast::{CallExpr, LetStmt, TypeSpec};
    use crate::parser::lexer::Lexer;
    use pretty_assertions::assert_eq;

    macro_rules! test_parse_statement {
        ( $( $case:ident { input: $input:expr, want_var: $want_var:pat, want_value: $want_value:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let lexer = Lexer::new($input);
                    let mut parser = Parser::new(lexer);

                    let stmt = parse_statement(&mut parser).unwrap();
                    match stmt {
                        $want_var => $want_value,
                        _ => panic!("Expected {} => {}, but got {:?}", stringify!($want_var), stringify!($want_value), stmt),
                    }
                }

            )*
        }
    }

    test_parse_statement!(
        parse_stmt_let {
            input: "let x i32",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    name: "x".to_string(),
                    type_annotation: Some(TypeSpec::Int32),
                    value: None
                }
            ),
        },
        parse_stmt_let_with_value {
            input: "let y bool = true",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    name: "y".to_string(),
                    type_annotation: Some(TypeSpec::Bool),
                    value: Some(Expr::BoolLiteral(true)),
                },
            ),
        },
        parse_stmt_let_no_type {
            input: "let pi = 3.14",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    name: "pi".to_string(),
                    type_annotation: None,
                    value: Some(Expr::FloatLiteral(3.14)),
                }
            ),
        },
        parse_stmt_let_user_type {
            input: "let jill Person = new_person()",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    name: "jill".to_string(),
                    type_annotation: Some(TypeSpec::Named("Person".to_string())),
                    value: Some(Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "new_person".to_string()
                        })),
                        args: vec![],
                    })),
                },
            ),
        },
        parse_stmt_return {
            input: "return",
            want_var: Stmt::Return(stmt),
            want_value: assert_eq!(stmt, ReturnStmt { value: None }),
        },
        parse_stmt_return_value {
            input: "return 10",
            want_var: Stmt::Return(stmt),
            want_value: assert_eq!(
                stmt,
                ReturnStmt {
                    value: Some(Expr::IntLiteral(10))
                }
            ),
        },
        parse_stmt_return_complex_value {
            input: "return builder.string(true)[0]",
            want_var: Stmt::Return(stmt),
            want_value: assert_eq!(
                stmt,
                ReturnStmt {
                    value: Some(Expr::Index(IndexExpr {
                        target: Box::new(Expr::Call(CallExpr {
                            func: Box::new(Expr::FieldAccess(FieldAccessExpr {
                                target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "builder".to_string(),
                                }))),
                                field: Box::new(IdentifierExpr {
                                    name: "string".to_string()
                                }),
                            })),
                            args: vec![Expr::BoolLiteral(true)],
                        })),
                        index: Box::new(Expr::IntLiteral(0)),
                    }))
                }
            ),
        },
        parse_stmt_defer {
            input: "defer {}",
            want_var: Stmt::Defer(stmt),
            want_value: assert_eq!(
                stmt,
                DeferStmt {
                    block: BlockStmt { statements: vec![] }
                }
            ),
        },
        parse_stmt_defer_free {
            input: "defer {\nfree(ptr)\n}",
            want_var: Stmt::Defer(stmt),
            want_value: assert_eq!(
                stmt,
                DeferStmt {
                    block: BlockStmt {
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Free(FreeExpr {
                                expr: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "ptr".to_string()
                                })),
                            })
                        })]
                    }
                }
            ),
        },
        parse_stmt_defer_multi {
            input: "defer {\nprint(\"done:\", err)\nnew(i32)\n}",
            want_var: Stmt::Defer(stmt),
            want_value: assert_eq!(
                stmt,
                DeferStmt {
                    block: BlockStmt {
                        statements: vec![
                            Stmt::Expr(ExprStmt {
                                expr: Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        name: "print".to_string()
                                    })),
                                    args: vec![
                                        Expr::StringLiteral("done:".to_string()),
                                        Expr::Identifier(IdentifierExpr {
                                            name: "err".to_string(),
                                        })
                                    ],
                                })
                            }),
                            Stmt::Expr(ExprStmt {
                                expr: Expr::New(NewExpr {
                                    type_spec: TypeSpec::Int32,
                                    len: None,
                                    cap: None,
                                })
                            })
                        ]
                    }
                }
            ),
        },
        parse_stmt_block {
            input: "{\nlet a = 10\nlet b i16 = 20\nlet c = a + b\n}",
            want_var: Stmt::Block(stmt),
            want_value: assert_eq!(
                stmt,
                BlockStmt {
                    statements: vec![
                        Stmt::Let(LetStmt {
                            name: "a".to_string(),
                            type_annotation: None,
                            value: Some(Expr::IntLiteral(10)),
                        }),
                        Stmt::Let(LetStmt {
                            name: "b".to_string(),
                            type_annotation: Some(TypeSpec::Int16),
                            value: Some(Expr::IntLiteral(20)),
                        }),
                        Stmt::Let(LetStmt {
                            name: "c".to_string(),
                            type_annotation: None,
                            value: Some(Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "a".to_string()
                                })),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "b".to_string()
                                })),
                            })),
                        }),
                    ]
                }
            ),
        },
        parse_stmt_assign {
            input: "x = 10",
            want_var: Stmt::Assign(stmt),
            want_value: assert_eq!(
                stmt,
                AssignStmt {
                    lvalue: Expr::Identifier(IdentifierExpr {
                        name: "x".to_string(),
                    }),
                    rvalue: Expr::IntLiteral(10),
                },
            ),
        },
        parse_stmt_assign_call {
            input: "name = person.name(a, 1 + two())",
            want_var: Stmt::Assign(stmt),
            want_value: assert_eq!(
                stmt,
                AssignStmt {
                    lvalue: Expr::Identifier(IdentifierExpr {
                        name: "name".to_string(),
                    }),
                    rvalue: Expr::Call(CallExpr {
                        func: Box::new(Expr::FieldAccess(FieldAccessExpr {
                            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                                name: "person".to_string()
                            }))),
                            field: Box::new(IdentifierExpr {
                                name: "name".to_string()
                            }),
                        })),
                        args: vec![
                            Expr::Identifier(IdentifierExpr {
                                name: "a".to_string(),
                            }),
                            Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::IntLiteral(1)),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        name: "two".to_string(),
                                    })),
                                    args: vec![],
                                })),
                            })
                        ],
                    })
                }
            ),
        },
        parse_stmt_assign_array {
            input: "a[0] = 10",
            want_var: Stmt::Assign(stmt),
            want_value: assert_eq!(
                stmt,
                AssignStmt {
                    lvalue: Expr::Index(IndexExpr {
                        target: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "a".to_string()
                        })),
                        index: Box::new(Expr::IntLiteral(0)),
                    }),
                    rvalue: Expr::IntLiteral(10),
                },
            ),
        },
        parse_stmt_short_let {
            input: "x := 10",
            want_var: Stmt::ShortLet(stmt),
            want_value: assert_eq!(
                stmt,
                ShortLetStmt {
                    name: IdentifierExpr {
                        name: "x".to_string()
                    },
                    value: Expr::IntLiteral(10),
                },
            ),
        },
        parse_stmt_short_let_index {
            input: "got := test.result.value[2 | 3_000]",
            want_var: Stmt::ShortLet(stmt),
            want_value: assert_eq!(
                stmt,
                ShortLetStmt {
                    name: IdentifierExpr {
                        name: "got".to_string(),
                    },
                    value: Expr::Index(IndexExpr {
                        target: Box::new(Expr::FieldAccess(FieldAccessExpr {
                            target: Some(Box::new(Expr::FieldAccess(FieldAccessExpr {
                                target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "test".to_string(),
                                }))),
                                field: Box::new(IdentifierExpr {
                                    name: "result".to_string(),
                                }),
                            }))),
                            field: Box::new(IdentifierExpr {
                                name: "value".to_string(),
                            }),
                        })),
                        index: Box::new(Expr::Binary(BinaryExpr {
                            left: Box::new(Expr::IntLiteral(2)),
                            operator: BinaryOp::BitwiseOr,
                            right: Box::new(Expr::IntLiteral(3_000)),
                        })),
                    })
                }
            ),
        },
        parse_stmt_if {
            input: "if true {\nprint(\"ok\")\n}",
            want_var: Stmt::If(stmt),
            want_value: assert_eq!(
                stmt,
                IfStmt {
                    check: Box::new(Expr::BoolLiteral(true)),
                    success: BlockStmt {
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "print".to_string(),
                                })),
                                args: vec![Expr::StringLiteral("ok".to_string())],
                            })
                        })],
                    },
                    fail: None,
                },
            ),
        },
        parse_stmt_if_else {
            input: "if a < 13 {\nprint(\"ok\")\n} else {\na = 10 + number(3.14)\n}\n",
            want_var: Stmt::If(stmt),
            want_value: assert_eq!(
                stmt,
                IfStmt {
                    check: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "a".to_string(),
                        })),
                        operator: BinaryOp::LessThan,
                        right: Box::new(Expr::IntLiteral(13)),
                    })),
                    success: BlockStmt {
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "print".to_string(),
                                })),
                                args: vec![Expr::StringLiteral("ok".to_string())],
                            }),
                        })],
                    },
                    fail: Some(BlockStmt {
                        statements: vec![Stmt::Assign(AssignStmt {
                            lvalue: Expr::Identifier(IdentifierExpr {
                                name: "a".to_string(),
                            }),
                            rvalue: Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::IntLiteral(10)),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        name: "number".to_string(),
                                    })),
                                    args: vec![Expr::FloatLiteral(3.14)],
                                })),
                            })
                        }),]
                    })
                }
            ),
        },
    );
}
