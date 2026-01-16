use crate::ast::{BlockStmt, ExprStmt, Stmt};
use crate::parser::lexer::TokenKind;
use crate::parser::{ParseError, Parser};

/// Parse manta statements
pub fn parse_statement(parser: &mut Parser) -> Result<Stmt, ParseError> {
    // need to peek here because we don't know if this is an expression or a statement yet
    let token = parser.lookahead(0)?;
    let token_kind = token.kind;

    let parselet = parser.prefix_stmt_parselets.get(&token_kind);
    if let Some(parselet) = parselet {
        let parselet = parselet.clone();
        let token = parser.consume()?;
        let stmt = parselet.parse(parser, token)?;

        // Consume trailing semicolon for prefix statements
        let matched = parser.match_token(TokenKind::Semicolon)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                parser.lookahead(0)?.clone(),
                "missing ';'".to_string(),
            ));
        }

        return Ok(stmt);
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
        let stmt = parselet.parse(parser, expr, token)?;

        let matched = parser.match_token(TokenKind::Semicolon)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                parser.lookahead(0)?.clone(),
                "missing ';'".to_string(),
            ));
        }

        Ok(stmt)
    } else {
        let matched = parser.match_token(TokenKind::Semicolon)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                parser.lookahead(0)?.clone(),
                "missing ';'".to_string(),
            ));
        }

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

        let eof_token = parser.lookahead(0)?;
        if eof_token.kind == TokenKind::Eof {
            return Err(ParseError::UnexpectedToken(
                eof_token.clone(),
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
        AssignStmt, BinaryExpr, BinaryOp, BlockStmt, CallExpr, DeferStmt, DotAccessExpr,
        DotAccessPat, EnumVariantPat, Expr, FreeExpr, IdentifierExpr, IdentifierPat, IfStmt,
        IndexExpr, LetStmt, MatchArm, MatchStmt, ModuleAccesPat, ModuleAccessExpr, NewExpr,
        Pattern, PayloadPat, ReturnStmt, Stmt, TypeSpec, UnaryExpr, UnaryOp,
    };
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
            input: "let x = 10",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::Identifier(IdentifierPat {
                        name: "x".to_string()
                    }),
                    value: Expr::IntLiteral(10),
                    or_binding: None,
                    except: None,
                }
            ),
        },
        parse_stmt_let_with_value {
            input: "let bool(y) = true",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::Payload(PayloadPat {
                        pat: Box::new(Pattern::Identifier(IdentifierPat {
                            name: "bool".to_string()
                        })),
                        payload: "y".to_string()
                    }),
                    value: Expr::BoolLiteral(true),
                    or_binding: None,
                    except: None,
                },
            ),
        },
        parse_stmt_let_no_type {
            input: "let pi = 3.45",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::Identifier(IdentifierPat {
                        name: "pi".to_string()
                    }),
                    value: Expr::FloatLiteral(3.45),
                    or_binding: None,
                    except: None,
                },
            ),
        },
        parse_stmt_let_user_type {
            input: "let Person(jill) = new_person()",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::Payload(PayloadPat {
                        pat: Box::new(Pattern::Identifier(IdentifierPat {
                            name: "Person".to_string(),
                        })),
                        payload: "jill".to_string()
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "new_person".to_string()
                        })),
                        args: vec![],
                    }),
                    or_binding: None,
                    except: None,
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
                            func: Box::new(Expr::DotAccess(DotAccessExpr {
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
            input: r###"{
    let a = 10
    let .Ok(b) = maybe_int(42) !
    let c = a + b
}"###,
            want_var: Stmt::Block(stmt),
            want_value: assert_eq!(
                stmt,
                BlockStmt {
                    statements: vec![
                        Stmt::Let(LetStmt {
                            pattern: Pattern::Identifier(IdentifierPat {
                                name: "a".to_string()
                            }),
                            value: Expr::IntLiteral(10),
                            or_binding: None,
                            except: None,
                        }),
                        Stmt::Let(LetStmt {
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        name: "Ok".to_string()
                                    },
                                })),
                                payload: "b".to_string()
                            }),
                            value: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "maybe_int".to_string()
                                })),
                                args: vec![Expr::IntLiteral(42)],
                            }),
                            or_binding: Some(Box::new(IdentifierExpr {
                                name: "e".to_string()
                            })),
                            except: Some(BlockStmt {
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            name: "panic".to_string()
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            name: "e".to_string(),
                                        })],
                                    })
                                })],
                            }),
                        }),
                        Stmt::Let(LetStmt {
                            pattern: Pattern::Identifier(IdentifierPat {
                                name: "c".to_string()
                            }),
                            value: Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "a".to_string()
                                })),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "b".to_string()
                                })),
                            }),
                            or_binding: None,
                            except: None,
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
        parse_stmt_ptr_assign {
            input: "*p = 42",
            want_var: Stmt::Assign(stmt),
            want_value: assert_eq!(
                stmt,
                AssignStmt {
                    lvalue: Expr::Unary(UnaryExpr {
                        operator: UnaryOp::Dereference,
                        operand: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "p".to_string(),
                        })),
                    }),
                    rvalue: Expr::IntLiteral(42),
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
                        func: Box::new(Expr::DotAccess(DotAccessExpr {
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
        parse_stmt_if {
            input: r###"if true {
    print("ok")
}"###,
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
            input: r###"if a < 13 {
    print("ok")
} else {
    a = 10 + number(3.45)
}"###,
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
                                    args: vec![Expr::FloatLiteral(3.45)],
                                })),
                            })
                        }),]
                    })
                }
            ),
        },
        parse_stmt_return_enum_variant {
            input: "return .Ok",
            want_var: Stmt::Return(stmt),
            want_value: assert_eq!(
                stmt,
                ReturnStmt {
                    value: Some(Expr::DotAccess(DotAccessExpr {
                        target: None,
                        field: Box::new(IdentifierExpr {
                            name: "Ok".to_string(),
                        }),
                    })),
                }
            ),
        },
        parse_stmt_let_with_panic {
            input: "let .Ok = call() !",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::DotAccess(DotAccessPat {
                        target: None,
                        field: IdentifierPat {
                            name: "Ok".to_string(),
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "call".to_string()
                        })),
                        args: vec![],
                    }),
                    or_binding: Some(Box::new(IdentifierExpr {
                        name: "e".to_string()
                    })),
                    except: Some(BlockStmt {
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "panic".to_string()
                                })),
                                args: vec![Expr::Identifier(IdentifierExpr {
                                    name: "e".to_string()
                                })],
                            }),
                        })],
                    }),
                },
            ),
        },
        parse_stmt_try_simple_catch {
            input: r###"let Ret.Valid(v) = validate("data") or {
    print("invalid!")
}"###,
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::Payload(PayloadPat {
                        pat: Box::new(Pattern::DotAccess(DotAccessPat {
                            target: Some(Box::new(Pattern::Identifier(IdentifierPat {
                                name: "Ret".to_string(),
                            }))),
                            field: IdentifierPat {
                                name: "Valid".to_string(),
                            },
                        })),
                        payload: "v".to_string()
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "validate".to_string()
                        })),
                        args: vec![Expr::StringLiteral("data".to_string())],
                    }),
                    or_binding: None,
                    except: Some(BlockStmt {
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "print".to_string(),
                                })),
                                args: vec![Expr::StringLiteral("invalid!".to_string())],
                            }),
                        })],
                    }),
                },
            ),
        },
        parse_stmt_try_catch_binding {
            input: r###"let .Err = build_item(name, false) or(i) {
    print("built item")
    return i
}"###,
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::DotAccess(DotAccessPat {
                        target: None,
                        field: IdentifierPat {
                            name: "Err".to_string()
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "build_item".to_string(),
                        })),
                        args: vec![
                            Expr::Identifier(IdentifierExpr {
                                name: "name".to_string()
                            }),
                            Expr::BoolLiteral(false),
                        ],
                    }),
                    or_binding: Some(Box::new(IdentifierExpr {
                        name: "i".to_string()
                    })),
                    except: Some(BlockStmt {
                        statements: vec![
                            Stmt::Expr(ExprStmt {
                                expr: Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        name: "print".to_string()
                                    })),
                                    args: vec![Expr::StringLiteral("built item".to_string())],
                                })
                            }),
                            Stmt::Return(ReturnStmt {
                                value: Some(Expr::Identifier(IdentifierExpr {
                                    name: "i".to_string()
                                })),
                            }),
                        ],
                    }),
                }
            ),
        },
        parse_stmt_let_with_wrap {
            input: "let .Ok(d) = div() wrap .Err",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::Payload(PayloadPat {
                        pat: Box::new(Pattern::DotAccess(DotAccessPat {
                            target: None,
                            field: IdentifierPat {
                                name: "Ok".to_string()
                            },
                        })),
                        payload: "d".to_string()
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "div".to_string()
                        })),
                        args: vec![],
                    }),
                    or_binding: Some(Box::new(IdentifierExpr {
                        name: "e".to_string()
                    })),
                    except: Some(BlockStmt {
                        statements: vec![Stmt::Return(ReturnStmt {
                            value: Some(Expr::Call(CallExpr {
                                func: Box::new(Expr::DotAccess(DotAccessExpr {
                                    target: None,
                                    field: Box::new(IdentifierExpr {
                                        name: "Err".to_string()
                                    })
                                })),
                                args: vec![Expr::Identifier(IdentifierExpr {
                                    name: "e".to_string()
                                })],
                            })),
                        })],
                    }),
                },
            ),
        },
        parse_stmt_match {
            input: r###"match x {
    .Some(v) { print(v) }
    .None { print("none") }
}"###,
            want_var: Stmt::Match(stmt),
            want_value: assert_eq!(
                stmt,
                MatchStmt {
                    target: Expr::Identifier(IdentifierExpr {
                        name: "x".to_string(),
                    }),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        name: "Some".to_string()
                                    },
                                })),
                                payload: "v".to_string(),
                            }),
                            body: BlockStmt {
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            name: "print".to_string(),
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            name: "v".to_string(),
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            pattern: Pattern::DotAccess(DotAccessPat {
                                target: None,
                                field: IdentifierPat {
                                    name: "None".to_string()
                                },
                            }),
                            body: BlockStmt {
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            name: "print".to_string(),
                                        })),
                                        args: vec![Expr::StringLiteral("none".to_string())],
                                    }),
                                })],
                            },
                        },
                    ],
                }
            ),
        },
        parse_stmt_match_mixed_patterns {
            input: r###"match result {
    .Success(val) { print(val) }
    .Warning(msg) { print(msg) }
    .Failed { print("error") }
}"###,
            want_var: Stmt::Match(stmt),
            want_value: assert_eq!(
                stmt,
                MatchStmt {
                    target: Expr::Identifier(IdentifierExpr {
                        name: "result".to_string(),
                    }),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        name: "Success".to_string(),
                                    },
                                })),
                                payload: "val".to_string()
                            }),
                            body: BlockStmt {
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            name: "print".to_string(),
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            name: "val".to_string(),
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        name: "Warning".to_string(),
                                    },
                                })),
                                payload: "msg".to_string()
                            }),
                            body: BlockStmt {
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            name: "print".to_string(),
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            name: "msg".to_string(),
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            pattern: Pattern::DotAccess(DotAccessPat {
                                target: None,
                                field: IdentifierPat {
                                    name: "Failed".to_string()
                                },
                            }),
                            body: BlockStmt {
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            name: "print".to_string(),
                                        })),
                                        args: vec![Expr::StringLiteral("error".to_string())],
                                    }),
                                })],
                            },
                        },
                    ],
                }
            ),
        },
        parse_stmt_match_empty_payload {
            input: r###"match signal {
    .Ready { print("go") }
    .Idle(ts) { print("idle since", ts) }
}"###,
            want_var: Stmt::Match(stmt),
            want_value: assert_eq!(
                stmt,
                MatchStmt {
                    target: Expr::Identifier(IdentifierExpr {
                        name: "signal".to_string(),
                    }),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::DotAccess(DotAccessPat {
                                target: None,
                                field: IdentifierPat {
                                    name: "Ready".to_string()
                                },
                            }),
                            body: BlockStmt {
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            name: "print".to_string(),
                                        })),
                                        args: vec![Expr::StringLiteral("go".to_string())],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        name: "Idle".to_string(),
                                    },
                                })),
                                payload: "ts".to_string()
                            }),
                            body: BlockStmt {
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            name: "print".to_string(),
                                        })),
                                        args: vec![
                                            Expr::StringLiteral("idle since".to_string()),
                                            Expr::Identifier(IdentifierExpr {
                                                name: "ts".to_string(),
                                            }),
                                        ],
                                    }),
                                })],
                            },
                        },
                    ],
                }
            ),
        },
        parse_stmt_module_access_identifier {
            input: "fmt::println",
            want_var: Stmt::Expr(stmt),
            want_value: assert_eq!(
                stmt,
                ExprStmt {
                    expr: Expr::ModuleAccess(ModuleAccessExpr {
                        module: Box::new(IdentifierExpr {
                            name: "fmt".to_string(),
                        }),
                        expr: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "println".to_string(),
                        })),
                    }),
                }
            ),
        },
        parse_stmt_module_access_call {
            input: "fmt::println(\"hello\")",
            want_var: Stmt::Expr(stmt),
            want_value: assert_eq!(
                stmt,
                ExprStmt {
                    expr: Expr::ModuleAccess(ModuleAccessExpr {
                        module: Box::new(IdentifierExpr {
                            name: "fmt".to_string(),
                        }),
                        expr: Box::new(Expr::Call(CallExpr {
                            func: Box::new(Expr::Identifier(IdentifierExpr {
                                name: "println".to_string(),
                            })),
                            args: vec![Expr::StringLiteral("hello".to_string())],
                        })),
                    }),
                }
            ),
        },
        parse_stmt_module_access_type {
            input: "math::vec3",
            want_var: Stmt::Expr(stmt),
            want_value: assert_eq!(
                stmt,
                ExprStmt {
                    expr: Expr::ModuleAccess(ModuleAccessExpr {
                        module: Box::new(IdentifierExpr {
                            name: "math".to_string(),
                        }),
                        expr: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "vec3".to_string(),
                        })),
                    }),
                }
            ),
        },
    );
}
