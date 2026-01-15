use crate::ast::{BlockStmt, ExprStmt, Pattern, Stmt, TypeSpec};
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
            return Err(ParseError::UnexpectedToken("missing ';'".to_string()));
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
            return Err(ParseError::UnexpectedToken("missing ';'".to_string()));
        }

        Ok(stmt)
    } else {
        let matched = parser.match_token(TokenKind::Semicolon)?;
        if !matched {
            return Err(ParseError::UnexpectedToken("missing ';'".to_string()));
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

pub fn parse_pattern(parser: &mut Parser) -> Result<Pattern, ParseError> {
    let token = parser.lookahead(0)?;

    match token.kind {
        TokenKind::Identifier => {
            let token = parser.consume()?;
            let next = parser.lookahead(0)?;
            match next.kind {
                TokenKind::Dot => {
                    parser.consume()?;
                    parse_enum_pattern(parser, Some(token.lexeme))
                }
                TokenKind::OpenParen => {
                    parser.consume()?;
                    if parser.lookahead(0)?.kind == TokenKind::Identifier {
                        let payload_binding = parser.consume()?.lexeme;
                        let type_spec = match token.lexeme.as_str() {
                            "i8" => TypeSpec::Int8,
                            "i16" => TypeSpec::Int16,
                            "i32" => TypeSpec::Int32,
                            "i64" => TypeSpec::Int64,
                            "u8" => TypeSpec::UInt8,
                            "u16" => TypeSpec::UInt16,
                            "u32" => TypeSpec::UInt32,
                            "u64" => TypeSpec::UInt64,
                            "f32" => TypeSpec::Float32,
                            "f64" => TypeSpec::Float64,
                            "str" => TypeSpec::String,
                            "bool" => TypeSpec::Bool,
                            s => TypeSpec::Named(s.to_string()),
                        };

                        parser.match_token(TokenKind::CloseParen)?;
                        Ok(Pattern::TypeSpec {
                            type_spec,
                            payload_binding,
                        })
                    } else {
                        Err(ParseError::UnexpectedToken(
                            "invalid type pattern match".to_string(),
                        ))
                    }
                }
                _ => {
                    if token.lexeme == "_" {
                        Ok(Pattern::Default)
                    } else {
                        Ok(Pattern::Identifier(token.lexeme))
                    }
                }
            }
        }
        TokenKind::Int => {
            let token = parser.consume()?;
            let i: i64 = token.lexeme.parse().unwrap();
            Ok(Pattern::IntLiteral(i))
        }
        TokenKind::Str => {
            let token = parser.consume()?;
            Ok(Pattern::StringLiteral(token.lexeme))
        }
        TokenKind::Float => {
            let token = parser.consume()?;
            let f: f64 = token.lexeme.parse().unwrap();
            Ok(Pattern::FloatLiteral(f))
        }
        TokenKind::TrueLiteral => {
            parser.consume()?;
            Ok(Pattern::BoolLiteral(true))
        }
        TokenKind::FalseLiteral => {
            parser.consume()?;
            Ok(Pattern::BoolLiteral(false))
        }
        TokenKind::Underscore => {
            parser.consume()?;
            Ok(Pattern::Default)
        }
        TokenKind::Dot => {
            parser.consume()?;
            parse_enum_pattern(parser, None)
        }
        _ => Err(ParseError::UnexpectedToken(
            "Not a valid pattern".to_string(),
        )),
    }
}

fn parse_enum_pattern(
    parser: &mut Parser,
    type_name: Option<String>,
) -> Result<Pattern, ParseError> {
    let ident = parser.lookahead(0)?;
    if ident.kind != TokenKind::Identifier {
        return Err(ParseError::UnexpectedToken(
            "Not a valid pattern".to_string(),
        ));
    }
    let name = ident.lexeme.clone();

    let mut payload_binding = None;
    parser.consume()?;
    if parser.lookahead(0).unwrap().kind == TokenKind::OpenParen {
        parser.consume()?;
        let payload = parser.lookahead(0)?;
        if payload.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                "Not a valid pattern".to_string(),
            ));
        }

        payload_binding = Some(payload.lexeme.clone());
        parser.consume()?;

        parser.match_token(TokenKind::CloseParen)?;
    }

    Ok(Pattern::EnumVariant {
        type_name,
        name,
        payload_binding,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{
        AssignStmt, BinaryExpr, BinaryOp, BlockStmt, CallExpr, DeferStmt, EnumVariant, Expr,
        FieldAccessExpr, FreeExpr, IdentifierExpr, IfStmt, IndexExpr, LetStmt, MatchArm, MatchStmt,
        NewExpr, Pattern, ReturnStmt, Stmt, TypeSpec, UnaryExpr, UnaryOp,
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
                    pattern: Pattern::Identifier("x".to_string()),
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
                    pattern: Pattern::TypeSpec {
                        type_spec: TypeSpec::Bool,
                        payload_binding: "y".to_string(),
                    },
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
                    pattern: Pattern::Identifier("pi".to_string()),
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
                    pattern: Pattern::TypeSpec {
                        type_spec: TypeSpec::Named("Person".to_string()),
                        payload_binding: "jill".to_string(),
                    },
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
                            pattern: Pattern::Identifier("a".to_string()),
                            value: Expr::IntLiteral(10),
                            or_binding: None,
                            except: None,
                        }),
                        Stmt::Let(LetStmt {
                            pattern: Pattern::EnumVariant {
                                type_name: None,
                                name: "Ok".to_string(),
                                payload_binding: Some("b".to_string()),
                            },
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
                            pattern: Pattern::Identifier("c".to_string()),
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
                    value: Some(Expr::FieldAccess(FieldAccessExpr {
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
                    pattern: Pattern::EnumVariant {
                        type_name: None,
                        name: "Ok".to_string(),
                        payload_binding: None,
                    },
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
                    pattern: Pattern::EnumVariant {
                        type_name: Some("Ret".to_string()),
                        name: "Valid".to_string(),
                        payload_binding: Some("v".to_string()),
                    },
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
                    pattern: Pattern::EnumVariant {
                        type_name: None,
                        name: "Err".to_string(),
                        payload_binding: None
                    },
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
                    pattern: Pattern::EnumVariant {
                        type_name: None,
                        name: "Ok".to_string(),
                        payload_binding: Some("d".to_string())
                    },
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
                                func: Box::new(Expr::FieldAccess(FieldAccessExpr {
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
                            pattern: Pattern::EnumVariant {
                                type_name: None,
                                name: "Some".to_string(),
                                payload_binding: Some("v".to_string()),
                            },
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
                            pattern: Pattern::EnumVariant {
                                type_name: None,
                                name: "None".to_string(),
                                payload_binding: None,
                            },
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
                            pattern: Pattern::EnumVariant {
                                type_name: None,
                                name: "Success".to_string(),
                                payload_binding: Some("val".to_string()),
                            },
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
                            pattern: Pattern::EnumVariant {
                                type_name: None,
                                name: "Warning".to_string(),
                                payload_binding: Some("msg".to_string()),
                            },
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
                            pattern: Pattern::EnumVariant {
                                type_name: None,
                                name: "Failed".to_string(),
                                payload_binding: None,
                            },
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
                            pattern: Pattern::EnumVariant {
                                type_name: None,
                                name: "Ready".to_string(),
                                payload_binding: None,
                            },
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
                            pattern: Pattern::EnumVariant {
                                type_name: None,
                                name: "Idle".to_string(),
                                payload_binding: Some("ts".to_string()),
                            },
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
    );

    macro_rules! test_parse_patterns {
        ( $( $case:ident { input: $input:expr, want: $want:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let lexer = Lexer::new($input);
                    let mut parser = Parser::new(lexer);
                    let pattern = parse_pattern(&mut parser).unwrap();
                    assert_eq!(pattern, $want)
                }
            )*
        }
    }

    test_parse_patterns!(
        test_parse_pattern_int_literal {
            input: "42",
            want: Pattern::IntLiteral(42),
        },
        test_parse_pattern_string_literal {
            input: r###""hello""###,
            want: Pattern::StringLiteral("hello".to_string()),
        },
        test_parse_pattern_float_literal {
            input: "3.45",
            want: Pattern::FloatLiteral(3.45),
        },
        test_parse_pattern_true_literal {
            input: "true",
            want: Pattern::BoolLiteral(true),
        },
        test_parse_pattern_default {
            input: "_",
            want: Pattern::Default,
        },
        test_parse_pattern_identifier {
            input: "my_var",
            want: Pattern::Identifier("my_var".to_string()),
        },
        test_parse_pattern_type_match {
            input: "f32(f)",
            want: Pattern::TypeSpec {
                type_spec: TypeSpec::Float32,
                payload_binding: "f".to_string(),
            },
        },
    );

    #[test]
    fn test_parse_pattern_invalid() {
        let lexer = Lexer::new("+ ");
        let mut parser = Parser::new(lexer);
        let result = parse_pattern(&mut parser);
        assert!(result.is_err());
    }
}
