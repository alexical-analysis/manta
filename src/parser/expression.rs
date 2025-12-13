use super::Precedence;
use crate::ast::Expr;
use crate::parser::lexer::TokenKind;
use crate::parser::{ParseError, Parser};

/// Parse an expression with a minimum precedence level.
/// This implements the Pratt parser algorithm.
pub fn parse_expression(
    parser: &mut Parser,
    min_precedence: Precedence,
) -> Result<Expr, ParseError> {
    let token = parser.consume()?;

    let prefix_opt = parser.prefix_expr_parselets.get(&token.kind);
    if prefix_opt.is_none() {
        return Err(ParseError::UnexpectedToken(format!(
            "No prefix parselet for token kind: {:?}",
            token.kind
        )));
    };

    let prefix = prefix_opt.unwrap().clone();
    let mut left = prefix.parse(parser, token)?;

    // Loop while the next token's precedence is higher than or equal to min_precedence
    loop {
        let next_token = parser.lookahead(0)?.clone();
        if expression_done(&next_token.kind) {
            break;
        }

        let next_precedence = parser.get_precedence(&next_token.kind);
        if next_precedence.is_err() {
            break;
        }
        let next_precedence = next_precedence.unwrap();

        if next_precedence <= min_precedence {
            break;
        }

        let token = parser.consume()?;
        let infix_opt = parser.infix_expr_parselets.get(&token.kind);
        if infix_opt.is_none() {
            break;
        }
        let infix = infix_opt.unwrap().clone();

        left = infix.parse(parser, left, token)?;
    }

    Ok(left)
}

/// Check if the expression is done based on the next token kind
fn expression_done(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Eof
            | TokenKind::CloseParen
            | TokenKind::CloseSquare
            | TokenKind::CloseBrace
            | TokenKind::Comma
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        BinaryExpr, BinaryOp, CallExpr, Expr, FieldAccessExpr, FreeExpr, IdentifierExpr, IndexExpr,
        NewExpr, TypeSpec, UnaryExpr, UnaryOp,
    };
    use crate::parser::lexer::Lexer;
    use pretty_assertions::assert_eq;

    macro_rules! test_parse_expressions {
        ( $( $case:ident { input: $input:expr, want_var: $want_var:pat, want_value: $want_value:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let lexer = Lexer::new($input);
                    let mut parser = Parser::new(lexer);

                    let expr = parse_expression(&mut parser, Precedence::Base).unwrap();
                    match expr {
                        $want_var => $want_value,
                        _ => panic!("Expected {:?} => {{ {:?} }}, got {:?}", stringify!($want_var), stringify!($want_value), expr),
                    }
                }
            )*
        };
    }

    test_parse_expressions!(
        parse_expression_int_literal {
            input: "42",
            want_var: Expr::IntLiteral(42),
            want_value: (),
        },
        parse_expression_float_literal {
            input: "3.14",
            want_var: Expr::FloatLiteral(3.14),
            want_value: (),
        },
        parse_expression_string_literal {
            input: r#""hello world""#,
            want_var: Expr::StringLiteral(s),
            want_value: assert_eq!(s, "hello world"),
        },
        parse_expression_bool_true {
            input: "true",
            want_var: Expr::BoolLiteral(true),
            want_value: (),
        },
        parse_expression_bool_false {
            input: "false",
            want_var: Expr::BoolLiteral(false),
            want_value: (),
        },
        parse_expression_nil_literal {
            input: "nil",
            want_var: Expr::NilLiteral,
            want_value: (),
        },
        parse_expression_identifier {
            input: "myVariable",
            want_var: Expr::Identifier(ident),
            want_value: assert_eq!(ident.name, "myVariable"),
        },
        parse_expression_identifier_single_char {
            input: "x",
            want_var: Expr::Identifier(ident),
            want_value: assert_eq!(ident.name, "x"),
        },
        parse_expression_negative_int {
            input: "-42",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Negate,
                    operand: Box::new(Expr::IntLiteral(42)),
                }
            ),
        },
        parse_expression_positive_int {
            input: "+42",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Positive,
                    operand: Box::new(Expr::IntLiteral(42)),
                },
            ),
        },
        parse_expression_positive_negative_int {
            input: "+-42",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Positive,
                    operand: Box::new(Expr::Unary(UnaryExpr {
                        operator: UnaryOp::Negate,
                        operand: Box::new(Expr::IntLiteral(42)),
                    })),
                },
            ),
        },
        parse_expression_not_bool {
            input: "!true",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Not,
                    operand: Box::new(Expr::BoolLiteral(true)),
                },
            ),
        },
        parse_expression_dereference {
            input: "*ptr",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Dereference,
                    operand: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "ptr".to_string(),
                    })),
                },
            ),
        },
        parse_expression_address_of {
            input: "&var",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::AddressOf,
                    operand: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "var".to_string(),
                    })),
                },
            ),
        },
        parse_expression_double_dereference {
            input: "**ptr",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Dereference,
                    operand: Box::new(Expr::Unary(UnaryExpr {
                        operator: UnaryOp::Dereference,
                        operand: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "ptr".to_string(),
                        })),
                    })),
                },
            ),
        },
        parse_expression_negative_not {
            input: "-!x",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Negate,
                    operand: Box::new(Expr::Unary(UnaryExpr {
                        operator: UnaryOp::Not,
                        operand: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "x".to_string(),
                        })),
                    })),
                },
            ),
        },
        parse_expression_grouped_negative {
            input: "(-x)",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Negate,
                    operand: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "x".to_string(),
                    })),
                },
            ),
        },
        parse_expression_negative_grouped_not {
            input: "-(!y)",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Negate,
                    operand: Box::new(Expr::Unary(UnaryExpr {
                        operator: UnaryOp::Not,
                        operand: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "y".to_string(),
                        })),
                    })),
                }
            ),
        },
        parse_expression_addition {
            input: "1 + 2",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(1)),
                    operator: BinaryOp::Add,
                    right: Box::new(Expr::IntLiteral(2)),
                },
            ),
        },
        parse_expression_multiplication {
            input: "3 * 4",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(3)),
                    operator: BinaryOp::Multiply,
                    right: Box::new(Expr::IntLiteral(4)),
                },
            ),
        },
        parse_expression_subtraction {
            input: "10 - 3",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(10)),
                    operator: BinaryOp::Subtract,
                    right: Box::new(Expr::IntLiteral(3)),
                },
            ),
        },
        parse_expression_division {
            input: "20 / 4",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(20)),
                    operator: BinaryOp::Divide,
                    right: Box::new(Expr::IntLiteral(4)),
                },
            ),
        },
        parse_expression_modulo {
            input: "10 % 3",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(10)),
                    operator: BinaryOp::Modulo,
                    right: Box::new(Expr::IntLiteral(3)),
                },
            ),
        },
        parse_expression_multiply_over_add {
            input: "1 + 2 * 3",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(1)),
                    operator: BinaryOp::Add,
                    right: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::IntLiteral(2)),
                        operator: BinaryOp::Multiply,
                        right: Box::new(Expr::IntLiteral(3)),
                    })),
                },
            ),
        },
        parse_expression_associative_addition {
            input: "1 + 2 + 3",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::IntLiteral(1)),
                        operator: BinaryOp::Add,
                        right: Box::new(Expr::IntLiteral(2)),
                    })),
                    operator: BinaryOp::Add,
                    right: Box::new(Expr::IntLiteral(3)),
                },
            ),
        },
        parse_expression_equality_check {
            input: "a == b",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "a".to_string(),
                    })),
                    operator: BinaryOp::Equal,
                    right: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "b".to_string(),
                    })),
                }
            ),
        },
        parse_expression_inequality_check {
            input: "x != y",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "x".to_string(),
                    })),
                    operator: BinaryOp::NotEqual,
                    right: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "y".to_string(),
                    })),
                },
            ),
        },
        parse_expression_less_than {
            input: "4 < b",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(4)),
                    operator: BinaryOp::LessThan,
                    right: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "b".to_string(),
                    })),
                },
            ),
        },
        parse_expression_greater_than {
            input: "x > y",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "x".to_string(),
                    })),
                    operator: BinaryOp::GreaterThan,
                    right: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "y".to_string(),
                    })),
                },
            ),
        },
        parse_expression_less_or_equal {
            input: "a <= b",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "a".to_string(),
                    })),
                    operator: BinaryOp::LessThanOrEqual,
                    right: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "b".to_string(),
                    })),
                },
            ),
        },
        parse_expression_greater_or_equal {
            input: "x >= 9",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "x".to_string(),
                    })),
                    operator: BinaryOp::GreaterThanOrEqual,
                    right: Box::new(Expr::IntLiteral(9)),
                },
            ),
        },
        parse_expression_bool_expression {
            input: "3.14 == b && true != d",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::FloatLiteral(3.14)),
                        operator: BinaryOp::Equal,
                        right: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "b".to_string(),
                        })),
                    })),
                    operator: BinaryOp::LogicalAnd,
                    right: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::BoolLiteral(true)),
                        operator: BinaryOp::NotEqual,
                        right: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "d".to_string()
                        })),
                    })),
                },
            ),
        },
        parse_expression_or_over_and {
            input: "1 | 2 & b",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(1)),
                    operator: BinaryOp::BitwiseOr,
                    right: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::IntLiteral(2)),
                        operator: BinaryOp::BitwiseAnd,
                        right: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "b".to_string(),
                        })),
                    })),
                },
            ),
        },
        parse_expression_call_no_args {
            input: "print()",
            want_var: Expr::Call(expr),
            want_value: assert_eq!(
                expr,
                CallExpr {
                    func: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "print".to_string(),
                    })),
                    args: vec![],
                },
            ),
        },
        parse_expression_call_with_args {
            input: "sum(1, b, 3)",
            want_var: Expr::Call(expr),
            want_value: assert_eq!(
                expr,
                CallExpr {
                    func: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "sum".to_string(),
                    })),
                    args: vec![
                        Expr::IntLiteral(1),
                        Expr::Identifier(IdentifierExpr {
                            name: "b".to_string()
                        }),
                        Expr::IntLiteral(3),
                    ],
                },
            ),
        },
        parse_expression_index_access {
            input: "arr[5]",
            want_var: Expr::Index(expr),
            want_value: assert_eq!(
                expr,
                IndexExpr {
                    target: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "arr".to_string(),
                    })),
                    index: Box::new(Expr::IntLiteral(5)),
                },
            ),
        },
        parse_expression_index_access_complex {
            input: "matrix[i + 1]",
            want_var: Expr::Index(expr),
            want_value: assert_eq!(
                expr,
                IndexExpr {
                    target: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "matrix".to_string(),
                    })),
                    index: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "i".to_string(),
                        })),
                        operator: BinaryOp::Add,
                        right: Box::new(Expr::IntLiteral(1)),
                    })),
                },
            ),
        },
        parse_expression_index_access_nested {
            input: "data[rows[i]]",
            want_var: Expr::Index(expr),
            want_value: assert_eq!(
                expr,
                IndexExpr {
                    target: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "data".to_string(),
                    })),
                    index: Box::new(Expr::Index(IndexExpr {
                        target: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "rows".to_string(),
                        })),
                        index: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "i".to_string(),
                        })),
                    })),
                },
            ),
        },
        parse_expression_index_access_3d {
            input: "tensor[x][y][z]",
            want_var: Expr::Index(expr),
            want_value: assert_eq!(
                expr,
                IndexExpr {
                    target: Box::new(Expr::Index(IndexExpr {
                        target: Box::new(Expr::Index(IndexExpr {
                            target: Box::new(Expr::Identifier(IdentifierExpr {
                                name: "tensor".to_string()
                            })),
                            index: Box::new(Expr::Identifier(IdentifierExpr {
                                name: "x".to_string(),
                            })),
                        })),
                        index: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "y".to_string(),
                        })),
                    })),
                    index: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "z".to_string(),
                    })),
                },
            ),
        },
        parse_expression_field_access {
            input: "person.name",
            want_var: Expr::FieldAccess(expr),
            want_value: assert_eq!(
                expr,
                FieldAccessExpr {
                    target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                        name: "person".to_string(),
                    }))),
                    field: Box::new(IdentifierExpr {
                        name: "name".to_string(),
                    }),
                }
            ),
        },
        parse_expression_field_access_nested {
            input: "company.ceo.name",
            want_var: Expr::FieldAccess(expr),
            want_value: assert_eq!(
                expr,
                FieldAccessExpr {
                    target: Some(Box::new(Expr::FieldAccess(FieldAccessExpr {
                        target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                            name: "company".to_string(),
                        }))),
                        field: Box::new(IdentifierExpr {
                            name: "ceo".to_string(),
                        }),
                    }))),
                    field: Box::new(IdentifierExpr {
                        name: "name".to_string(),
                    }),
                },
            ),
        },
        parse_expression_field_access_index {
            input: "users[0].email",
            want_var: Expr::FieldAccess(expr),
            want_value: assert_eq!(
                expr,
                FieldAccessExpr {
                    target: Some(Box::new(Expr::Index(IndexExpr {
                        target: Box::new(Expr::Identifier(IdentifierExpr {
                            name: "users".to_string(),
                        })),
                        index: Box::new(Expr::IntLiteral(0)),
                    }))),
                    field: Box::new(IdentifierExpr {
                        name: "email".to_string(),
                    }),
                },
            ),
        },
        parse_expression_field_access_call {
            input: "config.getDatabase().host",
            want_var: Expr::FieldAccess(expr),
            want_value: assert_eq!(
                expr,
                FieldAccessExpr {
                    target: Some(Box::new(Expr::Call(CallExpr {
                        func: Box::new(Expr::FieldAccess(FieldAccessExpr {
                            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                                name: "config".to_string(),
                            }))),
                            field: Box::new(IdentifierExpr {
                                name: "getDatabase".to_string(),
                            }),
                        })),
                        args: vec![],
                    }))),
                    field: Box::new(IdentifierExpr {
                        name: "host".to_string(),
                    }),
                },
            ),
        },
        parse_expression_call_new {
            input: "new(i32)",
            want_var: Expr::New(expr),
            want_value: assert_eq!(
                expr,
                NewExpr {
                    type_spec: TypeSpec::Int32,
                    len: None,
                    cap: None,
                }
            ),
        },
        parse_expression_call_new_slice {
            input: "new([]f64, 10)",
            want_var: Expr::New(expr),
            want_value: assert_eq!(
                expr,
                NewExpr {
                    type_spec: TypeSpec::Slice(Box::new(TypeSpec::Float64)),
                    len: Some(Box::new(Expr::IntLiteral(10))),
                    cap: None,
                }
            ),
        },
        parse_expression_call_new_slice_with_cap {
            input: "new([ ] bool, 10, 20)",
            want_var: Expr::New(new_expr),
            want_value: {
                match new_expr.type_spec {
                    TypeSpec::Slice(t) => assert_eq!(*t, TypeSpec::Bool),
                    _ => panic!(
                        "Expected TypeSpec::Slice(TypeSpec::Bool), but got {:?}",
                        new_expr.type_spec
                    ),
                }
                match new_expr.len {
                    Some(len_expr) => match *len_expr {
                        Expr::IntLiteral(10) => (),
                        _ => panic!("Expected len to be IntLiteral(10)"),
                    },
                    None => panic!("Expected len to be Some"),
                }
                match new_expr.cap {
                    Some(cap_expr) => match *cap_expr {
                        Expr::IntLiteral(20) => (),
                        _ => panic!("Expected cap to be IntLiteral(20)"),
                    },
                    None => panic!("Expected cap to be Some"),
                }
            },
        },
        parse_expression_call_new_method {
            input: "new(i32).method()",
            want_var: Expr::Call(expr),
            want_value: assert_eq!(
                expr,
                CallExpr {
                    func: Box::new(Expr::FieldAccess(FieldAccessExpr {
                        target: Some(Box::new(Expr::New(NewExpr {
                            type_spec: TypeSpec::Int32,
                            len: None,
                            cap: None,
                        }))),
                        field: Box::new(IdentifierExpr {
                            name: "method".to_string(),
                        }),
                    })),
                    args: vec![],
                }
            ),
        },
        parse_expression_call_free {
            input: "free(ptr)",
            want_var: Expr::Free(expr),
            want_value: assert_eq!(
                expr,
                FreeExpr {
                    expr: Box::new(Expr::Identifier(IdentifierExpr {
                        name: "ptr".to_string(),
                    })),
                }
            ),
        },
        parse_expression_call_free_complex {
            input: "free(data.buffer.ptr)",
            want_var: Expr::Free(expr),
            want_value: assert_eq!(
                expr,
                FreeExpr {
                    expr: Box::new(Expr::FieldAccess(FieldAccessExpr {
                        target: Some(Box::new(Expr::FieldAccess(FieldAccessExpr {
                            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                                name: "data".to_string(),
                            }))),
                            field: Box::new(IdentifierExpr {
                                name: "buffer".to_string(),
                            }),
                        }))),
                        field: Box::new(IdentifierExpr {
                            name: "ptr".to_string(),
                        }),
                    })),
                }
            ),
        },
    );
}
