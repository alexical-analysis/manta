use crate::ast::{LetStmt, ReturnStmt, Stmt};
use crate::parser::lexer::TokenKind;
use crate::parser::{ParseError, Parser, types};

/// Parse manta statements
pub fn parse_statement(parser: &mut Parser) -> Result<Stmt, ParseError> {
    let token = parser.consume()?;

    match token.kind {
        TokenKind::ReturnKeyword => {
            let next = parser.lookahead(0)?;
            let token_kind = next.kind;

            let value;
            if parser.is_expression_prefix(&token_kind) {
                value = Some(parser.parse_expression()?);
            } else {
                value = None;
            }

            Ok(Stmt::Return(ReturnStmt { value }))
        }
        TokenKind::LetKeyword => {
            let ident = parser.consume()?;
            if ident.kind != TokenKind::Ident {
                return Err(ParseError::UnexpectedToken(
                    "let statment required an identifier".to_string(),
                ));
            }

            let type_spec;
            let mut matched = parser.match_token(TokenKind::Equal)?;
            if matched {
                type_spec = None;
            } else {
                type_spec = Some(types::parse_type(parser)?);
                matched = parser.match_token(TokenKind::Equal)?;
            }

            if !matched && type_spec.is_none() {
                return Err(ParseError::UnexpectedToken(
                    "let statement must have either a type spec or initialization value"
                        .to_string(),
                ));
            }

            // this means the statment follows the format of
            // `let x i32`
            if !matched {
                return Ok(Stmt::Let(LetStmt {
                    name: ident.lexeme,
                    type_annotation: type_spec,
                    initializer: None,
                }));
            }

            let value = parser.parse_expression()?;

            // this covers statements like `let x = 5` as well as
            // `let x bool = true`
            Ok(Stmt::Let(LetStmt {
                name: ident.lexeme,
                type_annotation: type_spec,
                initializer: Some(value),
            }))
        }
        _ => Err(ParseError::UnknownStatement(format!(
            "unknown statment starting with token {}",
            token.kind
        ))),
    }
}

#[cfg(test)]
mod test {
    use serde_json::value::Index;

    use super::*;
    use crate::ast::{Expr, FieldAccess, IdentifierExpr, IndexAccess};
    use crate::ast::{FunctionCall, LetStmt, TypeSpec};
    use crate::parser::lexer::Lexer;

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
                    initializer: None
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
                    initializer: Some(Expr::BoolLiteral(true)),
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
                    initializer: Some(Expr::FloatLiteral(3.14)),
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
                    initializer: Some(Expr::Call(FunctionCall {
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
                    value: Some(Expr::Index(IndexAccess {
                        target: Box::new(Expr::Call(FunctionCall {
                            func: Box::new(Expr::FieldAccess(FieldAccess {
                                target: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "builder".to_string(),
                                })),
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
    );
}
