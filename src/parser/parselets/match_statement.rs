use crate::ast::{BlockStmt, MatchArm, MatchStmt, Pattern, Stmt};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixStmtParselet;
use crate::parser::{statement, ParseError, Parser};

/// Parses match statements
///
/// Example: `match x { .Some(v) { print(v) } .None { print("none") } }`
pub struct MatchParselet;

impl PrefixStmtParselet for MatchParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Stmt, ParseError> {
        let target = parser.parse_expression()?;

        let matched = parser.match_token(TokenKind::OpenBrace)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                "Expected '{' after match expression".to_string(),
            ));
        }

        let mut arms = vec![];

        loop {
            let matched = parser.match_token(TokenKind::CloseBrace)?;
            if matched {
                break;
            }

            let matches = parser.match_token(TokenKind::Eof)?;
            if matches {
                return Err(ParseError::UnexpectedToken(
                    "missing closing '}' in match block".to_string(),
                ));
            }

            let pattern = parse_pattern(parser)?;

            let matched = parser.match_token(TokenKind::OpenBrace)?;
            if !matched {
                return Err(ParseError::UnexpectedToken(
                    "Expected '{' after pattern in match arm".to_string(),
                ));
            }

            let body = statement::parse_block(parser)?;

            arms.push(MatchArm { pattern, body });
        }

        if arms.is_empty() {
            return Err(ParseError::UnexpectedToken(
                "match statement must have at least one arm".to_string(),
            ));
        }

        Ok(Stmt::Match(MatchStmt { target, arms }))
    }
}

/// Parse a pattern in a match arm
fn parse_pattern(parser: &mut Parser) -> Result<Pattern, ParseError> {
    let token = parser.lookahead(0)?;

    match token.kind {
        TokenKind::Dot => {
            parser.consume()?;
            parse_enum_variant(parser)
        }
        _ => Err(ParseError::UnexpectedToken(
            "Expected pattern (e.g., '.Variant' or '.Variant(binding)')".to_string(),
        )),
    }
}

/// Parse enum variant pattern: .Variant or .Variant(binding)
fn parse_enum_variant(parser: &mut Parser) -> Result<Pattern, ParseError> {
    let token = parser.lookahead(0)?;

    if token.kind != TokenKind::Identifier {
        return Err(ParseError::UnexpectedToken(
            "Expected identifier after '.' in enum variant pattern".to_string(),
        ));
    }

    let variant_token = parser.consume()?;
    let name = variant_token.lexeme;

    let mut payload_binding = None;

    // Check for optional payload binding: (identifier)
    if parser.match_token(TokenKind::OpenParen)? {
        let binding_token = parser.lookahead(0)?;

        if binding_token.kind == TokenKind::CloseParen {
            // Empty parens case
            parser.consume()?;
        } else if binding_token.kind == TokenKind::Identifier {
            let ident_token = parser.consume()?;
            payload_binding = Some(ident_token.lexeme);

            let matched = parser.match_token(TokenKind::CloseParen)?;
            if !matched {
                return Err(ParseError::UnexpectedToken(
                    "Expected ')' after binding identifier in enum pattern".to_string(),
                ));
            }
        } else {
            return Err(ParseError::UnexpectedToken(
                "Expected identifier or ')' in enum pattern payload".to_string(),
            ));
        }
    }

    Ok(Pattern::EnumVariant {
        name,
        payload_binding,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{CallExpr, IdentifierExpr};
    use crate::parser::lexer::Lexer;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_match_simple_enum_variants() {
        let input = r#"match x {
            .Some(v) { print(v) }
            .None { print("none") }
        }"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let stmt = statement::parse_statement(&mut parser).unwrap();

        let expected = Stmt::Match(MatchStmt {
            target: crate::ast::Expr::Identifier(IdentifierExpr {
                name: "x".to_string(),
            }),
            arms: vec![
                MatchArm {
                    pattern: Pattern::EnumVariant {
                        name: "Some".to_string(),
                        payload_binding: Some("v".to_string()),
                    },
                    body: BlockStmt {
                        statements: vec![Stmt::Expr(crate::ast::ExprStmt {
                            expr: crate::ast::Expr::Call(CallExpr {
                                func: Box::new(crate::ast::Expr::Identifier(IdentifierExpr {
                                    name: "print".to_string(),
                                })),
                                args: vec![crate::ast::Expr::Identifier(IdentifierExpr {
                                    name: "v".to_string(),
                                })],
                            }),
                        })],
                    },
                },
                MatchArm {
                    pattern: Pattern::EnumVariant {
                        name: "None".to_string(),
                        payload_binding: None,
                    },
                    body: BlockStmt {
                        statements: vec![Stmt::Expr(crate::ast::ExprStmt {
                            expr: crate::ast::Expr::Call(CallExpr {
                                func: Box::new(crate::ast::Expr::Identifier(IdentifierExpr {
                                    name: "print".to_string(),
                                })),
                                args: vec![crate::ast::Expr::StringLiteral("none".to_string())],
                            }),
                        })],
                    },
                },
            ],
        });

        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_match_expression_target() {
        let input = r#"match maybe_div(10, 2) {
            .Some(result) { print(result) }
            .None { print("error") }
        }"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let stmt = statement::parse_statement(&mut parser).unwrap();

        match stmt {
            Stmt::Match(match_stmt) => {
                // Verify target is a function call
                match &match_stmt.target {
                    crate::ast::Expr::Call(call) => {
                        assert!(matches!(call.func.as_ref(), crate::ast::Expr::Identifier(_)));
                        assert_eq!(call.args.len(), 2);
                    }
                    _ => panic!("Expected call expression as match target"),
                }
                // Verify we have 2 arms
                assert_eq!(match_stmt.arms.len(), 2);
            }
            _ => panic!("Expected match statement"),
        }
    }

    #[test]
    fn test_match_no_payload() {
        let input = r#"match status {
            .Ok { print("success") }
            .Error { print("failed") }
        }"#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let stmt = statement::parse_statement(&mut parser).unwrap();

        match stmt {
            Stmt::Match(match_stmt) => {
                assert_eq!(match_stmt.arms.len(), 2);
                // First arm should have no binding
                match &match_stmt.arms[0].pattern {
                    Pattern::EnumVariant {
                        name,
                        payload_binding,
                    } => {
                        assert_eq!(name, "Ok");
                        assert_eq!(payload_binding, &None);
                    }
                    _ => panic!("Expected enum variant pattern"),
                }
            }
            _ => panic!("Expected match statement"),
        }
    }
}
