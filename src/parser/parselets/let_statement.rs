use crate::ast::{BlockStmt, CallExpr, Expr, ExprStmt, IdentifierExpr, LetStmt, ReturnStmt, Stmt};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixStmtParselet;
use crate::parser::{ParseError, Parser, Precedence, expression, statement};

/// Parses try expressions
///
/// Example: `let .Ok = do() or { return .Err }`
/// Example: `let .Ok(buf) = read_file(file) or(e) { print(e); return }`
/// Example: `let Ret.Ok = send_data(data) !`
pub struct LetParselet;

impl PrefixStmtParselet for LetParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Stmt, ParseError> {
        let pattern = statement::parse_pattern(parser)?;

        let matched = parser.match_token(TokenKind::Equal)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(format!(
                "expected ':=' but got {:?}",
                parser.lookahead(0)?,
            )));
        }

        let value = parser.parse_expression()?;

        let next = parser.lookahead(0)?;
        match next.kind {
            TokenKind::OrKeyword => {
                parser.consume()?;
                let next = parser.lookahead(0)?;
                let or_binding = if next.kind == TokenKind::OpenParen {
                    parser.consume()?;
                    let catch_ident = parser.consume()?;
                    if catch_ident.kind != TokenKind::Identifier {
                        return Err(ParseError::UnexpectedToken(format!(
                            "catch binding must be an identifier, but got {:?}",
                            catch_ident,
                        )));
                    }
                    parser.match_token(TokenKind::CloseParen)?;

                    Some(Box::new(IdentifierExpr {
                        name: catch_ident.lexeme,
                    }))
                } else {
                    None
                };

                let matches = parser.match_token(TokenKind::OpenBrace)?;
                if !matches {
                    return Err(ParseError::UnexpectedToken(
                        "or should be followed by a block".to_string(),
                    ));
                }

                let except = Some(statement::parse_block(parser)?);

                Ok(Stmt::Let(LetStmt {
                    pattern,
                    value,
                    or_binding,
                    except,
                }))
            }
            TokenKind::Bang => {
                parser.consume()?;

                Ok(Stmt::Let(LetStmt {
                    pattern,
                    value,
                    or_binding: Some(Box::new(IdentifierExpr {
                        name: "e".to_string(),
                    })),
                    except: Some(BlockStmt {
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "panic".to_string(),
                                })),
                                args: vec![Expr::Identifier(IdentifierExpr {
                                    name: "e".to_string(),
                                })],
                            }),
                        })],
                    }),
                }))
            }
            TokenKind::WrapKeyword => {
                parser.consume()?;

                let expr = expression::parse_expression(parser, Precedence::Base)?;

                Ok(Stmt::Let(LetStmt {
                    pattern,
                    value,
                    or_binding: Some(Box::new(IdentifierExpr {
                        name: "e".to_string(),
                    })),
                    except: Some(BlockStmt {
                        statements: vec![Stmt::Return(ReturnStmt {
                            value: Some(Expr::Call(CallExpr {
                                func: Box::new(expr),
                                args: vec![Expr::Identifier(IdentifierExpr {
                                    name: "e".to_string(),
                                })],
                            })),
                        })],
                    }),
                }))
            }
            TokenKind::Semicolon => Ok(Stmt::Let(LetStmt {
                pattern,
                value,
                or_binding: None,
                except: None,
            })),
            _ => Err(ParseError::UnexpectedToken(format!(
                "invalid token after try/catch {:?}",
                next,
            ))),
        }
    }
}
