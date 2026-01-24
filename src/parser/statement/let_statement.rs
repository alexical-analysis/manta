use crate::ast::{BlockStmt, CallExpr, Expr, ExprStmt, IdentifierExpr, LetStmt, ReturnStmt, Stmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};

/// Parses let expressions
///
/// Example: `let .Ok = do() or { return .Err }`
/// Example: `let .Ok(buf) = read_file(file) or(e) { print(e); return }`
/// Example: `let Ret.Ok = send_data(data) !`
pub struct LetParselet;

impl PrefixStmtParselet for LetParselet {
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Stmt, ParseError> {
        let pattern = parser.parse_pattern(lexer)?;

        let token = lexer.next_token();
        if token.kind != TokenKind::Equal {
            return Err(ParseError::UnexpectedToken(
                token,
                "expected '='".to_string(),
            ));
        }

        let value = parser.parse_expression(lexer)?;

        let next = lexer.peek();
        match next.kind {
            TokenKind::OrKeyword => {
                lexer.next_token();
                let next = lexer.peek();
                let or_binding = if next.kind == TokenKind::OpenParen {
                    lexer.next_token();
                    let catch_ident = lexer.next_token();
                    if catch_ident.kind != TokenKind::Identifier {
                        return Err(ParseError::UnexpectedToken(
                            catch_ident,
                            "catch binding must be an identifier".to_string(),
                        ));
                    }

                    let token = lexer.next_token();
                    if token.kind != TokenKind::CloseParen {
                        return Err(ParseError::UnexpectedToken(
                            token,
                            "missing closing paren".to_string(),
                        ));
                    }

                    Some(IdentifierExpr {
                        name: lexer.lexeme(catch_ident),
                    })
                } else {
                    None
                };

                let token = lexer.next_token();
                if token.kind != TokenKind::OpenBrace {
                    return Err(ParseError::UnexpectedToken(
                        token,
                        "'or' should be followed by a block".to_string(),
                    ));
                }

                let except = Some(parser.parse_block(lexer)?);

                Ok(Stmt::Let(LetStmt {
                    pattern,
                    value,
                    or_binding,
                    except,
                }))
            }
            TokenKind::Bang => {
                lexer.next_token();

                Ok(Stmt::Let(LetStmt {
                    pattern,
                    value,
                    or_binding: Some(IdentifierExpr {
                        name: "e".to_string(),
                    }),
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
                lexer.next_token();

                let expr = parser.parse_expression(lexer)?;

                Ok(Stmt::Let(LetStmt {
                    pattern,
                    value,
                    or_binding: Some(IdentifierExpr {
                        name: "e".to_string(),
                    }),
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
            _ => Err(ParseError::UnexpectedToken(
                next.clone(),
                format!("invalid token after try/catch {:?}", next,),
            )),
        }
    }
}
