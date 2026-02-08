use crate::ast::{LetExcept, LetStmt, Stmt};
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
                let binding = if next.kind == TokenKind::OpenParen {
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

                    Some(catch_ident.lexeme_id)
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

                let body = parser.parse_block(lexer)?;
                let except = LetExcept::Or { binding, body };

                Ok(Stmt::Let(LetStmt {
                    pattern,
                    value,
                    except,
                }))
            }
            TokenKind::WrapKeyword => {
                lexer.next_token();

                let expr = parser.parse_expression(lexer)?;

                Ok(Stmt::Let(LetStmt {
                    pattern,
                    value,
                    except: LetExcept::Wrap(expr),
                }))
            }
            TokenKind::Bang => {
                lexer.next_token();

                Ok(Stmt::Let(LetStmt {
                    pattern,
                    value,
                    except: LetExcept::Panic,
                }))
            }
            TokenKind::Semicolon => Ok(Stmt::Let(LetStmt {
                pattern,
                value,
                except: LetExcept::None,
            })),
            _ => Err(ParseError::UnexpectedToken(
                next,
                "invalid token after let expr".to_string(),
            )),
        }
    }
}
