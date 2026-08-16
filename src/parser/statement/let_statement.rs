use crate::ast::{LetExcept, LetStmt, Stmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};
use crate::str_store::StrStore;

/// Parses let expressions
///
/// Example: `let .Ok = do() or { return .Err }`
/// Example: `mut .Ok(buf) = read_file(file) or(e) { print(e); return }`
/// Example: `let Ret.Ok = send_data(data) !`
pub struct LetParselet {
    pub mutable_binding: bool,
}

impl PrefixStmtParselet for LetParselet {
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Stmt, ParseError> {
        let pattern = parser.parse_pattern(lexer)?;

        todo!("need to figure this out, how to get the str_store");
        let mut str_store = StrStore::new();

        let token = lexer.next(&mut str_store);
        if token.ty != Ty::Equal {
            return Err(ParseError::UnexpectedToken(
                token,
                "expected '='".to_string(),
            ));
        }

        let value = parser.parse_expression(lexer)?;

        let next = lexer.peek();
        match next.ty {
            Ty::OrKeyword => {
                let or_token = lexer.next(&mut str_store);
                let next = lexer.peek();
                let binding = if next.ty == Ty::OpenParen {
                    lexer.next(&mut str_store);
                    let catch_ident = lexer.next(&mut str_store);
                    if catch_ident.ty != Ty::Identifier {
                        return Err(ParseError::UnexpectedToken(
                            catch_ident,
                            "catch binding must be an identifier".to_string(),
                        ));
                    }

                    let token = lexer.next(&mut str_store);
                    if token.ty != Ty::CloseParen {
                        return Err(ParseError::UnexpectedToken(
                            token,
                            "missing closing paren".to_string(),
                        ));
                    }

                    Some(catch_ident.lexeme)
                } else {
                    None
                };

                let token = lexer.next(&mut str_store);
                if token.ty != Ty::OpenBrace {
                    return Err(ParseError::UnexpectedToken(
                        token,
                        "'or' should be followed by a block".to_string(),
                    ));
                }

                let body = parser.parse_block(lexer, token)?;
                let except = LetExcept::Or {
                    id: or_token.pos,
                    binding,
                    body,
                };

                Ok(Stmt::Let(LetStmt {
                    mutable: self.mutable_binding,
                    pattern,
                    value,
                    except,
                }))
            }
            Ty::WrapKeyword => {
                lexer.next(&mut str_store);

                let expr = parser.parse_expression(lexer)?;

                Ok(Stmt::Let(LetStmt {
                    mutable: self.mutable_binding,
                    pattern,
                    value,
                    except: LetExcept::Wrap(expr),
                }))
            }
            Ty::Bang => {
                lexer.next(&mut str_store);

                Ok(Stmt::Let(LetStmt {
                    mutable: self.mutable_binding,
                    pattern,
                    value,
                    except: LetExcept::Panic,
                }))
            }
            Ty::Semicolon => Ok(Stmt::Let(LetStmt {
                mutable: self.mutable_binding,
                pattern,
                value,
                except: LetExcept::None,
            })),
            _ => Err(ParseError::UnexpectedToken(
                next.clone(),
                "invalid token after let expr".to_string(),
            )),
        }
    }
}
