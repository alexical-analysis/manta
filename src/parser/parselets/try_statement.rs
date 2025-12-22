use crate::ast::{IdentifierExpr, Stmt, TryStmt};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixStmtParselet;
use crate::parser::{ParseError, Parser, statement};

/// Parses try expressions
///
/// Example: `try .Ok := do() catch { return .Err }`
/// Example: `try .Ok(buf) := read_file(file) catch(e) { print(e); return }`
/// Example: `try Ret.Ok := send_data(data) !`
pub struct TryParselet;

impl PrefixStmtParselet for TryParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Stmt, ParseError> {
        let mut enum_name = None;

        let enum_name_token = parser.consume()?;
        if let TokenKind::Identifier = enum_name_token.kind {
            enum_name = Some(Box::new(IdentifierExpr {
                name: enum_name_token.lexeme.clone(),
            }));
        };

        parser.match_token(TokenKind::Dot)?;

        let variant_name;
        let variant_name_token = parser.consume()?;
        if let TokenKind::Identifier = variant_name_token.kind {
            variant_name = Box::new(IdentifierExpr {
                name: variant_name_token.lexeme.clone(),
            });
        } else {
            return Err(ParseError::UnexpectedToken(
                "Variant name must be an identifier".to_string(),
            ));
        }

        let next = parser.lookahead(0)?;
        let decl = if next.kind == TokenKind::OpenParen {
            parser.consume()?;
            let ident_name = parser.consume()?;
            let matched = parser.match_token(TokenKind::CloseParen)?;
            if !matched {
                return Err(ParseError::UnexpectedToken(
                    "Decl paren must be closed".to_string(),
                ));
            }

            Some(Box::new(IdentifierExpr {
                name: ident_name.lexeme,
            }))
        } else {
            None
        };

        let matched = parser.match_token(TokenKind::ColonEqual)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(format!(
                "expected ':=' but got {:?}",
                parser.lookahead(0)?,
            )));
        }

        let expr = parser.parse_expression()?;

        let next = parser.consume()?;
        match next.kind {
            TokenKind::CatchKeyword => {
                let next = parser.lookahead(0)?;
                let catch_binding = if next.kind == TokenKind::OpenParen {
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

                let next = parser.lookahead(0)?;
                let catch_body = if next.kind == TokenKind::OpenBrace {
                    parser.consume()?;
                    Some(Box::new(statement::parse_block(parser)?))
                } else {
                    None
                };

                Ok(Stmt::Try(TryStmt {
                    pattern_enum: enum_name,
                    pattern_variant: variant_name,
                    decl: decl,
                    expr: Box::new(expr),
                    catch_binding: catch_binding,
                    catch_body,
                }))
            }
            TokenKind::Bang => Ok(Stmt::Try(TryStmt {
                pattern_enum: enum_name,
                pattern_variant: variant_name,
                decl: decl,
                expr: Box::new(expr),
                catch_binding: None,
                catch_body: None,
            })),
            _ => Err(ParseError::UnexpectedToken(format!(
                "invalid token after try/catch {:?}",
                next,
            ))),
        }
    }
}
