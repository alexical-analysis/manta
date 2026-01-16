use crate::ast::{IfStmt, Stmt};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixStmtParselet;
use crate::parser::{ParseError, Parser, statement};

/// Parses if statements
///
/// Example: `if 10 < 20 { print("ok") }`
pub struct IfParselet;

impl PrefixStmtParselet for IfParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Stmt, ParseError> {
        let check = parser.parse_expression()?;
        let check = Box::new(check);

        let matched = parser.match_token(TokenKind::OpenBrace)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                parser.lookahead(0)?.clone(),
                "Expected '{' after if check".to_string(),
            ));
        }

        let success = statement::parse_block(parser)?;

        let mut fail = None;
        let matches = parser.match_token(TokenKind::ElseKeyword)?;
        if matches {
            let matched = parser.match_token(TokenKind::OpenBrace)?;
            if !matched {
                return Err(ParseError::UnexpectedToken(
                    parser.lookahead(0)?.clone(),
                    "Expected '{' after else keyword".to_string(),
                ));
            }
            fail = Some(statement::parse_block(parser)?);
        }

        Ok(Stmt::If(IfStmt {
            check,
            success,
            fail,
        }))
    }
}
