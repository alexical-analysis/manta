use crate::ast::{DeferStmt, Stmt};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixStmtParselet;
use crate::parser::statement;
use crate::parser::{ParseError, Parser};

/// Parses defer statements.
///
/// Example: `defer { free(ptr) }`
pub struct DeferParselet;

impl PrefixStmtParselet for DeferParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> Result<Stmt, ParseError> {
        println!("token {:?} {:?}", token, parser.lookahead(0));
        let matched = parser.match_token(TokenKind::OpenBrace)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                "block must start with '{'".to_string(),
            ));
        }

        let block = statement::parse_block(parser)?;

        Ok(Stmt::Defer(DeferStmt { block }))
    }

    fn matches(&self, _parser: &mut Parser) -> bool {
        true
    }
}
