use crate::ast::Stmt;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixStmtParselet;
use crate::parser::statement;
use crate::parser::{ParseError, Parser};

/// Parses a block statement.
///
/// Example `{ print("ok"); free(ptr) }`
pub struct BlockParselet;

impl PrefixStmtParselet for BlockParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Stmt, ParseError> {
        let block = statement::parse_block(parser)?;

        Ok(Stmt::Block(block))
    }

    fn matches(&self, _parser: &mut Parser) -> bool {
        true
    }
}
