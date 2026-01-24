use crate::ast::Stmt;
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};

/// Parses a block statement.
///
/// Example `{ print("ok"); free(ptr) }`
pub struct BlockParselet;

impl PrefixStmtParselet for BlockParselet {
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Stmt, ParseError> {
        let block = parser.parse_block(lexer)?;

        Ok(Stmt::Block(block))
    }
}
