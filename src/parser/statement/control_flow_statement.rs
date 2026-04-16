use crate::ast::Stmt;
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};

/// Parse break statements.
///
/// Example `break`
pub struct BreakParselet;

impl PrefixStmtParselet for BreakParselet {
    fn parse(
        &self,
        _parser: &StmtParser,
        _lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Stmt, ParseError> {
        Ok(Stmt::Break)
    }
}
