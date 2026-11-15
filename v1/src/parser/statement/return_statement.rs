use crate::ast::{ReturnStmt, Stmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};

/// Parses let statements.
///
/// Example: `return 10`
pub struct ReturnParselet;

impl PrefixStmtParselet for ReturnParselet {
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Stmt, ParseError> {
        let token = lexer.peek();
        let value = if parser.is_expression_prefix(token) {
            Some(parser.parse_expression(lexer)?)
        } else {
            None
        };

        Ok(Stmt::Return(ReturnStmt { value }))
    }
}
