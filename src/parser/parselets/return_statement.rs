use crate::ast::{ReturnStmt, Stmt};
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixStmtParselet;
use crate::parser::{ParseError, Parser};

/// Parses let statements.
///
/// Example: `return 10`
pub struct ReturnParselet;

impl PrefixStmtParselet for ReturnParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Stmt, ParseError> {
        let next = parser.lookahead(0)?;
        let token_kind = next.kind;

        let value = if parser.is_expression_prefix(&token_kind) {
            Some(parser.parse_expression()?)
        } else {
            None
        };

        Ok(Stmt::Return(ReturnStmt { value }))
    }
}
