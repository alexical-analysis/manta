use crate::ast::{AssignStmt, Expr, Stmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token};
use crate::parser::statement::{InfixStmtParselet, StmtParser};

/// Parses assignment statements
///
/// Example: `x = 10`
pub struct AssignParselet;

impl InfixStmtParselet for AssignParselet {
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        left: Expr,
        _token: Token,
    ) -> Result<Stmt, ParseError> {
        let expr = parser.parse_expression(lexer)?;

        Ok(Stmt::Assign(AssignStmt {
            lvalue: left,
            rvalue: expr,
        }))
    }
}
