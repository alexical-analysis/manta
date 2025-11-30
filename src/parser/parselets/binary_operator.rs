use crate::ast::{BinaryExpr, BinaryOp, Expr};
use crate::parser::expression;
use crate::parser::lexer::Token;
use crate::parser::parselets::{InfixExprParselet, Precedence};
use crate::parser::{ParseError, Parser};

/// Parses binary arithmetic operators: +, -, *, /
/// This is a single parselet that handles all binary arithmetic operations
/// with their appropriate precedence levels.
///
/// Examples:
/// - `1 + 2`
/// - `5 * 3 - 2`
/// - `10 / 2 + 3`
pub struct BinaryOperatorParselet {
    pub operator: BinaryOp,
    pub precedence: Precedence,
}

impl InfixExprParselet for BinaryOperatorParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> Result<Expr, ParseError> {
        let right = expression::parse_expression(parser, self.precedence)?;

        Ok(Expr::BinaryExpr(BinaryExpr {
            left: Box::new(left),
            operator: self.operator,
            right: Box::new(right),
        }))
    }

    fn precedence(&self) -> Precedence {
        self.precedence
    }
}
