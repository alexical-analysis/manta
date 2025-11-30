use crate::ast::{Expr, UnaryExpr, UnaryOp};
use crate::parser::expression;
use crate::parser::lexer::Token;
use crate::parser::parselets::{Precedence, PrefixExprParselet};
use crate::parser::{ParseError, Parser};

/// Parses unary negation expressions.
///
/// Example: `-42`
pub struct UnaryOperatorParselet {
    pub operator: UnaryOp,
}

impl PrefixExprParselet for UnaryOperatorParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Expr, ParseError> {
        let right = expression::parse_expression(parser, Precedence::Prefix)?;
        Ok(Expr::UnaryExpr(UnaryExpr {
            operator: self.operator,
            operand: Box::new(right),
        }))
    }
}
