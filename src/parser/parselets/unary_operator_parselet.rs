use crate::ast::{Expr, UnaryExpr, UnaryOp};
use crate::parser::lexer::Token;
use crate::parser::parselets::{Precedence, PrefixParselet};
use crate::parser::{ParseError, Parser};

/// Parses unary negation expressions.
///
/// Example: `-42`
pub struct UnaryOperatorParselet {
    pub operator: UnaryOp,
}

impl PrefixParselet for UnaryOperatorParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Expr, ParseError> {
        let right = parser.parse_expression_precedence(Precedence::Prefix)?;
        Ok(Expr::UnaryExpr(UnaryExpr {
            operator: self.operator,
            operand: Box::new(right),
        }))
    }
}
