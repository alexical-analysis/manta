use crate::ast::{Expr, UnaryExpr, UnaryOp};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, Precedence, PrefixExprParselet};
use crate::parser::lexer::{Lexer, Token};

/// Parses unary negation expressions.
///
/// Example: `-42`
pub struct UnaryOperatorParselet {
    pub operator: UnaryOp,
}

impl PrefixExprParselet for UnaryOperatorParselet {
    fn parse(
        &self,
        parser: &ExprParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Expr, ParseError> {
        let right = parser.parse(lexer, Precedence::Prefix)?;
        Ok(Expr::Unary(UnaryExpr {
            operator: self.operator,
            operand: Box::new(right),
        }))
    }
}
