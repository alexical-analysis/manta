use crate::ast::{Expr, RangeExpr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, InfixExprParselet, Precedence};
use crate::parser::lexer::{Lexer, Token};

pub struct RangeOperatorParselet {
    pub inclusive: bool,
}

impl InfixExprParselet for RangeOperatorParselet {
    fn parse(
        &self,
        parser: &ExprParser,
        lexer: &mut Lexer,
        left: Expr,
        _token: Token,
    ) -> Result<Expr, ParseError> {
        let end = parser.parse(lexer, Precedence::Range)?;

        Ok(Expr::Range(RangeExpr {
            start: Box::new(left),
            inclusive: self.inclusive,
            end: Box::new(end),
        }))
    }

    fn precedence(&self) -> Precedence {
        Precedence::Range
    }
}
