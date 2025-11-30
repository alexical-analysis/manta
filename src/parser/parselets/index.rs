use super::Precedence;
use crate::ast::{Expr, IndexExpr};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::InfixExprParselet;
use crate::parser::{ParseError, Parser};

/// Parses index access expressions.
///
/// Example: `array[0]`
pub struct IndexParselet;

impl InfixExprParselet for IndexParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> Result<Expr, ParseError> {
        let index_expr = parser.parse_expression()?;

        // TODO: support slices like arr[1:3] or arr[:3] etc.
        let matched = parser.match_token(TokenKind::CloseSquare)?;
        if !matched {
            return Err(ParseError::MissingExpression(
                "missing index expression".to_string(),
            ));
        }

        Ok(Expr::Index(IndexExpr {
            target: Box::new(left),
            index: Box::new(index_expr),
        }))
    }

    fn precedence(&self) -> Precedence {
        Precedence::Call
    }
}
