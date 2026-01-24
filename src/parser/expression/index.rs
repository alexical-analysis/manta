use super::Precedence;
use crate::ast::{Expr, IndexExpr, RangeExpr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, InfixExprParselet};
use crate::parser::lexer::{Lexer, Token, TokenKind};

/// Parses index access expressions.
///
/// Example: `array[0]`
pub struct IndexParselet;

impl InfixExprParselet for IndexParselet {
    fn parse(
        &self,
        parser: &ExprParser,
        lexer: &mut Lexer,
        left: Expr,
        _token: Token,
    ) -> Result<Expr, ParseError> {
        let index_expr = parser.parse(lexer, Precedence::Base)?;

        let next = lexer.peek();
        let end_expr = if next.kind == TokenKind::Colon {
            lexer.next_token();
            Some(parser.parse(lexer, Precedence::Base)?)
        } else {
            None
        };

        let next = lexer.next_token();
        if next.kind != TokenKind::CloseSquare {
            return Err(ParseError::MissingExpression(
                "missing index expression".to_string(),
            ));
        }

        if let Some(end_expr) = end_expr {
            Ok(Expr::Index(IndexExpr {
                target: Box::new(left),
                index: Box::new(Expr::Range(RangeExpr {
                    start: Box::new(index_expr),
                    end: Box::new(end_expr),
                })),
            }))
        } else {
            Ok(Expr::Index(IndexExpr {
                target: Box::new(left),
                index: Box::new(index_expr),
            }))
        }
    }

    fn precedence(&self) -> Precedence {
        Precedence::Call
    }
}
