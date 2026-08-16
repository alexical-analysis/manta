use crate::ast::Expr;
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, Precedence, PrefixExprParselet};
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::str_store::StrStore;

/// Parses grouped expressions enclosed in parentheses.
///
/// Example: `(a + b)`
pub struct GroupParselet;

impl PrefixExprParselet for GroupParselet {
    fn parse(
        &self,
        parser: &ExprParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Expr, ParseError> {
        // Parse the inner expression
        let expr = match parser.parse(lexer, Precedence::Base) {
            Ok(expr) => expr,
            Err(_) => {
                return Err(ParseError::MissingExpression(
                    token,
                    "missing expression inside parentheses".to_string(),
                ));
            }
        };

        todo!("need to figure out how to get the actual str_store");
        let mut str_store = StrStore::new();

        // Expect a closing ')'
        let close = lexer.next(&mut str_store);
        if close.ty != Ty::CloseParen {
            return Err(ParseError::UnexpectedToken(
                close,
                "expected ')'".to_string(),
            ));
        }

        Ok(expr)
    }
}
