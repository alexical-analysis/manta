use crate::ast::Expr;
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, Precedence, PrefixExprParselet};
use crate::parser::lexer::{Lexer, Token, TokenKind};

/// Parses grouped expressions enclosed in parentheses.
///
/// Example: `(a + b)`
pub struct GroupParselet;

impl PrefixExprParselet for GroupParselet {
    fn parse(
        &self,
        parser: &ExprParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Expr, ParseError> {
        // Parse the inner expression
        let expr = match parser.parse(lexer, Precedence::Base) {
            Ok(expr) => expr,
            Err(_) => {
                return Err(ParseError::MissingExpression(
                    "missing expression inside parentheses".to_string(),
                ));
            }
        };

        // Expect a closing ')'
        let close = lexer.next_token();
        if close.kind != TokenKind::CloseParen {
            return Err(ParseError::UnexpectedToken(
                close,
                "expected ')'".to_string(),
            ));
        }

        Ok(expr)
    }
}

// TODO: add test cases to make sure the group parselet is working
