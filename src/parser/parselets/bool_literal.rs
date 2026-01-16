use crate::ast::Expr;
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixExprParselet;
use crate::parser::{ParseError, Parser};

/// Parses boolean literal expressions.
///
/// Example: `true`, `false`
pub struct BoolLiteralParselet;

impl PrefixExprParselet for BoolLiteralParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        match token.kind {
            TokenKind::TrueLiteral => Ok(Expr::BoolLiteral(true)),
            TokenKind::FalseLiteral => Ok(Expr::BoolLiteral(false)),
            _ => Err(ParseError::Custom("Invalid boolean token".to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    crate::test_parselet!(
        BoolLiteralParselet,
        test_parse_true {
            input: "true",
            want: Expr::BoolLiteral(true),
            want_value: (),
        },
        test_parse_false {
            input: "false",
            want: Expr::BoolLiteral(false),
            want_value: (),
        },
    );
}
