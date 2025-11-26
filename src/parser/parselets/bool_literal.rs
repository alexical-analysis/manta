use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixParselet;
use crate::parser::{ParseError, Parser};

/// Parses boolean literal expressions.
///
/// Example: `true`, `false`
pub struct BoolLiteralParselet;

impl PrefixParselet for BoolLiteralParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        let value = match token.kind {
            crate::parser::lexer::TokenKind::TrueLiteral => true,
            crate::parser::lexer::TokenKind::FalseLiteral => false,
            _ => return Err(ParseError::Custom("Invalid boolean token".to_string())),
        };

        Ok(Expr::BoolLiteral(value))
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
