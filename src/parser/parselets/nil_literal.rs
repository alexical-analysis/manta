use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixParselet;
use crate::parser::{ParseError, Parser};

/// Parses nil literal expressions.
///
/// Example: `nil`
pub struct NilLiteralParselet;

impl PrefixParselet for NilLiteralParselet {
    fn parse(&self, _parser: &mut Parser, _token: Token) -> Result<Expr, ParseError> {
        Ok(Expr::NilLiteral)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    crate::test_parselet!(
        NilLiteralParselet,
        test_parse_nil {
            input: "nil",
            want: Expr::NilLiteral,
            want_value: (),
        },
    );
}
