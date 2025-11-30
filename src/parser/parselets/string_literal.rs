use crate::ast::Expr;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixExprParselet;
use crate::parser::{ParseError, Parser};

/// Parses string literal expressions.
///
/// Example: `"hello world"`
pub struct StringLiteralParselet;

impl PrefixExprParselet for StringLiteralParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Expr, ParseError> {
        Ok(Expr::StringLiteral(token.lexeme))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    crate::test_parselet!(
        StringLiteralParselet,
        test_parse_hello {
            input: "\"hello\"",
            want: Expr::StringLiteral(s),
            want_value: assert_eq!(s, "hello"),
        },
        test_parser_empty {
            input: "\"\"",
            want: Expr::StringLiteral(s),
            want_value: assert_eq!(s, ""),
        },
        test_parse_hello_world {
            input: "\"hello world\"",
            want: Expr::StringLiteral(s),
            want_value: assert_eq!(s, "hello world"),
        },
    );
}
