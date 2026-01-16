use crate::ast::Pattern;
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixPatternParselet;
use crate::parser::{ParseError, Parser};

/// Parses literal value patterns
///
/// Example: `true`, `false`
/// Example: `10`
/// Example: `"Hello World"`
/// Example: `3.14`
pub struct LiteralPatternParselet;

impl PrefixPatternParselet for LiteralPatternParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Pattern, ParseError> {
        match token.kind {
            TokenKind::TrueLiteral => Ok(Pattern::BoolLiteral(true)),
            TokenKind::FalseLiteral => Ok(Pattern::BoolLiteral(false)),
            TokenKind::Int => match token.lexeme.replace("_", "").parse() {
                Ok(n) => Ok(Pattern::IntLiteral(n)),
                Err(e) => Err(ParseError::Custom(format!(
                    "Invalid integer pattern {:?}",
                    e
                ))),
            },
            TokenKind::Float => match token.lexeme.replace("_", "").parse() {
                Ok(f) => Ok(Pattern::FloatLiteral(f)),
                Err(e) => Err(ParseError::Custom(format!("Invalid float pattern {:?}", e))),
            },
            TokenKind::Str => Ok(Pattern::StringLiteral(token.lexeme)),
            _ => Err(ParseError::Custom("Invalid bool token".to_string())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    crate::test_parselet!(
        LiteralPatternParselet,
        test_parse_true {
            input: "true",
            want: Pattern::BoolLiteral(true),
            want_value: (),
        },
        test_parse_false {
            input: "false",
            want: Pattern::BoolLiteral(false),
            want_value: (),
        },
        test_parse_int {
            input: "10",
            want: Pattern::IntLiteral(10),
            want_value: (),
        },
        test_parse_float {
            input: "3.45",
            want: Pattern::FloatLiteral(3.45),
            want_value: (),
        },
        test_parse_string {
            input: r###""Hello World""###,
            want: Pattern::StringLiteral(str),
            want_value: assert_eq!(str, "Hello World".to_string()),
        },
    );
}
