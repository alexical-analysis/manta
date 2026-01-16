use crate::ast::{IdentifierPat, Pattern};
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixPatternParselet;
use crate::parser::{ParseError, Parser};

/// Parses identifier patterns.
///
/// Example: `foo`, `myVariable`, `count`
pub struct IdentifierPatternParselet;

impl PrefixPatternParselet for IdentifierPatternParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> Result<Pattern, ParseError> {
        match token.lexeme.as_str() {
            "_" => Ok(Pattern::Default),
            _ => Ok(Pattern::Identifier(IdentifierPat { name: token.lexeme })),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    crate::test_parselet!(
        IdentifierPatternParselet,
        test_parse_simple_identifier {
            input: "foo",
            want: Pattern::Identifier(ident),
            want_value: assert_eq!(
                ident,
                IdentifierPat {
                    name: "foo".to_string()
                }
            ),
        },
        test_parse_variable_name {
            input: "my_variable",
            want: Pattern::Identifier(ident),
            want_value: assert_eq!(
                ident,
                IdentifierPat {
                    name: "my_variable".to_string()
                }
            ),
        },
        test_parse_identifier_with_numbers {
            input: "var123",
            want: Pattern::Identifier(ident),
            want_value: assert_eq!(
                ident,
                IdentifierPat {
                    name: "var123".to_string()
                }
            ),
        },
    );
}
