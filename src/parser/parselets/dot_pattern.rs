use crate::ast::{DotAccessPat, IdentifierPat, Pattern};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::{InfixPatternParselet, PrefixPatternParselet};
use crate::parser::{ParseError, Parser};

/// Parses dot patterns where the dot is the prefix.
///
/// Example: `.Ok`
/// Example: `.Err`
pub struct PrefixDotPatternParselet;

impl PrefixPatternParselet for PrefixDotPatternParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Pattern, ParseError> {
        let field_token = parser.consume()?;
        match field_token.kind {
            TokenKind::Identifier => Ok(Pattern::DotAccess(DotAccessPat {
                target: None,
                field: IdentifierPat {
                    name: field_token.lexeme,
                },
            })),
            _ => Err(ParseError::UnexpectedToken(
                field_token,
                "field name required after '.'".to_string(),
            )),
        }
    }
}

/// Parses dot patterns where the dot is the infix.
///
/// Example: `Ret.Ok`
/// Example: `Ret.Err`
pub struct InfixDotPatternParselet;

impl InfixPatternParselet for InfixDotPatternParselet {
    fn parse(
        &self,
        parser: &mut Parser,
        left: Pattern,
        _token: Token,
    ) -> Result<Pattern, ParseError> {
        let field_token = parser.consume()?;
        match field_token.kind {
            TokenKind::Identifier => Ok(Pattern::DotAccess(DotAccessPat {
                target: Some(Box::new(left)),
                field: IdentifierPat {
                    name: field_token.lexeme,
                },
            })),
            _ => Err(ParseError::UnexpectedToken(
                field_token,
                "field name required after '.'".to_string(),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::IdentifierPat;
    use crate::{test_infix_parselet, test_parselet};

    test_parselet!(
        PrefixDotPatternParselet,
        test_parse_dot_inferred_variant {
            input: ".Ok",
            want: Pattern::DotAccess(pat),
            want_value: assert_eq!(
                pat,
                DotAccessPat {
                    target: None,
                    field: IdentifierPat {
                        name: "Ok".to_string()
                    },
                }
            ),
        },
    );

    test_infix_parselet!(
        InfixDotPatternParselet,
        test_parse_dot_variant {
            input: ".Ok",
            left: Pattern::Identifier(IdentifierPat {
                name: "Ret".to_string(),
            }),
            want: Pattern::DotAccess(pat),
            want_value: assert_eq!(
                pat,
                DotAccessPat {
                    target: Some(Box::new(Pattern::Identifier(IdentifierPat {
                        name: "Ret".to_string()
                    }))),
                    field: IdentifierPat {
                        name: "Ok".to_string()
                    },
                },
            ),
        },
    );
}
