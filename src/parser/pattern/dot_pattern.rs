use crate::ast::{DotAccessPat, IdentifierPat, Pattern};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::{InfixPatternParselet, PatternParser, PrefixPatternParselet};

/// Parses dot patterns where the dot is the prefix.
///
/// Example: `.Ok`
/// Example: `.Err`
pub struct PrefixDotPatternParselet;

impl PrefixPatternParselet for PrefixDotPatternParselet {
    fn parse(&self, lexer: &mut Lexer, _token: Token) -> Result<Pattern, ParseError> {
        let field_token = lexer.next_token();
        match field_token.kind {
            TokenKind::Identifier => Ok(Pattern::DotAccess(DotAccessPat {
                target: None,
                field: IdentifierPat {
                    name: lexer.lexeme(field_token),
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
        _parser: &PatternParser,
        lexer: &mut Lexer,
        left: Pattern,
        _token: Token,
    ) -> Result<Pattern, ParseError> {
        let field_token = lexer.next_token();
        match field_token.kind {
            TokenKind::Identifier => Ok(Pattern::DotAccess(DotAccessPat {
                target: Some(Box::new(left)),
                field: IdentifierPat {
                    name: lexer.lexeme(field_token),
                },
            })),
            _ => Err(ParseError::UnexpectedToken(
                field_token,
                "field name required after '.'".to_string(),
            )),
        }
    }
}
