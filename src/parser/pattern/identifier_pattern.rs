use crate::ast::{IdentifierPat, Pattern};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token};
use crate::parser::pattern::PrefixPatternParselet;

/// Parses identifier patterns.
///
/// Example: `foo`, `myVariable`, `count`
pub struct IdentifierPatternParselet;

impl PrefixPatternParselet for IdentifierPatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        let name = token.lexeme_id;
        match lexer.lexeme(name).as_str() {
            "_" => Ok(Pattern::Default),
            _ => Ok(Pattern::Identifier(IdentifierPat {
                id: token.source_id,
                name,
            })),
        }
    }
}
