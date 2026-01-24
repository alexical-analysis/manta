use crate::ast::Pattern;
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token};
use crate::parser::pattern::PrefixPatternParselet;
use crate::parser::types;

/// Parses type patterns
///
/// Example: `i32`, `*bool`, `[]Vec3`
pub struct TypePatternParselet;

impl PrefixPatternParselet for TypePatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        let type_spec = types::parse_type(lexer, token)?;

        Ok(Pattern::TypeSpec(type_spec))
    }
}
