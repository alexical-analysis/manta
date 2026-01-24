use crate::ast::Pattern;
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::PrefixPatternParselet;

/// Parses literal value patterns
///
/// Example: `true`, `false`
/// Example: `10`
/// Example: `"Hello World"`
/// Example: `3.14`
pub struct LiteralPatternParselet;

impl PrefixPatternParselet for LiteralPatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        match token.kind {
            TokenKind::TrueLiteral => Ok(Pattern::BoolLiteral(true)),
            TokenKind::FalseLiteral => Ok(Pattern::BoolLiteral(false)),
            TokenKind::Int => match lexer.lexeme(token).replace("_", "").parse() {
                Ok(n) => Ok(Pattern::IntLiteral(n)),
                Err(e) => Err(ParseError::Custom(format!(
                    "Invalid integer pattern {:?}",
                    e
                ))),
            },
            TokenKind::Float => match lexer.lexeme(token).replace("_", "").parse() {
                Ok(f) => Ok(Pattern::FloatLiteral(f)),
                Err(e) => Err(ParseError::Custom(format!("Invalid float pattern {:?}", e))),
            },
            TokenKind::Str => Ok(Pattern::StringLiteral(lexer.lexeme(token))),
            _ => Err(ParseError::Custom("Invalid bool token".to_string())),
        }
    }
}
