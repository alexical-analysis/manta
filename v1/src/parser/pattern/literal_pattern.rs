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
        let lexeme = lexer.lexeme(token.lexeme_id);
        match token.kind {
            TokenKind::TrueLiteral => Ok(Pattern::BoolLiteral(true)),
            TokenKind::FalseLiteral => Ok(Pattern::BoolLiteral(false)),
            TokenKind::Int => match lexeme.replace("_", "").parse::<i64>() {
                Ok(n) => Ok(Pattern::IntLiteral(n)),
                Err(_) => match lexeme.replace("_", "").parse::<u64>() {
                    Ok(n) => Ok(Pattern::UIntLiteral(n)),
                    Err(e) => Err(ParseError::Custom(
                        token,
                        format!("Invalid integer pattern {:?}", e),
                    )),
                },
            },
            TokenKind::Float => match lexeme.replace("_", "").parse() {
                Ok(f) => Ok(Pattern::FloatLiteral(f)),
                Err(e) => Err(ParseError::Custom(
                    token,
                    format!("Invalid float pattern {:?}", e),
                )),
            },
            TokenKind::Str => Ok(Pattern::StringLiteral(token.lexeme_id)),
            _ => Err(ParseError::Custom(token, "Invalid bool token".to_string())),
        }
    }
}
