use crate::ast::Pattern;
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::parser::pattern::PrefixPatternParselet;
use crate::str_store::StrStore;

/// Parses literal value patterns
///
/// Example: `true`, `false`
/// Example: `10`
/// Example: `"Hello World"`
/// Example: `3.14`
pub struct LiteralPatternParselet;

impl PrefixPatternParselet for LiteralPatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let lexeme = str_store
            .get_string(token.lexeme)
            .expect("need to figure this out");
        match token.ty {
            Ty::TrueLiteral => Ok(Pattern::BoolLiteral(true)),
            Ty::FalseLiteral => Ok(Pattern::BoolLiteral(false)),
            Ty::Int => match lexeme.replace("_", "").parse::<i64>() {
                Ok(n) => Ok(Pattern::IntLiteral(n)),
                Err(_) => match lexeme.replace("_", "").parse::<u64>() {
                    Ok(n) => Ok(Pattern::UIntLiteral(n)),
                    Err(e) => Err(ParseError::Custom(
                        token,
                        format!("Invalid integer pattern {:?}", e),
                    )),
                },
            },
            Ty::Float => match lexeme.replace("_", "").parse() {
                Ok(f) => Ok(Pattern::FloatLiteral(f)),
                Err(e) => Err(ParseError::Custom(
                    token,
                    format!("Invalid float pattern {:?}", e),
                )),
            },
            Ty::Str => Ok(Pattern::StringLiteral(token.lexeme)),
            _ => Err(ParseError::Custom(token, "Invalid bool token".to_string())),
        }
    }
}
