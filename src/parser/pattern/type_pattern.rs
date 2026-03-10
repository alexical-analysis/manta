use crate::ast::{IdentifierExpr, Pattern, TypeSpecPat};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::PrefixPatternParselet;
use crate::parser::types;

/// Parses type patterns
///
/// Example: `i32`, `*bool`, `[]Vec3`
pub struct TypePatternParselet;

impl PrefixPatternParselet for TypePatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        let type_spec = types::parse_type(lexer, token)?;

        let mut payload = None;
        if lexer.peek().kind == TokenKind::OpenParen {
            lexer.next_token();
            let payload_token = lexer.next_token();
            if payload_token.kind != TokenKind::Identifier {
                return Err(ParseError::InvalidExpression(
                    payload_token,
                    "invalid payload for enum constructor".to_string(),
                ));
            }

            let close = lexer.next_token();
            if close.kind != TokenKind::CloseParen {
                return Err(ParseError::InvalidExpression(
                    payload_token,
                    "missing closing paran for pattern payload".to_string(),
                ));
            }

            payload = Some(payload_token.lexeme_id)
        }

        let id = token.source_id;
        Ok(Pattern::TypeSpec(TypeSpecPat {
            id,
            type_spec,
            payload,
        }))
    }
}
