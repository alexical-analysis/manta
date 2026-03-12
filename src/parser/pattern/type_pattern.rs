use crate::ast::{Pattern, Payload, TypeSpecPat};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::PrefixPatternParselet;
use crate::parser::types;
use crate::str_store;

/// Parses type patterns
///
/// Example: `i32(_)`, `*bool(_)`, `[]Vec3(_)`
pub struct TypePatternParselet;

impl PrefixPatternParselet for TypePatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        let type_spec = types::parse_type(lexer, token)?;

        // type patterns MUST have a payload
        let open = lexer.next_token();
        if open.kind != TokenKind::OpenParen {
            return Err(ParseError::Custom(
                open,
                "missing payload for type pattern match".to_string(),
            ));
        }
        let payload_token = lexer.next_token();
        if payload_token.kind != TokenKind::Identifier {
            return Err(ParseError::Custom(
                open,
                "missing payload for type pattern match".to_string(),
            ));
        }

        let close = lexer.next_token();
        if close.kind != TokenKind::CloseParen {
            return Err(ParseError::InvalidExpression(
                payload_token,
                "missing closing paran for pattern payload".to_string(),
            ));
        }

        let payload = match payload_token.lexeme_id {
            str_store::UNDERSCORE => Payload::Default,
            id => Payload::Some(id),
        };

        Ok(Pattern::TypeSpec(TypeSpecPat {
            id: token.source_id,
            type_spec,
            payload,
        }))
    }
}
