use crate::ast::{IdentifierPat, NamedType, Pattern, Payload, TypeSpec, TypeSpecPat};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::PrefixPatternParselet;
use crate::parser::types;
use crate::str_store;

/// Parses identifier patterns.
///
/// Example: `foo`, `myVariable`, `count`
pub struct IdentifierPatternParselet;

impl PrefixPatternParselet for IdentifierPatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        let mut payload = Payload::None;
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

            payload = Payload::Some(payload_token.lexeme_id)
        }

        let name = token.lexeme_id;
        match (name, &payload) {
            (str_store::UNDERSCORE, Payload::None) => Ok(Pattern::Default),
            (str_store::UNDERSCORE, Payload::Some(_) | Payload::Default) => {
                Err(ParseError::InvalidExpression(
                    token,
                    "default patterns can not have payloads".to_string(),
                ))
            }
            (_, Payload::None) => Ok(Pattern::Identifier(IdentifierPat {
                id: token.source_id,
                name,
            })),
            (_, Payload::Some(_) | Payload::Default) => {
                let type_spec = types::parse_type(lexer, token)?;
                Ok(Pattern::TypeSpec(TypeSpecPat {
                    id: token.source_id,
                    type_spec,
                    payload,
                }))
            }
        }
    }
}
