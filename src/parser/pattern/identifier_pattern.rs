use crate::ast::{IdentifierPat, Pattern, Payload, TypeSpecPat};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::parser::pattern::PrefixPatternParselet;
use crate::parser::types;
use crate::str_store::{self, StrStore};

/// Parses identifier patterns.
///
/// Example: `foo`, `myVariable`, `count`
pub struct IdentifierPatternParselet;

impl PrefixPatternParselet for IdentifierPatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let mut payload = Payload::None;
        if lexer.peek().ty == Ty::OpenParen {
            lexer.next(&mut str_store);
            let payload_token = lexer.next(&mut str_store);
            if payload_token.ty != Ty::Identifier {
                return Err(ParseError::InvalidExpression(
                    payload_token,
                    "invalid payload for enum constructor".to_string(),
                ));
            }

            let close = lexer.next(&mut str_store);
            if close.ty != Ty::CloseParen {
                return Err(ParseError::InvalidExpression(
                    payload_token,
                    "missing closing paran for pattern payload".to_string(),
                ));
            }

            payload = Payload::Some(payload_token.lexeme)
        }

        let name = token.lexeme;
        match (name, &payload) {
            (str_store::UNDERSCORE, Payload::None) => Ok(Pattern::Default),
            (str_store::UNDERSCORE, Payload::Some(_) | Payload::Default) => {
                Err(ParseError::InvalidExpression(
                    token,
                    "default patterns can not have payloads".to_string(),
                ))
            }
            (_, Payload::None) => Ok(Pattern::Identifier(IdentifierPat {
                id: token.pos,
                name,
            })),
            (_, Payload::Some(_) | Payload::Default) => {
                let type_spec = types::parse_type(lexer, token)?;
                Ok(Pattern::TypeSpec(TypeSpecPat {
                    id: token.pos,
                    type_spec,
                    payload,
                }))
            }
        }
    }
}
