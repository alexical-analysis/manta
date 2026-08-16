use crate::ast::{Pattern, Payload, TypeSpecPat};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::parser::pattern::PrefixPatternParselet;
use crate::parser::types;
use crate::str_store::{self, StrStore};

/// Parses type patterns
///
/// Example: `i32(_)`, `*bool(_)`, `[]Vec3(_)`
pub struct TypePatternParselet;

impl PrefixPatternParselet for TypePatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        let type_spec = types::parse_type(lexer, token)?;

        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        // type patterns MUST have a payload
        let open = lexer.next(&mut str_store);
        if open.ty != Ty::OpenParen {
            return Err(ParseError::Custom(
                open,
                "missing payload for type pattern match".to_string(),
            ));
        }
        let payload_token = lexer.next(&mut str_store);
        if payload_token.ty != Ty::Identifier {
            return Err(ParseError::Custom(
                open,
                "missing payload for type pattern match".to_string(),
            ));
        }

        let close = lexer.next(&mut str_store);
        if close.ty != Ty::CloseParen {
            return Err(ParseError::InvalidExpression(
                payload_token,
                "missing closing paran for pattern payload".to_string(),
            ));
        }

        let payload = match payload_token.lexeme {
            str_store::UNDERSCORE => Payload::Default,
            id => Payload::Some(id),
        };

        Ok(Pattern::TypeSpec(TypeSpecPat {
            id: token.pos,
            type_spec,
            payload,
        }))
    }
}
