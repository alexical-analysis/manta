use crate::ast::{EnumVariantPat, IdentifierExpr, Pattern, Payload};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::parser::pattern::{InfixPatternParselet, PatternParser, PrefixPatternParselet};
use crate::str_store::{self, StrStore};

/// Parses dot patterns where the dot is the prefix.
///
/// Example: `.Ok`
/// Example: `.Err`
pub struct PrefixEnumVariantPatternParselet;

impl PrefixPatternParselet for PrefixEnumVariantPatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        todo!("need to figure this out");
        let str_store = StrStore::new();

        let field_token = lexer.next(&mut str_store);

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

            payload = match payload_token.lexeme {
                str_store::UNDERSCORE => Payload::Default,
                id => Payload::Some(id),
            }
        }

        match field_token.ty {
            Ty::Identifier => Ok(Pattern::EnumVariant(EnumVariantPat {
                id: token.pos,
                enum_name: None,
                variant: field_token.lexeme,
                payload,
            })),
            _ => Err(ParseError::UnexpectedToken(
                field_token,
                "field name required after '.'".to_string(),
            )),
        }
    }
}

/// Parses dot patterns where the dot is the infix.
///
/// Example: `Ret.Ok`
/// Example: `mod::Ret.Err`
/// Example: `color::RGB.Red(r)`
pub struct InfixEnumVariantPatternParselet;

impl InfixPatternParselet for InfixEnumVariantPatternParselet {
    fn parse(
        &self,
        _parser: &PatternParser,
        lexer: &mut Lexer,
        left: Pattern,
        token: Token,
    ) -> Result<Pattern, ParseError> {
        let enum_name = match left {
            Pattern::Identifier(ident) => Some(IdentifierExpr {
                id: ident.id,
                module: None,
                name: ident.name,
            }),
            Pattern::ModuleIdentifier(ident) => Some(IdentifierExpr {
                id: ident.id,
                module: Some(ident.module),
                name: ident.name,
            }),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    token,
                    "enum variant pattern must use an identifier for the variant".to_string(),
                ));
            }
        };

        todo!("need to figure this out");
        let str_store = StrStore::new();

        let field_token = lexer.next(&mut str_store);

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

            payload = match payload_token.lexeme {
                str_store::UNDERSCORE => Payload::Default,
                id => Payload::Some(id),
            }
        }

        match field_token.ty {
            Ty::Identifier => Ok(Pattern::EnumVariant(EnumVariantPat {
                id: token.pos,
                enum_name,
                variant: field_token.lexeme,
                payload,
            })),
            _ => Err(ParseError::UnexpectedToken(
                field_token,
                "field name required after '.'".to_string(),
            )),
        }
    }
}
