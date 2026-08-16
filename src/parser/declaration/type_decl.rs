use crate::ast::{Decl, EnumType, EnumVariant, StructType, StructTypeField, TypeDecl, TypeSpec};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::parser::types;
use crate::str_store::StrStore;

/// Dispatcher for type declarations - routes to struct or enum parselets
pub struct TypeDeclParselet {
    pub public: bool,
}

impl DeclParselet for TypeDeclParselet {
    fn parse(
        &self,
        _parser: &DeclParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Decl, ParseError> {
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let name = lexer.next(&mut str_store);
        if name.ty != Ty::Identifier {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected type name after 'type'".to_string(),
            ));
        }

        let token = lexer.next(&mut str_store);
        match token.ty {
            Ty::StructKeyword => parse_struct(lexer, name, self.public),
            Ty::EnumKeyword => parse_enum(lexer, name, self.public),
            _ => {
                let type_spec = types::parse_type(lexer, token)?;
                Ok(Decl::Type(TypeDecl {
                    public: self.public,
                    id: name.pos,
                    name: name.lexeme,
                    type_spec,
                }))
            }
        }
    }
}

fn parse_enum(lexer: &mut Lexer, token: Token, public: bool) -> Result<Decl, ParseError> {
    todo!("need to figure this out");
    let mut str_store = StrStore::new();

    let open = lexer.next(&mut str_store);
    if open.ty != Ty::OpenBrace {
        return Err(ParseError::UnexpectedToken(
            open,
            "Expected '{' before enum body".to_string(),
        ));
    }

    // Parse enum variants
    let variants = parse_enum_variants(lexer)?;

    let close = lexer.next(&mut str_store);
    if close.ty != Ty::CloseBrace {
        return Err(ParseError::UnexpectedToken(
            close,
            "Expected '}' after enum body".to_string(),
        ));
    }

    Ok(Decl::Type(TypeDecl {
        public,
        id: token.pos,
        name: token.lexeme,
        type_spec: TypeSpec::Enum(EnumType { variants }),
    }))
}

/// Parse enum variants
/// Syntax: identifier ('(' type_spec ')')?
fn parse_enum_variants(lexer: &mut Lexer) -> Result<Vec<EnumVariant>, ParseError> {
    let mut variants = vec![];

    todo!("need to figure this out");
    let mut str_store = StrStore::new();

    // Check for empty variant list
    if lexer.peek().ty == Ty::CloseBrace {
        return Ok(variants);
    }

    loop {
        // Get variant name
        let token = lexer.next(&mut str_store);
        if token.ty != Ty::Identifier {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected variant name".to_string(),
            ));
        }
        let variant_name = token.lexeme;

        // Check for optional payload
        let payload = if lexer.peek().ty == Ty::OpenParen {
            lexer.next(&mut str_store);

            // Parse the payload type
            let token = lexer.next(&mut str_store);
            let payload_type = types::parse_type(lexer, token)?;

            // Expect closing paren
            let token = lexer.next(&mut str_store);
            if token.ty != Ty::CloseParen {
                return Err(ParseError::UnexpectedToken(
                    token,
                    "Expected ')' after variant payload".to_string(),
                ));
            }

            Some(payload_type)
        } else {
            None
        };

        variants.push(EnumVariant {
            name: variant_name,
            payload,
        });

        // Expect semicolon after variant
        let token = lexer.next(&mut str_store);
        if token.ty != Ty::Semicolon {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected ';' after enum variant".to_string(),
            ));
        }

        // Check if there are more variants
        match lexer.peek().ty {
            Ty::CloseBrace => break,
            Ty::Identifier => continue,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    lexer.peek().clone(),
                    "Expected variant name or '}' in enum body".to_string(),
                ));
            }
        }
    }

    Ok(variants)
}

/// Parse struct declaration after 'type' keyword has been consumed
fn parse_struct(lexer: &mut Lexer, token: Token, public: bool) -> Result<Decl, ParseError> {
    todo!("need to figure this out");
    let mut str_store = StrStore::new();

    // Expect opening brace
    let open = lexer.next(&mut str_store);
    if open.ty != Ty::OpenBrace {
        return Err(ParseError::UnexpectedToken(
            open,
            "Expected '{' before struct body".to_string(),
        ));
    }

    let fields = parse_struct_fields(lexer)?;

    let close = lexer.next(&mut str_store);
    if close.ty != Ty::CloseBrace {
        return Err(ParseError::UnexpectedToken(
            close,
            "Expected '}' after struct body".to_string(),
        ));
    }

    Ok(Decl::Type(TypeDecl {
        public,
        id: token.pos,
        name: token.lexeme,
        type_spec: TypeSpec::Struct(StructType { fields }),
    }))
}

/// Parse struct fields
/// Syntax: field_decl*
/// field_decl: identifier type_spec
fn parse_struct_fields(lexer: &mut Lexer) -> Result<Vec<StructTypeField>, ParseError> {
    let mut fields = vec![];
    todo!("need to figure this out");
    let mut str_store = StrStore::new();

    // Check for empty field list
    if lexer.peek().ty == Ty::CloseBrace {
        return Ok(fields);
    }

    loop {
        // check for a leading 'pub' keyword
        let mut public = false;
        let pub_token = lexer.peek();
        if pub_token.ty == Ty::PubKeyword {
            lexer.next(&mut str_store);
            public = true;
        }

        let token = lexer.next(&mut str_store);
        if token.ty != Ty::Identifier {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected field name".to_string(),
            ));
        }
        let name = token.lexeme;

        let token = lexer.next(&mut str_store);
        let type_spec = types::parse_type(lexer, token)?;

        fields.push(StructTypeField {
            public,
            name,
            type_spec,
        });

        let token = lexer.next(&mut str_store);
        if token.ty != Ty::Semicolon {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected ';' after enum variant".to_string(),
            ));
        }

        // Check if there are more fields
        match lexer.peek().ty {
            Ty::CloseBrace => break,
            Ty::Identifier => continue,
            Ty::PubKeyword => continue,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    lexer.peek().clone(),
                    "Expected field name or '}' in struct body".to_string(),
                ));
            }
        }
    }

    Ok(fields)
}
