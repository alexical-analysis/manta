use crate::ast::{
    Decl, EnumType, EnumVariant, IdentifierExpr, StructField, StructType, TypeDecl, TypeSpec,
};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::types;

/// Dispatcher for type declarations - routes to struct or enum parselets
pub struct TypeDeclParselet;

impl DeclParselet for TypeDeclParselet {
    fn parse(
        &self,
        _parser: &DeclParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Decl, ParseError> {
        let name = lexer.next_token();
        if name.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected type name after 'type'".to_string(),
            ));
        }

        let token = lexer.next_token();
        match token.kind {
            TokenKind::StructKeyword => parse_struct(lexer, name),
            TokenKind::EnumKeyword => parse_enum(lexer, name),
            _ => Err(ParseError::UnexpectedToken(
                token,
                "Expected 'struct' or 'enum' after type name".to_string(),
            )),
        }
    }
}

fn parse_enum(lexer: &mut Lexer, name: Token) -> Result<Decl, ParseError> {
    let name = name.lexeme_id;

    let token = lexer.next_token();
    if token.kind != TokenKind::OpenBrace {
        return Err(ParseError::UnexpectedToken(
            token,
            "Expected '{' before enum body".to_string(),
        ));
    }

    // Parse enum variants
    let variants = parse_enum_variants(lexer)?;

    let token = lexer.next_token();
    if token.kind != TokenKind::CloseBrace {
        return Err(ParseError::UnexpectedToken(
            token,
            "Expected '}' after enum body".to_string(),
        ));
    }

    Ok(Decl::Type(TypeDecl {
        name,
        type_spec: TypeSpec::Enum(EnumType { variants }),
    }))
}

/// Parse enum variants
/// Syntax: identifier ('(' type_spec ')')?
fn parse_enum_variants(lexer: &mut Lexer) -> Result<Vec<EnumVariant>, ParseError> {
    let mut variants = vec![];

    // Check for empty variant list
    if lexer.peek().kind == TokenKind::CloseBrace {
        return Ok(variants);
    }

    loop {
        // Get variant name
        let token = lexer.next_token();
        if token.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected variant name".to_string(),
            ));
        }
        let variant_name = token.lexeme_id;

        // Check for optional payload
        let payload = if lexer.peek().kind == TokenKind::OpenParen {
            lexer.next_token();

            // Parse the payload type
            let token = lexer.next_token();
            let payload_type = types::parse_type(lexer, token)?;

            // Expect closing paren
            let token = lexer.next_token();
            if token.kind != TokenKind::CloseParen {
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
        let token = lexer.next_token();
        if token.kind != TokenKind::Semicolon {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected ';' after enum variant".to_string(),
            ));
        }

        // Check if there are more variants
        match lexer.peek().kind {
            TokenKind::CloseBrace => break,
            TokenKind::Identifier => continue,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    lexer.peek(),
                    "Expected variant name or '}' in enum body".to_string(),
                ));
            }
        }
    }

    Ok(variants)
}

/// Parse struct declaration after 'type' keyword has been consumed
fn parse_struct(lexer: &mut Lexer, name: Token) -> Result<Decl, ParseError> {
    let name = name.lexeme_id;

    // Expect opening brace
    let token = lexer.next_token();
    if token.kind != TokenKind::OpenBrace {
        return Err(ParseError::UnexpectedToken(
            token,
            "Expected '{' before struct body".to_string(),
        ));
    }

    let fields = parse_struct_fields(lexer)?;

    let token = lexer.next_token();
    if token.kind != TokenKind::CloseBrace {
        return Err(ParseError::UnexpectedToken(
            token,
            "Expected '}' after struct body".to_string(),
        ));
    }

    Ok(Decl::Type(TypeDecl {
        name,
        type_spec: TypeSpec::Struct(StructType { fields }),
    }))
}

/// Parse struct fields
/// Syntax: field_decl*
/// field_decl: identifier type_spec
fn parse_struct_fields(lexer: &mut Lexer) -> Result<Vec<StructField>, ParseError> {
    let mut fields = vec![];

    // Check for empty field list
    if lexer.peek().kind == TokenKind::CloseBrace {
        return Ok(fields);
    }

    loop {
        let token = lexer.next_token();
        if token.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected field name".to_string(),
            ));
        }
        let name = token.lexeme_id;

        let token = lexer.next_token();
        let type_spec = types::parse_type(lexer, token)?;

        fields.push(StructField { name, type_spec });

        let token = lexer.next_token();
        if token.kind != TokenKind::Semicolon {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected ';' after enum variant".to_string(),
            ));
        }

        // Check if there are more fields
        match lexer.peek().kind {
            TokenKind::CloseBrace => break,
            TokenKind::Identifier => continue,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    lexer.peek(),
                    "Expected field name or '}' in struct body".to_string(),
                ));
            }
        }
    }

    Ok(fields)
}
