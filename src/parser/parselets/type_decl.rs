use crate::ast::{
    Decl, EnumType, EnumVariant, IdentifierExpr, StructField, StructType, TypeDecl, TypeSpec,
};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixDeclParselet;
use crate::parser::types;
use crate::parser::{ParseError, Parser};

/// Dispatcher for type declarations - routes to struct or enum parselets
pub struct TypeDeclParselet;

impl PrefixDeclParselet for TypeDeclParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Decl, ParseError> {
        // The 'type' keyword has already been consumed by parse_declaration
        // Lookahead to see what comes next: name struct {...} or name enum {...}
        // First, we need to check after the type name
        let name = parser.consume()?;
        if name.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                name.clone(),
                format!(
                    "Expected type name after 'type' but found {:?}",
                    parser.lookahead(0),
                ),
            ));
        }

        // Look ahead past the name to see struct or enum
        let next = parser.consume()?;
        match next.kind {
            TokenKind::StructKeyword => parse_struct(name.lexeme, parser),
            TokenKind::EnumKeyword => parse_enum(name.lexeme, parser),
            _ => Err(ParseError::UnexpectedToken(
                next.clone(),
                format!(
                    "Expected 'struct' or 'enum' after type name but found {:?}",
                    next
                ),
            )),
        }
    }
}

fn parse_enum(name: String, parser: &mut Parser) -> Result<Decl, ParseError> {
    // Expect opening brace
    let matched = parser.match_token(TokenKind::OpenBrace)?;
    if !matched {
        return Err(ParseError::UnexpectedToken(
            parser.lookahead(0)?.clone(),
            "Expected '{' before enum body".to_string(),
        ));
    }

    // Parse enum variants
    let variants = parse_enum_variants(parser)?;

    // Expect closing brace
    let matched = parser.match_token(TokenKind::CloseBrace)?;
    if !matched {
        return Err(ParseError::UnexpectedToken(
            parser.lookahead(0)?.clone(),
            "Expected '}' after enum body".to_string(),
        ));
    }

    Ok(Decl::Type(TypeDecl {
        name: IdentifierExpr { name },
        type_spec: TypeSpec::Enum(EnumType { variants }),
    }))
}

/// Parse enum variants
/// Syntax: identifier ('(' type_spec ')')?
fn parse_enum_variants(parser: &mut Parser) -> Result<Vec<EnumVariant>, ParseError> {
    let mut variants = vec![];

    // Check for empty variant list
    if parser.lookahead(0)?.kind == TokenKind::CloseBrace {
        return Ok(variants);
    }

    loop {
        // Get variant name
        let tok = parser.lookahead(0)?;
        if tok.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                tok.clone(),
                "Expected variant name".to_string(),
            ));
        }
        let variant_name = parser.consume()?.lexeme;

        // Check for optional payload
        let payload = if parser.lookahead(0)?.kind == TokenKind::OpenParen {
            parser.consume()?; // consume '('

            // Parse the payload type
            let token = parser.consume()?;
            let payload_type = types::parse_type(parser, token)?;

            // Expect closing paren
            let matched = parser.match_token(TokenKind::CloseParen)?;
            if !matched {
                return Err(ParseError::UnexpectedToken(
                    parser.lookahead(0)?.clone(),
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
        let matched = parser.match_token(TokenKind::Semicolon)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                parser.lookahead(0)?.clone(),
                "Expected ';' after enum variant".to_string(),
            ));
        }

        // Check if there are more variants
        match parser.lookahead(0)?.kind {
            TokenKind::CloseBrace => break,
            TokenKind::Identifier => {
                // More variants coming
                continue;
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    parser.lookahead(0)?.clone(),
                    "Expected variant name or '}' in enum body".to_string(),
                ));
            }
        }
    }

    Ok(variants)
}

/// Parse struct declaration after 'type' keyword has been consumed
fn parse_struct(name: String, parser: &mut Parser) -> Result<Decl, ParseError> {
    // Expect opening brace
    let matched = parser.match_token(TokenKind::OpenBrace)?;
    if !matched {
        return Err(ParseError::UnexpectedToken(
            parser.lookahead(0)?.clone(),
            "Expected '{' before struct body".to_string(),
        ));
    }

    // Parse struct fields
    let fields = parse_struct_fields(parser)?;

    // Expect closing brace
    let matched = parser.match_token(TokenKind::CloseBrace)?;
    if !matched {
        return Err(ParseError::UnexpectedToken(
            parser.lookahead(0)?.clone(),
            "Expected '}' after struct body".to_string(),
        ));
    }

    Ok(Decl::Type(TypeDecl {
        name: IdentifierExpr { name },
        type_spec: TypeSpec::Struct(StructType { fields }),
    }))
}

/// Parse struct fields
/// Syntax: field_decl*
/// field_decl: identifier type_spec
fn parse_struct_fields(parser: &mut Parser) -> Result<Vec<StructField>, ParseError> {
    let mut fields = vec![];

    // Check for empty field list
    if parser.lookahead(0)?.kind == TokenKind::CloseBrace {
        return Ok(fields);
    }

    loop {
        // Get field name
        let tok = parser.lookahead(0)?;
        if tok.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                tok.clone(),
                "Expected field name".to_string(),
            ));
        }
        let field_name = parser.consume()?.lexeme;

        // Parse field type
        let token = parser.consume()?;
        let field_type = types::parse_type(parser, token)?;

        fields.push(StructField {
            name: field_name,
            type_spec: field_type,
        });

        // Expect semicolon after field
        let matched = parser.match_token(TokenKind::Semicolon)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                parser.lookahead(0)?.clone(),
                "Expected ';' after enum variant".to_string(),
            ));
        }

        // Check if there are more fields
        match parser.lookahead(0)?.kind {
            TokenKind::CloseBrace => break,
            TokenKind::Identifier => {
                // More fields coming
                continue;
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    parser.lookahead(0)?.clone(),
                    "Expected field name or '}' in struct body".to_string(),
                ));
            }
        }
    }

    Ok(fields)
}
