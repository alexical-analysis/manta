use crate::ast::{Expr, StructType, StructTypeField, StructValueField};
use crate::ast::{NamedType, StructConstructor, TypeSpec};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, InfixExprParselet, Precedence, PrefixExprParselet};
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::types;

/// Parses struct constructor expressions that are constructed by
/// named types
///
/// Examples:
/// Vec2{x: 10, y: 20}
/// Empty{}
pub struct StructConstructorParselet;

impl InfixExprParselet for StructConstructorParselet {
    fn parse(
        &self,
        parser: &ExprParser,
        lexer: &mut Lexer,
        left: Expr,
        token: Token,
    ) -> Result<Expr, ParseError> {
        // the left expression needs to be an identifer for this to be valid
        let type_spec = match left {
            Expr::Identifier(ident) => TypeSpec::Named(NamedType {
                id: ident.id,
                module: ident.module,
                name: ident.name,
            }),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    token,
                    format!(
                        "struct literals must start with an identifier got {:?}",
                        left
                    )
                    .to_string(),
                ));
            }
        };

        parse_struct_constructor(parser, lexer, type_spec)
    }

    fn precedence(&self) -> Precedence {
        Precedence::Call
    }
}

/// Parses anonymous struct constructor expressions
///
/// Examples:
/// struct{x: i32, y: i32}{x: 10, y: 20}
/// struct{}{}
pub struct AnonymousStructConstructorParselet;

impl PrefixExprParselet for AnonymousStructConstructorParselet {
    fn parse(
        &self,
        parser: &ExprParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Expr, ParseError> {
        let mut fields = vec![];

        loop {
            let field = lexer.next_token();
            if field.kind != TokenKind::Identifier {
                return Err(ParseError::UnexpectedToken(
                    field,
                    "field names must be identifiers".to_string(),
                ));
            }

            let type_token = lexer.next_token();
            let type_spec = types::parse_type(lexer, type_token)?;

            fields.push(StructTypeField {
                name: field.lexeme_id,
                type_spec,
            });

            let close = lexer.peek();
            if close.kind == TokenKind::CloseBrace {
                lexer.next_token();
                break;
            }
        }

        let struct_type = TypeSpec::Struct(StructType { fields });

        parse_struct_constructor(parser, lexer, struct_type)
    }
}

fn parse_struct_constructor(
    parser: &ExprParser,
    lexer: &mut Lexer,
    type_spec: TypeSpec,
) -> Result<Expr, ParseError> {
    let mut fields = vec![];

    // check for empty struct constructor
    let close_token = lexer.peek();
    if close_token.kind == TokenKind::CloseBrace {
        lexer.next_token();
        return Ok(Expr::StructConstructor(StructConstructor {
            type_spec,
            fields,
        }));
    }

    // parse struct fields
    loop {
        let field_token = lexer.peek();
        let field = parser.parse(lexer, Precedence::Base)?;
        let name = match field {
            Expr::Identifier(ident) => ident.name,
            _ => {
                return Err(ParseError::InvalidExpression(
                    field_token,
                    "field name must be an identifier".to_string(),
                ));
            }
        };

        let colon = lexer.next_token();
        if colon.kind != TokenKind::Colon {
            return Err(ParseError::UnexpectedToken(
                colon,
                "struct fields must be separated by colons".to_string(),
            ));
        }

        let value = parser.parse(lexer, Precedence::Base)?;

        fields.push(StructValueField {
            name,
            value: Box::new(value),
        });

        let comma = lexer.peek();
        if comma.kind != TokenKind::Comma {
            break;
        }
        lexer.next_token();
    }

    // Consume the semicolon between each field, this will either be explicit or inserted by the
    // ASI system that automatically adds semicolon's after certian lines
    if lexer.peek().kind == TokenKind::Semicolon {
        lexer.next_token();
    }

    // Expect a closing '}'
    let close = lexer.next_token();
    if close.kind != TokenKind::CloseBrace {
        return Err(ParseError::UnexpectedToken(
            close,
            "expected '}' after struct fields".to_string(),
        ));
    }

    Ok(Expr::StructConstructor(StructConstructor {
        type_spec,
        fields,
    }))
}
