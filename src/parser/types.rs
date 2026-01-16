use crate::ast::{ArrayType, TypeSpec};
use crate::parser::ParseError;
use crate::parser::Parser;
use crate::parser::lexer::{Token, TokenKind};

/// Parse a type specification from the token stream.
/// Supports:
/// - built-in types: `i32`, `i16`, `i8`, `i64`, `u32`, `u16`, `u8`, `u64`, `f64`, `f32`, `str`, `bool`
/// - named types: any other identifier
/// - pointer: `* T`
/// - array: `[N] T` where N is an integer literal (compile-time size)
/// - slice: `[] T`
pub fn parse_type(parser: &mut Parser, token: Token) -> Result<TypeSpec, ParseError> {
    match &token.kind {
        TokenKind::Star => {
            // pointer
            let next = parser.consume()?;
            let inner = parse_type(parser, next)?;
            Ok(TypeSpec::Pointer(Box::new(inner)))
        }

        TokenKind::OpenSquare => {
            // array or slice
            let next = parser.lookahead(0)?;

            match &next.kind {
                TokenKind::CloseSquare => {
                    // slice: []T
                    parser.consume()?; // consume ']'
                    let next = parser.consume()?;
                    let inner = parse_type(parser, next)?;
                    Ok(TypeSpec::Slice(Box::new(inner)))
                }
                TokenKind::Int => {
                    // array: [N]T
                    let size_tok = parser.consume()?; // Int
                    let lex = size_tok.lexeme.replace('_', "");
                    let size = lex.parse::<usize>().map_err(|_| {
                        ParseError::InvalidTypeSpec(format!(
                            "Invalid array size: {}",
                            size_tok.lexeme
                        ))
                    })?;

                    // expect closing bracket
                    if !parser.match_token(TokenKind::CloseSquare)? {
                        return Err(ParseError::InvalidTypeSpec(
                            "Expected ']' after array size".into(),
                        ));
                    }

                    let next = parser.consume()?;
                    let inner = parse_type(parser, next)?;
                    Ok(TypeSpec::Array(ArrayType {
                        type_spec: Box::new(inner),
                        size,
                    }))
                }
                other => Err(ParseError::InvalidTypeSpec(format!(
                    "Unexpected token in array type: {:?}",
                    other
                ))),
            }
        }

        TokenKind::Identifier => {
            let name = token.lexeme;
            let tyspec = match name.as_str() {
                "i32" => TypeSpec::Int32,
                "i16" => TypeSpec::Int16,
                "i8" => TypeSpec::Int8,
                "i64" => TypeSpec::Int64,
                "u32" => TypeSpec::UInt32,
                "u16" => TypeSpec::UInt16,
                "u8" => TypeSpec::UInt8,
                "u64" => TypeSpec::UInt64,
                "f64" => TypeSpec::Float64,
                "f32" => TypeSpec::Float32,
                "str" => TypeSpec::String,
                "bool" => TypeSpec::Bool,
                other => match parser.lookahead(0)?.kind {
                    TokenKind::ColonColon => {
                        parser.consume()?;
                        let type_name = parser.consume()?;
                        if type_name.kind != TokenKind::Identifier {
                            return Err(ParseError::UnexpectedToken(
                                type_name,
                                "type name must be an identifier".to_string(),
                            ));
                        } else {
                            TypeSpec::Named {
                                module: Some(other.to_string()),
                                name: type_name.lexeme,
                            }
                        }
                    }
                    _ => TypeSpec::Named {
                        module: None,
                        name: other.to_string(),
                    },
                },
            };
            Ok(tyspec)
        }

        other => Err(ParseError::InvalidTypeSpec(format!(
            "Unexpected token while parsing type: {:?}",
            other
        ))),
    }
}

pub fn is_type_keyword(ident: &str) -> bool {
    matches!(
        ident,
        "i8" | "i16"
            | "i32"
            | "i64"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "f32"
            | "f64"
            | "str"
            | "bool"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::Lexer;

    macro_rules! test_parse_type_spec {
        ($( $case:ident { input: $input:expr, want: $want:expr, }),*, ) => {
           $(
                #[test]
                fn $case() {
                    let mut parser = Parser::new(Lexer::new($input));
                    let token = parser.consume().unwrap();
                    let type_spec = parse_type(&mut parser, token).unwrap();
                    assert_eq!(type_spec, $want);
                }
           )*
        };
    }

    test_parse_type_spec!(
        parse_type_primitive_i32 {
            input: "i32",
            want: TypeSpec::Int32,
        },
        parse_type_named_type {
            input: "MyType",
            want: TypeSpec::Named {
                module: None,
                name: "MyType".to_string()
            },
        },
        parse_type_pointer_type {
            input: "*i32",
            want: TypeSpec::Pointer(Box::new(TypeSpec::Int32)),
        },
        parse_type_array_type {
            input: "[3]i32",
            want: TypeSpec::Array(ArrayType {
                type_spec: Box::new(TypeSpec::Int32),
                size: 3
            }),
        },
        parse_type_slice_type {
            input: "[]i32",
            want: TypeSpec::Slice(Box::new(TypeSpec::Int32)),
        },
        parse_type_nested_pointer_array {
            input: "*[2]i32",
            want: TypeSpec::Pointer(Box::new(TypeSpec::Array(ArrayType {
                type_spec: Box::new(TypeSpec::Int32),
                size: 2
            }))),
        },
    );
}
