use crate::ast::{ArrayType, TypeSpec};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};

/// Parse a type specification from the token stream.
/// Supports:
/// - built-in types: `i32`, `i16`, `i8`, `i64`, `u32`, `u16`, `u8`, `u64`, `f64`, `f32`, `str`, `bool`
/// - named types: any other identifier
/// - pointer: `* T`
/// - array: `[N] T` where N is an integer literal (compile-time size)
/// - slice: `[] T`
pub fn parse_type(lexer: &mut Lexer, token: Token) -> Result<TypeSpec, ParseError> {
    match &token.kind {
        TokenKind::Star => {
            // pointer
            let next = lexer.next_token();
            let inner = parse_type(lexer, next)?;
            Ok(TypeSpec::Pointer(Box::new(inner)))
        }

        TokenKind::OpenSquare => {
            // array or slice
            let next = lexer.peek();

            match &next.kind {
                TokenKind::CloseSquare => {
                    // slice: []T
                    lexer.next_token(); // consume ']'
                    let next = lexer.next_token();
                    let inner = parse_type(lexer, next)?;
                    Ok(TypeSpec::Slice(Box::new(inner)))
                }
                TokenKind::Int => {
                    // array: [N]T

                    // TODO: this should actually parse an expression, since
                    // any constant expression will technically work her.
                    // That's gonna cause some problems once the refactor is done
                    let size_tok = lexer.next_token(); // Int
                    let lex = lexer.lexeme(size_tok.lexeme_id).replace('_', "");
                    let size = match lex.parse::<usize>() {
                        Ok(n) => n,
                        Err(_) => {
                            return Err(ParseError::InvalidTypeSpec(
                                size_tok,
                                format!("Invalid array size: {}", lex),
                            ));
                        }
                    };

                    // expect closing bracket
                    match lexer.peek().kind {
                        TokenKind::CloseSquare => lexer.next_token(),
                        _ => {
                            return Err(ParseError::InvalidTypeSpec(
                                next,
                                "Expected ']' after array size".into(),
                            ));
                        }
                    };

                    let next = lexer.next_token();
                    let inner = parse_type(lexer, next)?;
                    Ok(TypeSpec::Array(ArrayType {
                        type_spec: Box::new(inner),
                        size,
                    }))
                }
                other => Err(ParseError::InvalidTypeSpec(
                    next,
                    format!("Unexpected token in array type: {:?}", other),
                )),
            }
        }

        TokenKind::Identifier => {
            let name = lexer.lexeme(token.lexeme_id);
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
                _ => match lexer.peek().kind {
                    TokenKind::ColonColon => {
                        lexer.next_token();
                        let type_name = lexer.next_token();
                        if type_name.kind != TokenKind::Identifier {
                            return Err(ParseError::UnexpectedToken(
                                type_name,
                                "type name must be an identifier".to_string(),
                            ));
                        } else {
                            TypeSpec::Named {
                                module: Some(token.lexeme_id),
                                name: type_name.lexeme_id,
                            }
                        }
                    }
                    _ => TypeSpec::Named {
                        module: None,
                        name: token.lexeme_id,
                    },
                },
            };
            Ok(tyspec)
        }

        other => Err(ParseError::InvalidTypeSpec(
            token,
            format!("Unexpected token while parsing type: {:?}", other),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::Lexer;
    use crate::str_store::{StrID, StrStore};

    macro_rules! test_parse_type_spec {
        ($( $case:ident { input: $input:expr, want: $want:expr, }),*, ) => {
           $(
                #[test]
                fn $case() {
                    let mut str_store = StrStore::new();
                    let mut lexer = Lexer::new($input, &mut str_store);
                    let token = lexer.next_token();
                    let type_spec = parse_type(&mut lexer, token).unwrap();
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
                name: StrID(0),
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
