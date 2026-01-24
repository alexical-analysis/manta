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
                    let lex = lexer.lexeme(size_tok).replace('_', "");
                    let size = match lex.parse::<usize>() {
                        Ok(n) => n,
                        Err(_) => {
                            return Err(ParseError::InvalidTypeSpec(format!(
                                "Invalid array size: {}",
                                lex
                            )));
                        }
                    };

                    // expect closing bracket
                    match lexer.peek().kind {
                        TokenKind::CloseSquare => lexer.next_token(),
                        _ => {
                            return Err(ParseError::InvalidTypeSpec(
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
                other => Err(ParseError::InvalidTypeSpec(format!(
                    "Unexpected token in array type: {:?}",
                    other
                ))),
            }
        }

        TokenKind::Identifier => {
            let name = lexer.lexeme(token);
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
                other => match lexer.peek().kind {
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
                                module: Some(other.to_string()),
                                name: lexer.lexeme(type_name),
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
                    let mut lexer = Lexer::new($input);
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
