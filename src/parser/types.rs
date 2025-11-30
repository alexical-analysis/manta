use crate::ast::TypeSpec;
use crate::parser::ParseError;
use crate::parser::Parser;
use crate::parser::lexer::TokenKind;

/// Parse a type specification from the token stream.
/// Supports:
/// - built-in types: `i32`, `i16`, `i8`, `i64`, `u32`, `u16`, `u8`, `u64`, `f64`, `f32`, `str`, `bool`
/// - named types: any other identifier
/// - pointer: `* T`
/// - array: `[N] T` where N is an integer literal (compile-time size)
/// - slice: `[] T`
pub fn parse_type(parser: &mut Parser) -> Result<TypeSpec, ParseError> {
    let token = parser.lookahead(0)?;
    //let kind = tk.kind.clone();

    match &token.kind {
        TokenKind::Star => {
            // pointer
            parser.consume()?;
            let inner = parse_type(parser)?;
            Ok(TypeSpec::Pointer(Box::new(inner)))
        }

        TokenKind::OpenSquare => {
            // array or slice
            parser.consume()?; // consume '['
            let next = parser.lookahead(0)?;

            match &next.kind {
                TokenKind::CloseSquare => {
                    // slice: []T
                    parser.consume()?; // consume ']'
                    let inner = parse_type(parser)?;
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

                    let inner = parse_type(parser)?;
                    Ok(TypeSpec::Array(Box::new(inner), size))
                }
                other => Err(ParseError::InvalidTypeSpec(format!(
                    "Unexpected token in array type: {:?}",
                    other
                ))),
            }
        }

        TokenKind::Identifier => {
            let id = parser.consume()?;
            let name = id.lexeme;
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
                other => TypeSpec::Named(other.to_string()),
            };
            Ok(tyspec)
        }

        other => Err(ParseError::InvalidTypeSpec(format!(
            "Unexpected token while parsing type: {:?}",
            other
        ))),
    }
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
                    let type_spec = parse_type(&mut parser).unwrap();
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
            want: TypeSpec::Named("MyType".to_string()),
        },
        parse_type_pointer_type {
            input: "*i32",
            want: TypeSpec::Pointer(Box::new(TypeSpec::Int32)),
        },
        parse_type_array_type {
            input: "[3]i32",
            want: TypeSpec::Array(Box::new(TypeSpec::Int32), 3),
        },
        parse_type_slice_type {
            input: "[]i32",
            want: TypeSpec::Slice(Box::new(TypeSpec::Int32)),
        },
        parse_type_nested_pointer_array {
            input: "*[2]i32",
            want: TypeSpec::Pointer(Box::new(TypeSpec::Array(Box::new(TypeSpec::Int32), 2))),
        },
    );
}
