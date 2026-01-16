use crate::ast::Pattern;
use crate::parser::lexer::Token;
use crate::parser::parselets::PrefixPatternParselet;
use crate::parser::types;
use crate::parser::{ParseError, Parser};

/// Parses type patterns
///
/// Example: `i32`, `*bool`, `[]Vec3`
pub struct TypePatternParselet;

impl PrefixPatternParselet for TypePatternParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> Result<Pattern, ParseError> {
        let type_spec = types::parse_type(parser, token)?;

        Ok(Pattern::TypeSpec(type_spec))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ArrayType, TypeSpec};

    crate::test_parselet!(
        TypePatternParselet,
        test_parse_pointer {
            input: "*foo",
            want: Pattern::TypeSpec(pat),
            want_value: assert_eq!(
                pat,
                TypeSpec::Pointer(Box::new(TypeSpec::Named {
                    module: None,
                    name: "foo".to_string(),
                })),
            ),
        },
        test_parse_double_pointer {
            input: "**bar",
            want: Pattern::TypeSpec(pat),
            want_value: assert_eq!(
                pat,
                TypeSpec::Pointer(Box::new(TypeSpec::Pointer(Box::new(TypeSpec::Named {
                    module: None,
                    name: "bar".to_string(),
                })))),
            ),
        },
        test_parse_slice {
            input: "[]Vec2",
            want: Pattern::TypeSpec(pat),
            want_value: assert_eq!(
                pat,
                TypeSpec::Slice(Box::new(TypeSpec::Named {
                    module: None,
                    name: "Vec2".to_string(),
                }))
            ),
        },
        test_parse_3d_array {
            input: "[10][11][12]bool",
            want: Pattern::TypeSpec(pat),
            want_value: assert_eq!(
                pat,
                TypeSpec::Array(ArrayType {
                    size: 10,
                    type_spec: Box::new(TypeSpec::Array(ArrayType {
                        size: 11,
                        type_spec: Box::new(TypeSpec::Array(ArrayType {
                            size: 12,
                            type_spec: Box::new(TypeSpec::Bool),
                        })),
                    })),
                }),
            ),
        },
        test_parse_array_pointer_slice_pointer {
            input: "[3]*[]*pet::Dog",
            want: Pattern::TypeSpec(pat),
            want_value: assert_eq!(
                pat,
                TypeSpec::Array(ArrayType {
                    size: 3,
                    type_spec: Box::new(TypeSpec::Pointer(Box::new(TypeSpec::Slice(Box::new(
                        TypeSpec::Pointer(Box::new(TypeSpec::Named {
                            module: Some("pet".to_string()),
                            name: "Dog".to_string(),
                        }))
                    ))))),
                }),
            ),
        },
    );
}
