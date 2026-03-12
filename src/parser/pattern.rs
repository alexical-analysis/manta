mod enum_variant;
mod identifier_pattern;
mod literal_pattern;
mod mod_pattern;
mod payload_pattern;
mod type_pattern;

use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Pattern;
use crate::parser::ParseError;
use crate::parser::lexer::Lexer;
use crate::parser::lexer::{Token, TokenKind};

use enum_variant::{InfixEnumVariantPatternParselet, PrefixEnumVariantPatternParselet};
use identifier_pattern::IdentifierPatternParselet;
use literal_pattern::LiteralPatternParselet;
use mod_pattern::ModPatternParselet;
use payload_pattern::PayloadPatternParselet;
use type_pattern::TypePatternParselet;

/// Trait for prefix pattern parselets
pub trait PrefixPatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError>;
}

/// Trait for infix pattern parselets.
pub trait InfixPatternParselet {
    fn parse(
        &self,
        parser: &PatternParser,
        lexer: &mut Lexer,
        left: Pattern,
        token: Token,
    ) -> Result<Pattern, ParseError>;
}

/// PatternParser parses manta patterns
///
/// Example `mod::Enum.Variant`
/// Example `.Variant(ident)`
/// Example `Type(ident)`
/// Example `literal`
/// Example `_`
pub struct PatternParser {
    prefix_parselets: HashMap<TokenKind, Rc<dyn PrefixPatternParselet>>,
    infix_parselets: HashMap<TokenKind, Rc<dyn InfixPatternParselet>>,
}

impl PatternParser {
    pub fn new() -> Self {
        let mut prefix_parselets: HashMap<TokenKind, Rc<dyn PrefixPatternParselet>>;
        prefix_parselets = HashMap::new();
        prefix_parselets.insert(TokenKind::Identifier, Rc::new(IdentifierPatternParselet));
        prefix_parselets.insert(TokenKind::Dot, Rc::new(PrefixEnumVariantPatternParselet));
        prefix_parselets.insert(TokenKind::Star, Rc::new(TypePatternParselet));
        prefix_parselets.insert(TokenKind::OpenSquare, Rc::new(TypePatternParselet));
        prefix_parselets.insert(TokenKind::TrueLiteral, Rc::new(LiteralPatternParselet));
        prefix_parselets.insert(TokenKind::FalseLiteral, Rc::new(LiteralPatternParselet));
        prefix_parselets.insert(TokenKind::Int, Rc::new(LiteralPatternParselet));
        prefix_parselets.insert(TokenKind::Float, Rc::new(LiteralPatternParselet));
        prefix_parselets.insert(TokenKind::Str, Rc::new(LiteralPatternParselet));

        let mut infix_parselets: HashMap<TokenKind, Rc<dyn InfixPatternParselet>> = HashMap::new();
        infix_parselets.insert(TokenKind::Dot, Rc::new(InfixEnumVariantPatternParselet));
        infix_parselets.insert(TokenKind::ColonColon, Rc::new(ModPatternParselet));
        infix_parselets.insert(TokenKind::OpenParen, Rc::new(PayloadPatternParselet));

        PatternParser {
            prefix_parselets,
            infix_parselets,
        }
    }

    pub fn parse(&self, lexer: &mut Lexer) -> Result<Pattern, ParseError> {
        let token = lexer.next_token();

        let parselet = self.prefix_parselets.get(&token.kind);
        if parselet.is_none() {
            return Err(ParseError::UnexpectedToken(
                token,
                "invalid pattern prefix".to_string(),
            ));
        }

        let prefix = parselet.unwrap().clone();
        let mut left = prefix.parse(lexer, token)?;

        loop {
            let token = lexer.peek();
            match token.kind {
                TokenKind::OpenBrace | TokenKind::Equal => break,
                _ => (),
            };

            let parselet = self.infix_parselets.get(&token.kind);
            if let Some(parselet) = parselet {
                let token = lexer.next_token();
                left = parselet.parse(self, lexer, left, token)?;
            } else {
                return Err(ParseError::UnexpectedToken(
                    token,
                    "invalid infix pattern".to_string(),
                ));
            }
        }

        Ok(left)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{
        ArrayType, EnumVariantPat, IdentifierExpr, IdentifierPat, NamedType, Pattern, PayloadPat,
        TypeSpec, TypeSpecPat,
    };
    use crate::parser::lexer::{Lexer, SourceID};
    use crate::str_store::{self, StrID, StrStore};
    use pretty_assertions::assert_eq;

    macro_rules! test_parse_patterns {
        ( $( $case:ident { input: $input:expr, want: $want:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let parser = PatternParser::new();
                    let mut str_store = StrStore::new();
                    let mut lexer = Lexer::new($input, &mut str_store);
                    let pattern = parser.parse(&mut lexer).unwrap();
                    assert_eq!(pattern, $want)
                }
            )*
        }
    }

    test_parse_patterns!(
        parse_pattern_int_literal {
            input: "42 {",
            want: Pattern::IntLiteral(42),
        },
        parse_pattern_string_literal {
            input: r#""hello" {"#,
            want: Pattern::StringLiteral(StrID::from_usize(0)),
        },
        parse_pattern_float_literal {
            input: "3.45 {",
            want: Pattern::FloatLiteral(3.45),
        },
        parse_pattern_true_literal {
            input: "true {",
            want: Pattern::BoolLiteral(true),
        },
        parse_pattern_false_literal {
            input: "false {",
            want: Pattern::BoolLiteral(false),
        },
        parse_pattern_default {
            input: "_ =",
            want: Pattern::Default,
        },
        parse_pattern_identifier {
            input: "my_var =",
            want: Pattern::Identifier(IdentifierPat {
                id: SourceID::from_usize(0),
                name: StrID::from_usize(0)
            }),
        },
        parse_pattern_type_match {
            input: "f32(f) =",
            want: Pattern::TypeSpec(TypeSpecPat {
                id: SourceID::from_usize(0),
                type_spec: TypeSpec::Float32,
                payload: StrID::from_usize(1),
            }),
        },
        parse_pattern_pointer {
            input: "*foo(_) =",
            want: Pattern::TypeSpec(TypeSpecPat {
                id: SourceID::from_usize(0),
                type_spec: TypeSpec::Pointer(Box::new(TypeSpec::Named(NamedType {
                    id: SourceID::from_usize(1),
                    module: None,
                    name: StrID::from_usize(1),
                }))),
                payload: str_store::UNDERSCORE,
            }),
        },
        parse_pattern_double_pointer {
            input: "**bar(_) {",
            want: Pattern::TypeSpec(TypeSpecPat {
                id: SourceID::from_usize(0),
                type_spec: TypeSpec::Pointer(Box::new(TypeSpec::Pointer(Box::new(
                    TypeSpec::Named(NamedType {
                        id: SourceID::from_usize(2),
                        module: None,
                        name: StrID::from_usize(1),
                    })
                )))),
                payload: str_store::UNDERSCORE,
            }),
        },
        parse_pattern_slice {
            input: "[]Vec2(_) =",
            want: Pattern::TypeSpec(TypeSpecPat {
                id: SourceID::from_usize(0),
                type_spec: TypeSpec::Slice(Box::new(TypeSpec::Named(NamedType {
                    id: SourceID::from_usize(2),
                    module: None,
                    name: StrID::from_usize(2),
                }))),
                payload: str_store::UNDERSCORE,
            }),
        },
        parse_pattern_3d_array {
            input: "[10][11][12]bool(_) =",
            want: Pattern::TypeSpec(TypeSpecPat {
                id: SourceID::from_usize(0),
                type_spec: TypeSpec::Array(ArrayType {
                    size: 10,
                    type_spec: Box::new(TypeSpec::Array(ArrayType {
                        size: 11,
                        type_spec: Box::new(TypeSpec::Array(ArrayType {
                            size: 12,
                            type_spec: Box::new(TypeSpec::Bool),
                        })),
                    })),
                }),
                payload: str_store::UNDERSCORE,
            }),
        },
        parse_pattern_array_pointer_slice_pointer {
            input: "[3]*[]*pet::Dog(_) =",
            want: Pattern::TypeSpec(TypeSpecPat {
                id: SourceID::from_usize(0),
                type_spec: TypeSpec::Array(ArrayType {
                    size: 3,
                    type_spec: Box::new(TypeSpec::Pointer(Box::new(TypeSpec::Slice(Box::new(
                        TypeSpec::Pointer(Box::new(TypeSpec::Named(NamedType {
                            id: SourceID::from_usize(7),
                            module: Some(StrID::from_usize(4)),
                            name: StrID::from_usize(6),
                        })))
                    ))))),
                }),
                payload: str_store::UNDERSCORE,
            }),
        },
        parse_pattern_simple_identifier {
            input: "foo {",
            want: Pattern::Identifier(IdentifierPat {
                id: SourceID::from_usize(0),
                name: StrID::from_usize(0)
            }),
        },
        parse_pattern_variable_name {
            input: "my_variable {",
            want: Pattern::Identifier(IdentifierPat {
                id: SourceID::from_usize(0),
                name: StrID::from_usize(0)
            }),
        },
        parse_pattern_identifier_with_numbers {
            input: "var123 {",
            want: Pattern::Identifier(IdentifierPat {
                id: SourceID::from_usize(0),
                name: StrID::from_usize(0)
            }),
        },
        parse_pattern_dot_inferred_variant {
            input: ".Ok =",
            want: Pattern::EnumVariant(EnumVariantPat {
                id: SourceID::from_usize(0),
                enum_name: None,
                variant: StrID::from_usize(1),
                payload: None,
            }),
        },
        parse_pattern_dot_variant {
            input: "Ret.Ok {",
            want: Pattern::EnumVariant(EnumVariantPat {
                id: SourceID::from_usize(3),
                enum_name: Some(IdentifierExpr {
                    id: SourceID::from_usize(0),
                    module: None,
                    name: StrID::from_usize(0),
                }),
                variant: StrID::from_usize(2),
                payload: None,
            },),
        },
        parse_pattern_module_access_identifier {
            input: "math::Vec3(_) =",
            want: Pattern::TypeSpec(TypeSpecPat {
                id: SourceID::from_usize(4),
                type_spec: TypeSpec::Named(NamedType {
                    id: SourceID::from_usize(0),
                    module: Some(StrID::from_usize(0)),
                    name: StrID::from_usize(2),
                }),
                payload: str_store::UNDERSCORE,
            }),
        },
        parse_pattern_module_access_dot_variant {
            input: "result::Ret.Ok =",
            want: Pattern::EnumVariant(EnumVariantPat {
                id: SourceID::from_usize(11),
                enum_name: Some(IdentifierExpr {
                    id: SourceID::from_usize(0),
                    module: Some(StrID::from_usize(0)),
                    name: StrID::from_usize(2),
                }),
                variant: StrID::from_usize(4),
                payload: None,
            }),
        },
        parse_pattern_module_access_payload {
            input: "std::Option.Some(x) {",
            want: Pattern::EnumVariant(EnumVariantPat {
                id: SourceID::from_usize(11),
                enum_name: Some(IdentifierExpr {
                    id: SourceID::from_usize(0),
                    module: Some(StrID::from_usize(0)),
                    name: StrID::from_usize(2),
                }),
                variant: StrID::from_usize(4),
                payload: Some(StrID::from_usize(6)),
            }),
        },
        parse_pattern_payload_simple {
            input: "Result(err) =",
            want: Pattern::TypeSpec(TypeSpecPat {
                id: SourceID::from_usize(0),
                type_spec: TypeSpec::Named(NamedType {
                    id: SourceID::from_usize(0),
                    module: None,
                    name: StrID::from_usize(0)
                }),
                payload: StrID::from_usize(2),
            }),
        },
        parse_pattern_payload_dot_access {
            input: "Ret.Ok(value) {",
            want: Pattern::EnumVariant(EnumVariantPat {
                id: SourceID::from_usize(3),
                enum_name: Some(IdentifierExpr {
                    id: SourceID::from_usize(0),
                    module: None,
                    name: StrID::from_usize(0)
                }),
                variant: StrID::from_usize(2),
                payload: Some(StrID::from_usize(4)),
            }),
        },
        parse_pattern_payload_dot_inferred {
            input: ".Some(item) =",
            want: Pattern::EnumVariant(EnumVariantPat {
                id: SourceID::from_usize(0),
                enum_name: None,
                variant: StrID::from_usize(1),
                payload: Some(StrID::from_usize(3)),
            }),
        },
    );

    #[test]
    fn parse_pattern_invalid() {
        let mut str_store = StrStore::new();
        let mut lexer = Lexer::new("+ ", &mut str_store);
        let parser = PatternParser::new();
        let result = parser.parse(&mut lexer);
        assert!(result.is_err());
    }
}
