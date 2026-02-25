mod dot_pattern;
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

use dot_pattern::{InfixDotPatternParselet, PrefixDotPatternParselet};
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
        prefix_parselets.insert(TokenKind::Dot, Rc::new(PrefixDotPatternParselet));
        prefix_parselets.insert(TokenKind::Star, Rc::new(TypePatternParselet));
        prefix_parselets.insert(TokenKind::OpenSquare, Rc::new(TypePatternParselet));
        prefix_parselets.insert(TokenKind::TrueLiteral, Rc::new(LiteralPatternParselet));
        prefix_parselets.insert(TokenKind::FalseLiteral, Rc::new(LiteralPatternParselet));
        prefix_parselets.insert(TokenKind::Int, Rc::new(LiteralPatternParselet));
        prefix_parselets.insert(TokenKind::Float, Rc::new(LiteralPatternParselet));
        prefix_parselets.insert(TokenKind::Str, Rc::new(LiteralPatternParselet));

        let mut infix_parselets: HashMap<TokenKind, Rc<dyn InfixPatternParselet>> = HashMap::new();
        infix_parselets.insert(TokenKind::Dot, Rc::new(InfixDotPatternParselet));
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
        ArrayType, DotAccessPat, IdentifierPat, ModuleAccesPat, Pattern, PayloadPat, TypeSpec,
    };
    use crate::parser::lexer::Lexer;
    use crate::str_store::StrStore;
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
            want: Pattern::StringLiteral(0),
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
            want: Pattern::Identifier(IdentifierPat { id: 0, name: 0 }),
        },
        parse_pattern_type_match {
            input: "f32(f) =",
            want: Pattern::Payload(PayloadPat {
                pat: Box::new(Pattern::Identifier(IdentifierPat {
                    id: 0,
                    name: 18446744073709551606
                })),
                payload: IdentifierPat { id: 4, name: 1 },
            }),
        },
        parse_pattern_pointer {
            input: "*foo =",
            want: Pattern::TypeSpec(TypeSpec::Pointer(Box::new(TypeSpec::Named {
                module: None,
                name: 1,
            }))),
        },
        parse_pattern_double_pointer {
            input: "**bar {",
            want: Pattern::TypeSpec(TypeSpec::Pointer(Box::new(TypeSpec::Pointer(Box::new(
                TypeSpec::Named {
                    module: None,
                    name: 1,
                }
            ))))),
        },
        parse_pattern_slice {
            input: "[]Vec2 =",
            want: Pattern::TypeSpec(TypeSpec::Slice(Box::new(TypeSpec::Named {
                module: None,
                name: 2,
            }))),
        },
        parse_pattern_3d_array {
            input: "[10][11][12]bool =",
            want: Pattern::TypeSpec(TypeSpec::Array(ArrayType {
                size: 10,
                type_spec: Box::new(TypeSpec::Array(ArrayType {
                    size: 11,
                    type_spec: Box::new(TypeSpec::Array(ArrayType {
                        size: 12,
                        type_spec: Box::new(TypeSpec::Bool),
                    })),
                })),
            }),),
        },
        parse_pattern_array_pointer_slice_pointer {
            input: "[3]*[]*pet::Dog =",
            want: Pattern::TypeSpec(TypeSpec::Array(ArrayType {
                size: 3,
                type_spec: Box::new(TypeSpec::Pointer(Box::new(TypeSpec::Slice(Box::new(
                    TypeSpec::Pointer(Box::new(TypeSpec::Named {
                        module: Some(4),
                        name: 6,
                    }))
                ))))),
            }),),
        },
        parse_pattern_simple_identifier {
            input: "foo {",
            want: Pattern::Identifier(IdentifierPat { id: 0, name: 0 }),
        },
        parse_pattern_variable_name {
            input: "my_variable {",
            want: Pattern::Identifier(IdentifierPat { id: 0, name: 0 }),
        },
        parse_pattern_identifier_with_numbers {
            input: "var123 {",
            want: Pattern::Identifier(IdentifierPat { id: 0, name: 0 }),
        },
        parse_pattern_dot_inferred_variant {
            input: ".Ok =",
            want: Pattern::DotAccess(DotAccessPat {
                target: None,
                field: IdentifierPat { id: 1, name: 1 },
            }),
        },
        parse_pattern_dot_variant {
            input: "Ret.Ok {",
            want: Pattern::DotAccess(DotAccessPat {
                target: Some(Box::new(Pattern::Identifier(IdentifierPat {
                    id: 0,
                    name: 0
                }))),
                field: IdentifierPat { id: 4, name: 2 },
            },),
        },
        parse_pattern_module_access_identifier {
            input: "math::Vec3 =",
            want: Pattern::ModuleAccess(ModuleAccesPat {
                module: Box::new(IdentifierPat { id: 0, name: 0 }),
                pat: Box::new(Pattern::Identifier(IdentifierPat { id: 6, name: 2 })),
            }),
        },
        parse_pattern_module_access_dot_variant {
            input: "result::Ret.Ok =",
            want: Pattern::ModuleAccess(ModuleAccesPat {
                module: Box::new(IdentifierPat { id: 0, name: 0 }),
                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                    target: Some(Box::new(Pattern::Identifier(IdentifierPat {
                        id: 8,
                        name: 2
                    }))),
                    field: IdentifierPat { id: 12, name: 4 },
                })),
            }),
        },
        parse_pattern_module_access_payload {
            input: "std::Option.Some(x) {",
            want: Pattern::ModuleAccess(ModuleAccesPat {
                module: Box::new(IdentifierPat { id: 0, name: 0 }),
                pat: Box::new(Pattern::Payload(PayloadPat {
                    pat: Box::new(Pattern::DotAccess(DotAccessPat {
                        target: Some(Box::new(Pattern::Identifier(IdentifierPat {
                            id: 5,
                            name: 2
                        }))),
                        field: IdentifierPat { id: 12, name: 4 },
                    })),
                    payload: IdentifierPat { id: 17, name: 6 },
                })),
            }),
        },
        parse_pattern_payload_simple {
            input: "Result(err) =",
            want: Pattern::Payload(PayloadPat {
                pat: Box::new(Pattern::Identifier(IdentifierPat { id: 0, name: 0 })),
                payload: IdentifierPat { id: 7, name: 2 },
            }),
        },
        parse_pattern_payload_dot_access {
            input: "Ret.Ok(value) {",
            want: Pattern::Payload(PayloadPat {
                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                    target: Some(Box::new(Pattern::Identifier(IdentifierPat {
                        id: 0,
                        name: 0
                    }))),
                    field: IdentifierPat { id: 4, name: 2 },
                })),
                payload: IdentifierPat { id: 7, name: 4 },
            }),
        },
        parse_pattern_payload_dot_inferred {
            input: ".Some(item) =",
            want: Pattern::Payload(PayloadPat {
                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                    target: None,
                    field: IdentifierPat { id: 1, name: 1 },
                })),
                payload: IdentifierPat { id: 6, name: 3 },
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
