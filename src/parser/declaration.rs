mod const_decl;
mod function_declaration;
mod mod_declaration;
mod type_decl;
mod use_decl;
mod var_decl;

use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BlockStmt, Decl, Expr};
use crate::parser::ParseError;
use crate::parser::statement::StmtParser;
use crate::parser::{Lexer, Token, TokenKind};

use const_decl::ConstDeclParselet;
use function_declaration::FunctionDeclParselet;
use mod_declaration::ModDeclParselet;
use type_decl::TypeDeclParselet;
use use_decl::UseDeclParselet;
use var_decl::VarDeclParselet;

/// Trait for top-level declaration parselets.
pub trait DeclParselet {
    /// Parse a top-level declaration given the consumed token.
    fn parse(
        &self,
        parser: &DeclParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Decl, ParseError>;
}

pub struct DeclParser {
    statement_parser: StmtParser,
    parselets: HashMap<TokenKind, Rc<dyn DeclParselet>>,
}

impl DeclParser {
    pub fn new() -> Self {
        let mut parselets: HashMap<TokenKind, Rc<dyn DeclParselet>> = HashMap::new();
        parselets.insert(TokenKind::FnKeyword, Rc::new(FunctionDeclParselet));
        parselets.insert(TokenKind::TypeKeyword, Rc::new(TypeDeclParselet));
        parselets.insert(TokenKind::ConstKeyword, Rc::new(ConstDeclParselet));
        parselets.insert(TokenKind::VarKeyword, Rc::new(VarDeclParselet));
        parselets.insert(TokenKind::UseKeyword, Rc::new(UseDeclParselet));
        parselets.insert(TokenKind::ModKeyword, Rc::new(ModDeclParselet));

        let statement_parser = StmtParser::new();
        DeclParser {
            parselets,
            statement_parser,
        }
    }

    /// Parse a top level declration for a manta program.
    pub fn parse(&self, lexer: &mut Lexer) -> Result<Decl, ParseError> {
        let token = lexer.next_token();

        let prefix_opt = self.parselets.get(&token.kind);
        if prefix_opt.is_none() {
            return Err(ParseError::UnexpectedToken(
                token,
                format!("Unexpected token at top level: {:?}", token.kind),
            ));
        }

        let prefix = prefix_opt.unwrap().clone();
        let decl = prefix.parse(self, lexer, token)?;

        // expect a semicolon after declarations
        let token = lexer.next_token();
        if token.kind != TokenKind::Semicolon {
            Err(ParseError::UnexpectedToken(
                token,
                "missing semicolon".to_string(),
            ))
        } else {
            Ok(decl)
        }
    }

    pub fn parse_expression(&self, lexer: &mut Lexer) -> Result<Expr, ParseError> {
        self.statement_parser.parse_expression(lexer)
    }

    pub fn parse_block(&self, lexer: &mut Lexer) -> Result<BlockStmt, ParseError> {
        self.statement_parser.parse_block(lexer)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        AllocExpr, BinaryExpr, BinaryOp, BlockStmt, CallExpr, ConstDecl, EnumType, EnumVariant,
        Expr, ExprStmt, FunctionDecl, IdentifierExpr, IfStmt, MetaTypeExpr, Parameter, ReturnStmt,
        Stmt, StructField, StructType, TypeDecl, TypeSpec, UseDecl,
    };
    use crate::parser::lexer::Lexer;
    use crate::str_store::StrStore;
    use pretty_assertions::assert_eq;

    macro_rules! test_parse_declaration {
        ( $( $case:ident { input: $input:expr, want_var: $want_var:pat, want_value: $want_value:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let mut str_store = StrStore::new();
                    let mut lexer = Lexer::new($input, &mut str_store);
                    let parser = DeclParser::new();

                    let decl = parser.parse(&mut lexer).unwrap();
                    match decl {
                        $want_var => $want_value,
                        _ => panic!("Expected {} => {}, but got {:?}", stringify!($want_var), stringify!($want_value), decl)
                    }
                }
            )*
        }
    }

    test_parse_declaration!(
        parse_decl_single_function {
            input: r#"fn add(a, b i32) i32 {
                return a + b
            }"#,
            want_var: Decl::Function(decl),
            want_value: assert_eq!(
                decl,
                FunctionDecl {
                    name: 1,
                    params: vec![
                        Parameter {
                            name: 3,
                            type_spec: TypeSpec::Int32,
                        },
                        Parameter {
                            name: 5,
                            type_spec: TypeSpec::Int32,
                        }
                    ],
                    return_type: Some(TypeSpec::Int32),
                    body: BlockStmt {
                        statements: vec![Stmt::Return(ReturnStmt {
                            value: Some(Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::Identifier(IdentifierExpr { name: 3 })),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Identifier(IdentifierExpr { name: 5 })),
                            })),
                        })]
                    },
                }
            ),
        },
        parse_decl_function_no_params {
            input: r#"fn main() {
                print("hello")
                return
            }"#,
            want_var: Decl::Function(decl),
            want_value: assert_eq!(
                decl,
                FunctionDecl {
                    name: 1,
                    params: vec![],
                    return_type: None,
                    body: BlockStmt {
                        statements: vec![
                            Stmt::Expr(ExprStmt {
                                expr: Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr { name: 5 })),
                                    args: vec![Expr::StringLiteral(6)],
                                }),
                            }),
                            Stmt::Return(ReturnStmt { value: None }),
                        ],
                    },
                }
            ),
        },
        parse_decl_function_mixed_params {
            input: r#"fn process(x i32, msg str) bool {
                return true
            }"#,
            want_var: Decl::Function(decl),
            want_value: assert_eq!(
                decl,
                FunctionDecl {
                    name: 1,
                    params: vec![
                        Parameter {
                            name: 3,
                            type_spec: TypeSpec::Int32,
                        },
                        Parameter {
                            name: 6,
                            type_spec: TypeSpec::String,
                        },
                    ],
                    return_type: Some(TypeSpec::Bool),
                    body: BlockStmt {
                        statements: vec![Stmt::Return(ReturnStmt {
                            value: Some(Expr::BoolLiteral(true)),
                        })],
                    },
                },
            ),
        },
        parse_decl_function_no_return_type {
            input: r#"fn greet(name str) {
                print(name)
            }"#,
            want_var: Decl::Function(decl),
            want_value: assert_eq!(
                decl,
                FunctionDecl {
                    name: 1,
                    params: vec![Parameter {
                        name: 3,
                        type_spec: TypeSpec::String,
                    }],
                    return_type: None,
                    body: BlockStmt {
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr { name: 7 })),
                                args: vec![Expr::Identifier(IdentifierExpr { name: 3 })],
                            })
                        })],
                    },
                }
            ),
        },
        parse_decl_function_with_complex_body {
            input: r#"fn maybe_div(a, b i32) i32 {
                if b == 0 {
                    return 0
                }
                return a / b
            }"#,
            want_var: Decl::Function(decl),
            want_value: assert_eq!(
                decl,
                FunctionDecl {
                    name: 1,
                    params: vec![
                        Parameter {
                            name: 3,
                            type_spec: TypeSpec::Int32,
                        },
                        Parameter {
                            name: 5,
                            type_spec: TypeSpec::Int32,
                        },
                    ],
                    return_type: Some(TypeSpec::Int32),
                    body: BlockStmt {
                        statements: vec![
                            Stmt::If(IfStmt {
                                check: Box::new(Expr::Binary(BinaryExpr {
                                    left: Box::new(Expr::Identifier(IdentifierExpr { name: 5 })),
                                    operator: BinaryOp::Equal,
                                    right: Box::new(Expr::IntLiteral(0)),
                                })),
                                success: BlockStmt {
                                    statements: vec![Stmt::Return(ReturnStmt {
                                        value: Some(Expr::IntLiteral(0))
                                    })]
                                },
                                fail: None,
                            }),
                            Stmt::Return(ReturnStmt {
                                value: Some(Expr::Binary(BinaryExpr {
                                    left: Box::new(Expr::Identifier(IdentifierExpr { name: 3 })),
                                    operator: BinaryOp::Divide,
                                    right: Box::new(Expr::Identifier(IdentifierExpr { name: 5 })),
                                }))
                            }),
                        ]
                    },
                },
            ),
        },
        parse_decl_pointer_return_type {
            input: r#"fn my_alloc() unsafe::ptr {
                return alloc(@i32)
            }"#,
            want_var: Decl::Function(decl),
            want_value: assert_eq!(
                decl,
                FunctionDecl {
                    name: 1,
                    params: vec![],
                    return_type: Some(TypeSpec::Named {
                        module: Some(4),
                        name: 6,
                    }),
                    body: BlockStmt {
                        statements: vec![Stmt::Return(ReturnStmt {
                            value: Some(Expr::Alloc(AllocExpr {
                                meta_type: Box::new(Expr::MetaType(MetaTypeExpr {
                                    type_spec: TypeSpec::Int32,
                                })),
                                options: vec![],
                            })),
                        })],
                    },
                },
            ),
        },
        parse_decl_slice_parameter {
            input: r#"fn process_array(arr []i32) {
                print(arr)
            }"#,
            want_var: Decl::Function(decl),
            want_value: assert_eq!(
                decl,
                FunctionDecl {
                    name: 1,
                    params: vec![Parameter {
                        name: 3,
                        type_spec: TypeSpec::Slice(Box::new(TypeSpec::Int32))
                    }],
                    return_type: None,
                    body: BlockStmt {
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr { name: 9 })),
                                args: vec![Expr::Identifier(IdentifierExpr { name: 3 })],
                            }),
                        })],
                    },
                },
            ),
        },
        parse_decl_struct {
            input: r#"type Point struct {
    x i32
    y i32
}"#,
            want_var: Decl::Type(decl),
            want_value: assert_eq!(
                decl,
                TypeDecl {
                    name: 1,
                    type_spec: TypeSpec::Struct(StructType {
                        fields: vec![
                            StructField {
                                name: 4,
                                type_spec: TypeSpec::Int32,
                            },
                            StructField {
                                name: 7,
                                type_spec: TypeSpec::Int32,
                            },
                        ]
                    }),
                }
            ),
        },
        parse_decl_enum {
            input: r#"type Result enum {
    Ok(m::MyType)
    Error
}"#,
            want_var: Decl::Type(decl),
            want_value: assert_eq!(
                decl,
                TypeDecl {
                    name: 1,
                    type_spec: TypeSpec::Enum(EnumType {
                        variants: vec![
                            EnumVariant {
                                name: 4,
                                payload: Some(TypeSpec::Named {
                                    module: Some(6),
                                    name: 8
                                }),
                            },
                            EnumVariant {
                                name: 11,
                                payload: None,
                            },
                        ],
                    }),
                },
            ),
        },
        parse_decl_list_node {
            input: "type Node enum {\n\tSome(*Node)\n\tNone\n}",
            want_var: Decl::Type(decl),
            want_value: assert_eq!(
                decl,
                TypeDecl {
                    name: 1,
                    type_spec: TypeSpec::Enum(EnumType {
                        variants: vec![
                            EnumVariant {
                                name: 4,
                                payload: Some(TypeSpec::Pointer(Box::new(TypeSpec::Named {
                                    module: None,
                                    name: 1
                                }))),
                            },
                            EnumVariant {
                                name: 9,
                                payload: None,
                            },
                        ],
                    }),
                },
            ),
        },
        parse_decl_empty_struct {
            input: "type Empty struct{}",
            want_var: Decl::Type(decl),
            want_value: assert_eq!(
                decl,
                TypeDecl {
                    name: 1,
                    type_spec: TypeSpec::Struct(StructType { fields: vec![] }),
                },
            ),
        },
        parse_decl_const_literal {
            input: "const PI = 3.45",
            want_var: Decl::Const(decl),
            want_value: assert_eq!(
                decl,
                ConstDecl {
                    name: 1,
                    value: Expr::FloatLiteral(3.45),
                },
            ),
        },
        parse_decl_const_integer {
            input: "const MAX_SIZE = 1024",
            want_var: Decl::Const(decl),
            want_value: assert_eq!(
                decl,
                ConstDecl {
                    name: 1,
                    value: Expr::IntLiteral(1024),
                },
            ),
        },
        parse_decl_const_expression {
            input: "const DOUBLE = 5 + 15",
            want_var: Decl::Const(decl),
            want_value: assert_eq!(
                decl,
                ConstDecl {
                    name: 1,
                    value: Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::IntLiteral(5)),
                        operator: BinaryOp::Add,
                        right: Box::new(Expr::IntLiteral(15)),
                    }),
                },
            ),
        },
        parse_decl_const_string {
            input: r#"const VERSION = "1.0.0""#,
            want_var: Decl::Const(decl),
            want_value: assert_eq!(
                decl,
                ConstDecl {
                    name: 1,
                    value: Expr::StringLiteral(3),
                },
            ),
        },
        parse_decl_use_single {
            input: r#"use (
                "math"
            )"#,
            want_var: Decl::Use(decl),
            want_value: assert_eq!(decl, UseDecl { modules: vec![2] }),
        },
        parse_decl_use_multiple {
            input: r#"use (
                "std"
                "io"
                "math"
            )"#,
            want_var: Decl::Use(decl),
            want_value: assert_eq!(
                decl,
                UseDecl {
                    modules: vec![2, 4, 5],
                },
            ),
        },
    );
}
