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
                "Missing semicolon".to_string(),
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
    use pretty_assertions::assert_eq;

    macro_rules! test_parse_declaration {
        ( $( $case:ident { input: $input:expr, want_var: $want_var:pat, want_value: $want_value:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let mut lexer = Lexer::new($input);
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
                    name: "add".to_string(),
                    params: vec![
                        Parameter {
                            name: "a".to_string(),
                            type_spec: TypeSpec::Int32,
                        },
                        Parameter {
                            name: "b".to_string(),
                            type_spec: TypeSpec::Int32,
                        }
                    ],
                    return_type: Some(TypeSpec::Int32),
                    body: BlockStmt {
                        statements: vec![Stmt::Return(ReturnStmt {
                            value: Some(Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "a".to_string(),
                                })),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "b".to_string(),
                                })),
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
                    name: "main".to_string(),
                    params: vec![],
                    return_type: None,
                    body: BlockStmt {
                        statements: vec![
                            Stmt::Expr(ExprStmt {
                                expr: Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        name: "print".to_string()
                                    })),
                                    args: vec![Expr::StringLiteral("hello".to_string())],
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
                    name: "process".to_string(),
                    params: vec![
                        Parameter {
                            name: "x".to_string(),
                            type_spec: TypeSpec::Int32,
                        },
                        Parameter {
                            name: "msg".to_string(),
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
                    name: "greet".to_string(),
                    params: vec![Parameter {
                        name: "name".to_string(),
                        type_spec: TypeSpec::String,
                    }],
                    return_type: None,
                    body: BlockStmt {
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "print".to_string(),
                                })),
                                args: vec![Expr::Identifier(IdentifierExpr {
                                    name: "name".to_string(),
                                })],
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
                    name: "maybe_div".to_string(),
                    params: vec![
                        Parameter {
                            name: "a".to_string(),
                            type_spec: TypeSpec::Int32,
                        },
                        Parameter {
                            name: "b".to_string(),
                            type_spec: TypeSpec::Int32,
                        },
                    ],
                    return_type: Some(TypeSpec::Int32),
                    body: BlockStmt {
                        statements: vec![
                            Stmt::If(IfStmt {
                                check: Box::new(Expr::Binary(BinaryExpr {
                                    left: Box::new(Expr::Identifier(IdentifierExpr {
                                        name: "b".to_string(),
                                    })),
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
                                    left: Box::new(Expr::Identifier(IdentifierExpr {
                                        name: "a".to_string()
                                    })),
                                    operator: BinaryOp::Divide,
                                    right: Box::new(Expr::Identifier(IdentifierExpr {
                                        name: "b".to_string()
                                    })),
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
                    name: "my_alloc".to_string(),
                    params: vec![],
                    return_type: Some(TypeSpec::Named {
                        module: Some("unsafe".to_string()),
                        name: "ptr".to_string(),
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
                    name: "process_array".to_string(),
                    params: vec![Parameter {
                        name: "arr".to_string(),
                        type_spec: TypeSpec::Slice(Box::new(TypeSpec::Int32))
                    }],
                    return_type: None,
                    body: BlockStmt {
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    name: "print".to_string(),
                                })),
                                args: vec![Expr::Identifier(IdentifierExpr {
                                    name: "arr".to_string()
                                })],
                            }),
                        })],
                    },
                },
            ),
        },
        parse_decl_struct {
            input: "type Point struct {\n\tx i32\n\ty i32\n}",
            want_var: Decl::Type(decl),
            want_value: assert_eq!(
                decl,
                TypeDecl {
                    name: IdentifierExpr {
                        name: "Point".to_string()
                    },
                    type_spec: TypeSpec::Struct(StructType {
                        fields: vec![
                            StructField {
                                name: "x".to_string(),
                                type_spec: TypeSpec::Int32,
                            },
                            StructField {
                                name: "y".to_string(),
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
                    name: IdentifierExpr {
                        name: "Result".to_string()
                    },
                    type_spec: TypeSpec::Enum(EnumType {
                        variants: vec![
                            EnumVariant {
                                name: "Ok".to_string(),
                                payload: Some(TypeSpec::Named {
                                    module: Some("m".to_string()),
                                    name: "MyType".to_string()
                                }),
                            },
                            EnumVariant {
                                name: "Error".to_string(),
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
                    name: IdentifierExpr {
                        name: "Node".to_string(),
                    },
                    type_spec: TypeSpec::Enum(EnumType {
                        variants: vec![
                            EnumVariant {
                                name: "Some".to_string(),
                                payload: Some(TypeSpec::Pointer(Box::new(TypeSpec::Named {
                                    module: None,
                                    name: "Node".to_string()
                                }))),
                            },
                            EnumVariant {
                                name: "None".to_string(),
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
                    name: IdentifierExpr {
                        name: "Empty".to_string(),
                    },
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
                    name: "PI".to_string(),
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
                    name: "MAX_SIZE".to_string(),
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
                    name: "DOUBLE".to_string(),
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
                    name: "VERSION".to_string(),
                    value: Expr::StringLiteral("1.0.0".to_string()),
                },
            ),
        },
        parse_decl_use_single {
            input: r#"use (
                "math"
            )"#,
            want_var: Decl::Use(decl),
            want_value: assert_eq!(
                decl,
                UseDecl {
                    modules: vec!["math".to_string()],
                },
            ),
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
                    modules: vec!["std".to_string(), "io".to_string(), "math".to_string()],
                },
            ),
        },
    );
}
