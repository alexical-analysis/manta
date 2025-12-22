use crate::ast::Decl;
use crate::parser::{ParseError, Parser};

/// Parse a top level declration for a manta program.
pub fn parse_declaration(parser: &mut Parser) -> Result<Decl, ParseError> {
    let token = parser.consume()?;

    let prefix_opt = parser.prefix_decl_parselets.get(&token.kind);
    if prefix_opt.is_none() {
        return Err(ParseError::UnexpectedToken(format!(
            "Unexpected token at top level: {:?}",
            token.kind
        )));
    }

    let prefix = prefix_opt.unwrap().clone();
    let decl = prefix.parse(parser, token)?;

    Ok(decl)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        BinaryExpr, BinaryOp, BlockStmt, CallExpr, Expr, ExprStmt, FunctionDecl, IdentifierExpr,
        IfStmt, NewExpr, Parameter, ReturnStmt, Stmt, TypeSpec,
    };
    use crate::parser::lexer::Lexer;
    use pretty_assertions::assert_eq;

    macro_rules! test_parse_program {
        ( $( $case:ident { input: $input:expr, want_var: $want_var:pat, want_value: $want_value:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let lexer = Lexer::new($input);
                    let mut parser = Parser::new(lexer);

                    let decl = parse_declaration(&mut parser).unwrap();
                    match decl {
                        $want_var => $want_value,
                        _ => panic!("Expected {} => {}, but got {:?}", stringify!($want_var), stringify!($want_value), decl)
                    }
                }
            )*
        }
    }

    test_parse_program!(
        parse_program_single_function {
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
        parse_program_function_no_params {
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
        parse_program_function_mixed_params {
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
        parse_program_function_no_return_type {
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
        parse_program_function_with_complex_body {
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
        parse_program_pointer_return_type {
            input: r#"fn alloc() *i32 {
                return new(i32)
            }"#,
            want_var: Decl::Function(decl),
            want_value: assert_eq!(
                decl,
                FunctionDecl {
                    name: "alloc".to_string(),
                    params: vec![],
                    return_type: Some(TypeSpec::Pointer(Box::new(TypeSpec::Int32))),
                    body: BlockStmt {
                        statements: vec![Stmt::Return(ReturnStmt {
                            value: Some(Expr::New(NewExpr {
                                type_spec: TypeSpec::Int32,
                                len: None,
                                cap: None
                            })),
                        })],
                    },
                },
            ),
        },
        parse_program_slice_parameter {
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
    );
}
