mod assign_statement;
mod block_statement;
mod control_flow_statement;
mod defer_statement;
mod if_statement;
mod let_statement;
mod loop_statement;
mod match_statement;
mod return_statement;

use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BlockStmt, Expr, ExprStmt, Pattern, Stmt};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, Precedence};
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::PatternParser;

use assign_statement::AssignParselet;
use block_statement::BlockParselet;
use control_flow_statement::{BreakParselet, ContinueParselet};
use defer_statement::DeferParselet;
use if_statement::IfParselet;
use let_statement::LetParselet;
use loop_statement::{ForParselet, LoopParselet, WhileParselet};
use match_statement::MatchParselet;
use return_statement::ReturnParselet;

/// Trait for prefix statement parselets.
pub trait PrefixStmtParselet {
    /// Parse a prefix statement given the consumed token.
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Stmt, ParseError>;
}

/// Trait for infix statement parselets.
pub trait InfixStmtParselet {
    /// Parse an infix statement with `left` already parsed and the consumed token.
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        left: Expr,
        token: Token,
    ) -> Result<Stmt, ParseError>;
}

pub struct StmtParser {
    expr_parser: ExprParser,
    no_struct_expr_parser: ExprParser,
    pattern_parser: PatternParser,
    prefix_parselets: HashMap<TokenKind, Rc<dyn PrefixStmtParselet>>,
    infix_parselets: HashMap<TokenKind, Rc<dyn InfixStmtParselet>>,
}

impl StmtParser {
    pub fn new() -> Self {
        let mut prefix_parselets: HashMap<TokenKind, Rc<dyn PrefixStmtParselet>> = HashMap::new();
        prefix_parselets.insert(
            TokenKind::LetKeyword,
            Rc::new(LetParselet {
                mutable_binding: false,
            }),
        );
        prefix_parselets.insert(
            TokenKind::MutKeyword,
            Rc::new(LetParselet {
                mutable_binding: true,
            }),
        );
        prefix_parselets.insert(TokenKind::ReturnKeyword, Rc::new(ReturnParselet));
        prefix_parselets.insert(TokenKind::DeferKeyword, Rc::new(DeferParselet));
        prefix_parselets.insert(TokenKind::OpenBrace, Rc::new(BlockParselet));
        prefix_parselets.insert(TokenKind::IfKeyword, Rc::new(IfParselet));
        prefix_parselets.insert(TokenKind::MatchKeyword, Rc::new(MatchParselet));
        prefix_parselets.insert(TokenKind::LoopKeyword, Rc::new(LoopParselet));
        prefix_parselets.insert(TokenKind::WhileKeyword, Rc::new(WhileParselet));
        prefix_parselets.insert(TokenKind::ForKeyword, Rc::new(ForParselet));
        prefix_parselets.insert(TokenKind::BreakKeyword, Rc::new(BreakParselet));
        prefix_parselets.insert(TokenKind::ContinueKey, Rc::new(ContinueParselet));

        let mut infix_parselets: HashMap<TokenKind, Rc<dyn InfixStmtParselet>> = HashMap::new();
        infix_parselets.insert(TokenKind::Equal, Rc::new(AssignParselet));

        let expr_parser = ExprParser::new_parse_structs();
        let no_struct_expr_parser = ExprParser::new_no_structs();
        let pattern_parser = PatternParser::new();

        StmtParser {
            expr_parser,
            no_struct_expr_parser,
            pattern_parser,
            prefix_parselets,
            infix_parselets,
        }
    }

    /// Parse manta statements
    pub fn parse(&self, lexer: &mut Lexer) -> Result<Stmt, ParseError> {
        // need to peek here because we don't know if this is an expression or a statement yet
        let token = lexer.peek();

        let parselet = self.prefix_parselets.get(&token.kind);
        if let Some(parselet) = parselet {
            let parselet = parselet.clone();
            let token = lexer.next_token();
            let stmt = parselet.parse(self, lexer, token)?;

            // Consume trailing semicolon for prefix statements
            let next = lexer.next_token();
            if next.kind != TokenKind::Semicolon {
                return Err(ParseError::UnexpectedToken(
                    token,
                    "missing ';'".to_string(),
                ));
            }

            return Ok(stmt);
        };

        // if we failed to match a statment, parse this as expression instead
        let expr = self.expr_parser.parse(lexer, Precedence::Base)?;

        let token = lexer.peek();

        // check if this expression is actually the left hand side of a statement
        let parselet = self.infix_parselets.get(&token.kind);
        match parselet {
            Some(parselet) => {
                let parselet = parselet.clone();
                let token = lexer.next_token();
                let stmt = parselet.parse(self, lexer, expr, token)?;

                let next = lexer.next_token();
                if next.kind != TokenKind::Semicolon {
                    return Err(ParseError::UnexpectedToken(
                        token,
                        "missing ';'".to_string(),
                    ));
                }

                Ok(stmt)
            }
            None => {
                let next = lexer.next_token();
                if next.kind != TokenKind::Semicolon {
                    return Err(ParseError::UnexpectedToken(next, "missing ';'".to_string()));
                }

                Ok(Stmt::Expr(ExprStmt { expr }))
            }
        }
    }

    pub fn parse_block(&self, lexer: &mut Lexer, token: Token) -> Result<BlockStmt, ParseError> {
        let mut statements = vec![];

        loop {
            let next = lexer.peek();
            match next.kind {
                TokenKind::CloseBrace => {
                    lexer.next_token();
                    break;
                }
                TokenKind::Eof => {
                    return Err(ParseError::UnexpectedToken(
                        next,
                        "missing closing '}' in block".to_string(),
                    ));
                }
                _ => {
                    let stmt = self.parse(lexer)?;
                    statements.push(stmt);
                }
            };
        }

        Ok(BlockStmt {
            id: token.source_id,
            statements,
        })
    }

    pub fn parse_expression(&self, lexer: &mut Lexer) -> Result<Expr, ParseError> {
        self.expr_parser.parse(lexer, Precedence::Base)
    }

    pub fn parse_no_struct_expression(&self, lexer: &mut Lexer) -> Result<Expr, ParseError> {
        self.no_struct_expr_parser.parse(lexer, Precedence::Base)
    }

    pub fn parse_pattern(&self, lexer: &mut Lexer) -> Result<Pattern, ParseError> {
        self.pattern_parser.parse(lexer)
    }

    pub fn is_expression_prefix(&self, token: Token) -> bool {
        self.expr_parser.is_expression_prefix(token)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{
        AllocExpr, AssignStmt, BinaryExpr, BinaryOp, BlockStmt, CallExpr, DeferStmt, DotAccessExpr,
        EnumVariantPat, Expr, FreeExpr, IdentifierExpr, IdentifierPat, IfStmt, IndexExpr,
        LetExcept, LetStmt, MatchArm, MatchStmt, MetaTypeExpr, NamedType, Pattern, Payload,
        ReturnStmt, Stmt, TypeSpec, TypeSpecPat, UnaryExpr, UnaryOp,
    };
    use crate::parser::lexer::{Lexer, SourceID};
    use crate::str_store::{StrID, StrStore};
    use pretty_assertions::assert_eq;

    macro_rules! test_parse_statement {
        ( $( $case:ident { input: $input:expr, want_var: $want_var:pat, want_value: $want_value:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let mut str_store = StrStore::new();
                    let mut lexer = Lexer::new($input, &mut str_store, 0);
                    let parser = StmtParser::new();

                    let stmt = parser.parse(&mut lexer).unwrap();
                    match stmt {
                        $want_var => $want_value,
                        _ => panic!("Expected {} => {}, but got {:?}", stringify!($want_var), stringify!($want_value), stmt),
                    }
                }
            )*
        }
    }

    test_parse_statement!(
        parse_stmt_let {
            input: "let x = 10",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    mutable: false,
                    pattern: Pattern::Identifier(IdentifierPat {
                        id: SourceID::from_usize(4),
                        name: StrID::from_usize(1)
                    }),
                    value: Expr::IntLiteral(10),
                    except: LetExcept::None,
                }
            ),
        },
        parse_stmt_let_with_value {
            input: "mut bool(y) = true",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    mutable: true,
                    pattern: Pattern::TypeSpec(TypeSpecPat {
                        id: SourceID::from_usize(4),
                        type_spec: TypeSpec::Bool,
                        payload: Payload::Some(StrID::from_usize(2)),
                    }),
                    value: Expr::BoolLiteral(true),
                    except: LetExcept::None,
                },
            ),
        },
        parse_stmt_let_no_type {
            input: "let pi = 3.45",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    mutable: false,
                    pattern: Pattern::Identifier(IdentifierPat {
                        id: SourceID::from_usize(4),
                        name: StrID::from_usize(1)
                    }),
                    value: Expr::FloatLiteral(3.45),
                    except: LetExcept::None,
                },
            ),
        },
        parse_stmt_let_user_type {
            input: "mut Person(jill) = new_person()",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    mutable: true,
                    pattern: Pattern::TypeSpec(TypeSpecPat {
                        id: SourceID::from_usize(4),
                        type_spec: TypeSpec::Named(NamedType {
                            id: SourceID::from_usize(4),
                            module: None,
                            name: StrID::from_usize(1)
                        }),
                        payload: Payload::Some(StrID::from_usize(3)),
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            id: SourceID::from_usize(19),
                            module: None,
                            name: StrID::from_usize(6)
                        })),
                        args: vec![],
                    }),
                    except: LetExcept::None,
                },
            ),
        },
        parse_stmt_return {
            input: "return",
            want_var: Stmt::Return(stmt),
            want_value: assert_eq!(stmt, ReturnStmt { value: None }),
        },
        parse_stmt_return_value {
            input: "return 10",
            want_var: Stmt::Return(stmt),
            want_value: assert_eq!(
                stmt,
                ReturnStmt {
                    value: Some(Expr::IntLiteral(10))
                }
            ),
        },
        parse_stmt_return_complex_value {
            input: "return builder.string(true)[0]",
            want_var: Stmt::Return(stmt),
            want_value: assert_eq!(
                stmt,
                ReturnStmt {
                    value: Some(Expr::Index(IndexExpr {
                        target: Box::new(Expr::Call(CallExpr {
                            func: Box::new(Expr::DotAccess(DotAccessExpr {
                                target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                                    id: SourceID::from_usize(7),
                                    module: None,
                                    name: StrID::from_usize(1)
                                }))),
                                field: StrID::from_usize(3),
                            })),
                            args: vec![Expr::BoolLiteral(true)],
                        })),
                        index: Box::new(Expr::IntLiteral(0)),
                    }))
                }
            ),
        },
        parse_stmt_defer {
            input: "defer {}",
            want_var: Stmt::Defer(stmt),
            want_value: assert_eq!(
                stmt,
                DeferStmt {
                    block: BlockStmt {
                        id: SourceID::from_usize(6),
                        statements: vec![]
                    }
                }
            ),
        },
        parse_stmt_defer_free {
            input: "defer {\nfree(ptr)\n}",
            want_var: Stmt::Defer(stmt),
            want_value: assert_eq!(
                stmt,
                DeferStmt {
                    block: BlockStmt {
                        id: SourceID::from_usize(6),
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Free(FreeExpr {
                                expr: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: SourceID::from_usize(13),
                                    module: None,
                                    name: StrID::from_usize(3)
                                })),
                            })
                        })]
                    }
                }
            ),
        },
        parse_stmt_defer_multi {
            input: "defer {\nprintln(\"done:\", err)\nalloc(@i32)\n}",
            want_var: Stmt::Defer(stmt),
            want_value: assert_eq!(
                stmt,
                DeferStmt {
                    block: BlockStmt {
                        id: SourceID::from_usize(6),
                        statements: vec![
                            Stmt::Expr(ExprStmt {
                                expr: Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        id: SourceID::from_usize(8),
                                        module: None,
                                        name: StrID::from_usize(2)
                                    })),
                                    args: vec![
                                        Expr::StringLiteral(StrID::from_usize(4)),
                                        Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(25),
                                            module: None,
                                            name: StrID::from_usize(6)
                                        })
                                    ],
                                })
                            }),
                            Stmt::Expr(ExprStmt {
                                expr: Expr::Alloc(AllocExpr {
                                    meta_type: Box::new(Expr::MetaType(MetaTypeExpr {
                                        type_spec: TypeSpec::Int32,
                                    })),
                                    options: vec![],
                                })
                            }),
                        ],
                    },
                },
            ),
        },
        parse_stmt_block {
            input: r#"{
    let a = 10
    let .Ok(b) = maybe_int(42) !
    let c = a + b
}"#,
            want_var: Stmt::Block(stmt),
            want_value: assert_eq!(
                stmt,
                BlockStmt {
                    id: SourceID::from_usize(0),
                    statements: vec![
                        Stmt::Let(LetStmt {
                            mutable: false,
                            pattern: Pattern::Identifier(IdentifierPat {
                                id: SourceID::from_usize(10),
                                name: StrID::from_usize(2)
                            }),
                            value: Expr::IntLiteral(10),
                            except: LetExcept::None,
                        }),
                        Stmt::Let(LetStmt {
                            mutable: false,
                            pattern: Pattern::EnumVariant(EnumVariantPat {
                                id: SourceID::from_usize(25),
                                enum_name: None,
                                variant: StrID::from_usize(7),
                                payload: Payload::Some(StrID::from_usize(9)),
                            }),
                            value: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: SourceID::from_usize(34),
                                    module: None,
                                    name: StrID::from_usize(11)
                                })),
                                args: vec![Expr::IntLiteral(42)],
                            }),
                            except: LetExcept::Panic,
                        }),
                        Stmt::Let(LetStmt {
                            mutable: false,
                            pattern: Pattern::Identifier(IdentifierPat {
                                id: SourceID::from_usize(58),
                                name: StrID::from_usize(14)
                            }),
                            value: Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: SourceID::from_usize(62),
                                    module: None,
                                    name: StrID::from_usize(2)
                                })),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: SourceID::from_usize(66),
                                    module: None,
                                    name: StrID::from_usize(9)
                                })),
                            }),
                            except: LetExcept::None,
                        }),
                    ]
                }
            ),
        },
        parse_stmt_assign {
            input: "x = 10",
            want_var: Stmt::Assign(stmt),
            want_value: assert_eq!(
                stmt,
                AssignStmt {
                    lvalue: Expr::Identifier(IdentifierExpr {
                        id: SourceID::from_usize(0),
                        module: None,
                        name: StrID::from_usize(0)
                    }),
                    rvalue: Expr::IntLiteral(10),
                },
            ),
        },
        parse_stmt_ptr_assign {
            input: "*p = 42",
            want_var: Stmt::Assign(stmt),
            want_value: assert_eq!(
                stmt,
                AssignStmt {
                    lvalue: Expr::Unary(UnaryExpr {
                        operator: UnaryOp::Dereference,
                        operand: Box::new(Expr::Identifier(IdentifierExpr {
                            id: SourceID::from_usize(1),
                            module: None,
                            name: StrID::from_usize(1)
                        })),
                    }),
                    rvalue: Expr::IntLiteral(42),
                },
            ),
        },
        parse_stmt_assign_call {
            input: "name = person.name(a, 1 + two())",
            want_var: Stmt::Assign(stmt),
            want_value: assert_eq!(
                stmt,
                AssignStmt {
                    lvalue: Expr::Identifier(IdentifierExpr {
                        id: SourceID::from_usize(0),
                        module: None,
                        name: StrID::from_usize(0)
                    }),
                    rvalue: Expr::Call(CallExpr {
                        func: Box::new(Expr::DotAccess(DotAccessExpr {
                            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                                id: SourceID::from_usize(7),
                                module: None,
                                name: StrID::from_usize(2)
                            }))),
                            field: StrID::from_usize(0),
                        })),
                        args: vec![
                            Expr::Identifier(IdentifierExpr {
                                id: SourceID::from_usize(19),
                                module: None,
                                name: StrID::from_usize(5)
                            }),
                            Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::IntLiteral(1)),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        id: SourceID::from_usize(26),
                                        module: None,
                                        name: StrID::from_usize(9)
                                    })),
                                    args: vec![],
                                })),
                            })
                        ],
                    })
                }
            ),
        },
        parse_stmt_assign_array {
            input: "a[0] = 10",
            want_var: Stmt::Assign(stmt),
            want_value: assert_eq!(
                stmt,
                AssignStmt {
                    lvalue: Expr::Index(IndexExpr {
                        target: Box::new(Expr::Identifier(IdentifierExpr {
                            id: SourceID::from_usize(0),
                            module: None,
                            name: StrID::from_usize(0)
                        })),
                        index: Box::new(Expr::IntLiteral(0)),
                    }),
                    rvalue: Expr::IntLiteral(10),
                },
            ),
        },
        parse_stmt_if {
            input: r#"if true {
    println("ok")
}"#,
            want_var: Stmt::If(stmt),
            want_value: assert_eq!(
                stmt,
                IfStmt {
                    check: Box::new(Expr::BoolLiteral(true)),
                    success: BlockStmt {
                        id: SourceID::from_usize(8),
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: SourceID::from_usize(14),
                                    module: None,
                                    name: StrID::from_usize(3)
                                })),
                                args: vec![Expr::StringLiteral(StrID::from_usize(5))],
                            })
                        })],
                    },
                    fail: None,
                },
            ),
        },
        parse_stmt_if_else {
            input: r#"if a < 13 {
    println("ok")
} else {
    a = 10 + number(3.45)
}"#,
            want_var: Stmt::If(stmt),
            want_value: assert_eq!(
                stmt,
                IfStmt {
                    check: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::Identifier(IdentifierExpr {
                            id: SourceID::from_usize(3),
                            module: None,
                            name: StrID::from_usize(1)
                        })),
                        operator: BinaryOp::LessThan,
                        right: Box::new(Expr::IntLiteral(13)),
                    })),
                    success: BlockStmt {
                        id: SourceID::from_usize(10),
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: SourceID::from_usize(16),
                                    module: None,
                                    name: StrID::from_usize(5)
                                })),
                                args: vec![Expr::StringLiteral(StrID::from_usize(7))],
                            }),
                        })],
                    },
                    fail: Some(BlockStmt {
                        id: SourceID::from_usize(37),
                        statements: vec![Stmt::Assign(AssignStmt {
                            lvalue: Expr::Identifier(IdentifierExpr {
                                id: SourceID::from_usize(43),
                                module: None,
                                name: StrID::from_usize(1)
                            }),
                            rvalue: Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::IntLiteral(10)),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        id: SourceID::from_usize(52),
                                        module: None,
                                        name: StrID::from_usize(15),
                                    })),
                                    args: vec![Expr::FloatLiteral(3.45)],
                                })),
                            })
                        }),]
                    })
                }
            ),
        },
        parse_stmt_return_enum_variant {
            input: "return .Ok",
            want_var: Stmt::Return(stmt),
            want_value: assert_eq!(
                stmt,
                ReturnStmt {
                    value: Some(Expr::DotAccess(DotAccessExpr {
                        target: None,
                        field: StrID::from_usize(2),
                    })),
                }
            ),
        },
        parse_stmt_let_address_of {
            input: "let addr = &v",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    mutable: false,
                    pattern: Pattern::Identifier(IdentifierPat {
                        id: SourceID::from_usize(4),
                        name: StrID::from_usize(1)
                    }),
                    value: Expr::Unary(UnaryExpr {
                        operator: UnaryOp::AddressOf,
                        operand: Box::new(Expr::Identifier(IdentifierExpr {
                            id: SourceID::from_usize(12),
                            module: None,
                            name: StrID::from_usize(4)
                        })),
                    }),
                    except: LetExcept::None,
                }
            ),
        },
        parse_stmt_let_with_panic {
            input: "mut .Ok = call() !",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    mutable: true,
                    pattern: Pattern::EnumVariant(EnumVariantPat {
                        id: SourceID::from_usize(4),
                        enum_name: None,
                        variant: StrID::from_usize(2),
                        payload: Payload::None,
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            id: SourceID::from_usize(10),
                            module: None,
                            name: StrID::from_usize(4)
                        })),
                        args: vec![],
                    }),
                    except: LetExcept::Panic,
                },
            ),
        },
        parse_stmt_let_simple_catch {
            input: r#"let Ret.Valid(v) = validate("data") or {
    println("invalid!")
}"#,
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    mutable: false,
                    pattern: Pattern::EnumVariant(EnumVariantPat {
                        id: SourceID::from_usize(7),
                        enum_name: Some(IdentifierExpr {
                            id: SourceID::from_usize(4),
                            module: None,
                            name: StrID::from_usize(1),
                        }),
                        variant: StrID::from_usize(3),
                        payload: Payload::Some(StrID::from_usize(5)),
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            id: SourceID::from_usize(19),
                            module: None,
                            name: StrID::from_usize(8)
                        })),
                        args: vec![Expr::StringLiteral(StrID::from_usize(9))],
                    }),
                    except: LetExcept::Or {
                        id: SourceID::from_usize(36),
                        binding: None,
                        body: BlockStmt {
                            id: SourceID::from_usize(39),
                            statements: vec![Stmt::Expr(ExprStmt {
                                expr: Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        id: SourceID::from_usize(45),
                                        module: None,
                                        name: StrID::from_usize(12)
                                    })),
                                    args: vec![Expr::StringLiteral(StrID::from_usize(13))],
                                }),
                            })],
                        }
                    }
                },
            ),
        },
        parse_stmt_let_catch_binding {
            input: r#"let .Err = build_item(name, false) or(i) {
    println("built item")
    return i
}"#,
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    mutable: false,
                    pattern: Pattern::EnumVariant(EnumVariantPat {
                        id: SourceID::from_usize(4),
                        enum_name: None,
                        variant: StrID::from_usize(2),
                        payload: Payload::None,
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            id: SourceID::from_usize(11),
                            module: None,
                            name: StrID::from_usize(4)
                        })),
                        args: vec![
                            Expr::Identifier(IdentifierExpr {
                                id: SourceID::from_usize(22),
                                module: None,
                                name: StrID::from_usize(6)
                            }),
                            Expr::BoolLiteral(false),
                        ],
                    }),
                    except: LetExcept::Or {
                        id: SourceID::from_usize(35),
                        binding: Some(StrID::from_usize(11)),
                        body: BlockStmt {
                            id: SourceID::from_usize(41),
                            statements: vec![
                                Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(47),
                                            module: None,
                                            name: StrID::from_usize(13),
                                        })),
                                        args: vec![Expr::StringLiteral(StrID::from_usize(14))],
                                    })
                                }),
                                Stmt::Return(ReturnStmt {
                                    value: Some(Expr::Identifier(IdentifierExpr {
                                        id: SourceID::from_usize(80),
                                        module: None,
                                        name: StrID::from_usize(11)
                                    })),
                                }),
                            ],
                        },
                    }
                }
            ),
        },
        parse_stmt_let_with_wrap {
            input: "let .Ok(d) = div() wrap .Err",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    mutable: false,
                    pattern: Pattern::EnumVariant(EnumVariantPat {
                        id: SourceID::from_usize(4),
                        enum_name: None,
                        variant: StrID::from_usize(2),
                        payload: Payload::Some(StrID::from_usize(4)),
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            id: SourceID::from_usize(13),
                            module: None,
                            name: StrID::from_usize(7)
                        })),
                        args: vec![],
                    }),
                    except: LetExcept::Wrap(Expr::DotAccess(DotAccessExpr {
                        target: None,
                        field: StrID::from_usize(9),
                    })),
                },
            ),
        },
        parse_stmt_match {
            input: r#"match x {
    .Some(v) { println(v) }
    .None { println("none") }
}"#,
            want_var: Stmt::Match(stmt),
            want_value: assert_eq!(
                stmt,
                MatchStmt {
                    target: Expr::Identifier(IdentifierExpr {
                        id: SourceID::from_usize(6),
                        module: None,
                        name: StrID::from_usize(1)
                    }),
                    arms: vec![
                        MatchArm {
                            id: SourceID::from_usize(14),
                            pattern: Pattern::EnumVariant(EnumVariantPat {
                                id: SourceID::from_usize(14),
                                enum_name: None,
                                variant: StrID::from_usize(4),
                                payload: Payload::Some(StrID::from_usize(6)),
                            }),
                            body: BlockStmt {
                                id: SourceID::from_usize(23),
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(25),
                                            module: None,
                                            name: StrID::from_usize(8),
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(33),
                                            module: None,
                                            name: StrID::from_usize(6)
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            id: SourceID::from_usize(42),
                            pattern: Pattern::EnumVariant(EnumVariantPat {
                                id: SourceID::from_usize(42),
                                enum_name: None,
                                variant: StrID::from_usize(12),
                                payload: Payload::None,
                            }),
                            body: BlockStmt {
                                id: SourceID::from_usize(48),
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(50),
                                            module: None,
                                            name: StrID::from_usize(8)
                                        })),
                                        args: vec![Expr::StringLiteral(StrID::from_usize(13))],
                                    }),
                                })],
                            },
                        },
                    ],
                }
            ),
        },
        parse_stmt_match_mixed_patterns {
            input: r#"match result {
    .Success(val) { println(val) }
    .Warning(msg) { println(msg) }
    .Failed { println("error") }
}"#,
            want_var: Stmt::Match(stmt),
            want_value: assert_eq!(
                stmt,
                MatchStmt {
                    target: Expr::Identifier(IdentifierExpr {
                        id: SourceID::from_usize(6),
                        module: None,
                        name: StrID::from_usize(1)
                    }),
                    arms: vec![
                        MatchArm {
                            id: SourceID::from_usize(19),
                            pattern: Pattern::EnumVariant(EnumVariantPat {
                                id: SourceID::from_usize(19),
                                enum_name: None,
                                variant: StrID::from_usize(4),
                                payload: Payload::Some(StrID::from_usize(6)),
                            }),
                            body: BlockStmt {
                                id: SourceID::from_usize(33),
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(35),
                                            module: None,
                                            name: StrID::from_usize(8)
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(43),
                                            module: None,
                                            name: StrID::from_usize(6)
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            id: SourceID::from_usize(54),
                            pattern: Pattern::EnumVariant(EnumVariantPat {
                                id: SourceID::from_usize(54),
                                enum_name: None,
                                variant: StrID::from_usize(12),
                                payload: Payload::Some(StrID::from_usize(13)),
                            }),
                            body: BlockStmt {
                                id: SourceID::from_usize(68),
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(70),
                                            module: None,
                                            name: StrID::from_usize(8)
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(78),
                                            module: None,
                                            name: StrID::from_usize(13)
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            id: SourceID::from_usize(89),
                            pattern: Pattern::EnumVariant(EnumVariantPat {
                                id: SourceID::from_usize(89),
                                enum_name: None,
                                variant: StrID::from_usize(14),
                                payload: Payload::None,
                            }),
                            body: BlockStmt {
                                id: SourceID::from_usize(97),
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(99),
                                            module: None,
                                            name: StrID::from_usize(8)
                                        })),
                                        args: vec![Expr::StringLiteral(StrID::from_usize(15))],
                                    }),
                                })],
                            },
                        },
                    ],
                }
            ),
        },
        parse_stmt_match_empty_payload {
            input: r#"match signal {
    .Ready { println("go") }
    .Idle(ts) { println("idle since", ts) }
}"#,
            want_var: Stmt::Match(stmt),
            want_value: assert_eq!(
                stmt,
                MatchStmt {
                    target: Expr::Identifier(IdentifierExpr {
                        id: SourceID::from_usize(6),
                        module: None,
                        name: StrID::from_usize(1)
                    }),
                    arms: vec![
                        MatchArm {
                            id: SourceID::from_usize(19),
                            pattern: Pattern::EnumVariant(EnumVariantPat {
                                id: SourceID::from_usize(19),
                                enum_name: None,
                                variant: StrID::from_usize(4),
                                payload: Payload::None,
                            }),
                            body: BlockStmt {
                                id: SourceID::from_usize(26),
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(28),
                                            module: None,
                                            name: StrID::from_usize(5),
                                        })),
                                        args: vec![Expr::StringLiteral(StrID::from_usize(7))],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            id: SourceID::from_usize(48),
                            pattern: Pattern::EnumVariant(EnumVariantPat {
                                id: SourceID::from_usize(48),
                                enum_name: None,
                                variant: StrID::from_usize(12),
                                payload: Payload::Some(StrID::from_usize(13)),
                            }),
                            body: BlockStmt {
                                id: SourceID::from_usize(58),
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: SourceID::from_usize(60),
                                            module: None,
                                            name: StrID::from_usize(5)
                                        })),
                                        args: vec![
                                            Expr::StringLiteral(StrID::from_usize(14)),
                                            Expr::Identifier(IdentifierExpr {
                                                id: SourceID::from_usize(82),
                                                module: None,
                                                name: StrID::from_usize(13)
                                            }),
                                        ],
                                    }),
                                })],
                            },
                        },
                    ],
                }
            ),
        },
        parse_stmt_module_access_identifier {
            input: "fmt::println",
            want_var: Stmt::Expr(stmt),
            want_value: assert_eq!(
                stmt,
                ExprStmt {
                    expr: Expr::Identifier(IdentifierExpr {
                        id: SourceID::from_usize(0),
                        module: Some(StrID::from_usize(0)),
                        name: StrID::from_usize(2)
                    }),
                }
            ),
        },
        parse_stmt_module_access_call {
            input: "fmt::println(\"hello\")",
            want_var: Stmt::Expr(stmt),
            want_value: assert_eq!(
                stmt,
                ExprStmt {
                    expr: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            id: SourceID::from_usize(0),
                            module: Some(StrID::from_usize(0)),
                            name: StrID::from_usize(2)
                        })),
                        args: vec![Expr::StringLiteral(StrID::from_usize(4))],
                    }),
                }
            ),
        },
        parse_stmt_module_access_type {
            input: "math::vec3",
            want_var: Stmt::Expr(stmt),
            want_value: assert_eq!(
                stmt,
                ExprStmt {
                    expr: Expr::Identifier(IdentifierExpr {
                        id: SourceID::from_usize(0),
                        module: Some(StrID::from_usize(0)),
                        name: StrID::from_usize(2)
                    }),
                }
            ),
        },
    );
}
