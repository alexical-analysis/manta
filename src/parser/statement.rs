mod assign_statement;
mod block_statement;
mod defer_statement;
mod if_statement;
mod let_statement;
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
use defer_statement::DeferParselet;
use if_statement::IfParselet;
use let_statement::LetParselet;
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
    pattern_parser: PatternParser,
    prefix_parselets: HashMap<TokenKind, Rc<dyn PrefixStmtParselet>>,
    infix_parselets: HashMap<TokenKind, Rc<dyn InfixStmtParselet>>,
}

impl StmtParser {
    pub fn new() -> Self {
        let mut prefix_parselets: HashMap<TokenKind, Rc<dyn PrefixStmtParselet>> = HashMap::new();
        prefix_parselets.insert(TokenKind::LetKeyword, Rc::new(LetParselet));
        prefix_parselets.insert(TokenKind::ReturnKeyword, Rc::new(ReturnParselet));
        prefix_parselets.insert(TokenKind::DeferKeyword, Rc::new(DeferParselet));
        prefix_parselets.insert(TokenKind::OpenBrace, Rc::new(BlockParselet));
        prefix_parselets.insert(TokenKind::IfKeyword, Rc::new(IfParselet));
        prefix_parselets.insert(TokenKind::MatchKeyword, Rc::new(MatchParselet));

        let mut infix_parselets: HashMap<TokenKind, Rc<dyn InfixStmtParselet>> = HashMap::new();
        infix_parselets.insert(TokenKind::Equal, Rc::new(AssignParselet));

        let expr_parser = ExprParser::new();
        let pattern_parser = PatternParser::new();

        StmtParser {
            expr_parser,
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

    // TODO: should this be an infix parselet?
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
        DotAccessPat, Expr, FreeExpr, IdentifierExpr, IdentifierPat, IfStmt, IndexExpr, LetExcept,
        LetStmt, MatchArm, MatchStmt, MetaTypeExpr, ModuleAccessExpr, Pattern, PayloadPat,
        ReturnStmt, Stmt, TypeSpec, UnaryExpr, UnaryOp,
    };
    use crate::parser::lexer::Lexer;
    use crate::str_store::{self, StrID, StrStore};
    use pretty_assertions::assert_eq;

    macro_rules! test_parse_statement {
        ( $( $case:ident { input: $input:expr, want_var: $want_var:pat, want_value: $want_value:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let mut str_store = StrStore::new();
                    let mut lexer = Lexer::new($input, &mut str_store);
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
                    pattern: Pattern::Identifier(IdentifierPat {
                        id: 4,
                        name: StrID(1)
                    }),
                    value: Expr::IntLiteral(10),
                    except: LetExcept::None,
                }
            ),
        },
        parse_stmt_let_with_value {
            input: "let bool(y) = true",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::Payload(PayloadPat {
                        pat: Box::new(Pattern::Identifier(IdentifierPat {
                            id: 4,
                            name: str_store::BOOL,
                        })),
                        payload: IdentifierPat {
                            id: 9,
                            name: StrID(2)
                        },
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
                    pattern: Pattern::Identifier(IdentifierPat {
                        id: 4,
                        name: StrID(1)
                    }),
                    value: Expr::FloatLiteral(3.45),
                    except: LetExcept::None,
                },
            ),
        },
        parse_stmt_let_user_type {
            input: "let Person(jill) = new_person()",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::Payload(PayloadPat {
                        pat: Box::new(Pattern::Identifier(IdentifierPat {
                            id: 4,
                            name: StrID(1)
                        })),
                        payload: IdentifierPat {
                            id: 11,
                            name: StrID(3)
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            id: 19,
                            name: StrID(6)
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
                                    id: 7,
                                    name: StrID(1)
                                }))),
                                field: StrID(3),
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
                        id: 6,
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
                        id: 6,
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Free(FreeExpr {
                                expr: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: 13,
                                    name: StrID(4)
                                })),
                            })
                        })]
                    }
                }
            ),
        },
        parse_stmt_defer_multi {
            input: "defer {\nprint(\"done:\", err)\nalloc(@i32)\n}",
            want_var: Stmt::Defer(stmt),
            want_value: assert_eq!(
                stmt,
                DeferStmt {
                    block: BlockStmt {
                        id: 6,
                        statements: vec![
                            Stmt::Expr(ExprStmt {
                                expr: Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        id: 8,
                                        name: StrID(2)
                                    })),
                                    args: vec![
                                        Expr::StringLiteral(StrID(4)),
                                        Expr::Identifier(IdentifierExpr {
                                            id: 23,
                                            name: StrID(6)
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
                    id: 0,
                    statements: vec![
                        Stmt::Let(LetStmt {
                            pattern: Pattern::Identifier(IdentifierPat {
                                id: 10,
                                name: StrID(2)
                            }),
                            value: Expr::IntLiteral(10),
                            except: LetExcept::None,
                        }),
                        Stmt::Let(LetStmt {
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        id: 26,
                                        name: StrID(7)
                                    },
                                })),
                                payload: IdentifierPat {
                                    id: 29,
                                    name: StrID(9)
                                },
                            }),
                            value: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: 34,
                                    name: StrID(11)
                                })),
                                args: vec![Expr::IntLiteral(42)],
                            }),
                            except: LetExcept::Panic,
                        }),
                        Stmt::Let(LetStmt {
                            pattern: Pattern::Identifier(IdentifierPat {
                                id: 58,
                                name: StrID(14)
                            }),
                            value: Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: 62,
                                    name: StrID(2)
                                })),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: 66,
                                    name: StrID(9)
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
                        id: 0,
                        name: StrID(0)
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
                            id: 1,
                            name: StrID(1)
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
                        id: 0,
                        name: StrID(0)
                    }),
                    rvalue: Expr::Call(CallExpr {
                        func: Box::new(Expr::DotAccess(DotAccessExpr {
                            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                                id: 7,
                                name: StrID(2)
                            }))),
                            field: StrID(0),
                        })),
                        args: vec![
                            Expr::Identifier(IdentifierExpr {
                                id: 19,
                                name: StrID(5)
                            }),
                            Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::IntLiteral(1)),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        id: 26,
                                        name: StrID(9)
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
                            id: 0,
                            name: StrID(0)
                        })),
                        index: Box::new(Expr::IntLiteral(0)),
                    }),
                    rvalue: Expr::IntLiteral(10),
                },
            ),
        },
        parse_stmt_if {
            input: r#"if true {
    print("ok")
}"#,
            want_var: Stmt::If(stmt),
            want_value: assert_eq!(
                stmt,
                IfStmt {
                    check: Box::new(Expr::BoolLiteral(true)),
                    success: BlockStmt {
                        id: 8,
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: 14,
                                    name: StrID(3)
                                })),
                                args: vec![Expr::StringLiteral(StrID(5))],
                            })
                        })],
                    },
                    fail: None,
                },
            ),
        },
        parse_stmt_if_else {
            input: r#"if a < 13 {
    print("ok")
} else {
    a = 10 + number(3.45)
}"#,
            want_var: Stmt::If(stmt),
            want_value: assert_eq!(
                stmt,
                IfStmt {
                    check: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::Identifier(IdentifierExpr {
                            id: 3,
                            name: StrID(1)
                        })),
                        operator: BinaryOp::LessThan,
                        right: Box::new(Expr::IntLiteral(13)),
                    })),
                    success: BlockStmt {
                        id: 10,
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    id: 16,
                                    name: StrID(5)
                                })),
                                args: vec![Expr::StringLiteral(StrID(7))],
                            }),
                        })],
                    },
                    fail: Some(BlockStmt {
                        id: 35,
                        statements: vec![Stmt::Assign(AssignStmt {
                            lvalue: Expr::Identifier(IdentifierExpr {
                                id: 41,
                                name: StrID(1)
                            }),
                            rvalue: Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::IntLiteral(10)),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        id: 50,
                                        name: StrID(15),
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
                        field: StrID(2),
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
                    pattern: Pattern::Identifier(IdentifierPat {
                        id: 4,
                        name: StrID(1)
                    }),
                    value: Expr::Unary(UnaryExpr {
                        operator: UnaryOp::AddressOf,
                        operand: Box::new(Expr::Identifier(IdentifierExpr {
                            id: 12,
                            name: StrID(4)
                        })),
                    }),
                    except: LetExcept::None,
                }
            ),
        },
        parse_stmt_let_with_panic {
            input: "let .Ok = call() !",
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::DotAccess(DotAccessPat {
                        target: None,
                        field: IdentifierPat {
                            id: 5,
                            name: StrID(2)
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            id: 10,
                            name: StrID(4)
                        })),
                        args: vec![],
                    }),
                    except: LetExcept::Panic,
                },
            ),
        },
        parse_stmt_let_simple_catch {
            input: r#"let Ret.Valid(v) = validate("data") or {
    print("invalid!")
}"#,
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::Payload(PayloadPat {
                        pat: Box::new(Pattern::DotAccess(DotAccessPat {
                            target: Some(Box::new(Pattern::Identifier(IdentifierPat {
                                id: 4,
                                name: StrID(1)
                            }))),
                            field: IdentifierPat {
                                id: 8,
                                name: StrID(3)
                            },
                        })),
                        payload: IdentifierPat {
                            id: 14,
                            name: StrID(5)
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            id: 19,
                            name: StrID(8)
                        })),
                        args: vec![Expr::StringLiteral(StrID(9))],
                    }),
                    except: LetExcept::Or {
                        id: 36,
                        binding: None,
                        body: BlockStmt {
                            id: 39,
                            statements: vec![Stmt::Expr(ExprStmt {
                                expr: Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        id: 45,
                                        name: StrID(12)
                                    })),
                                    args: vec![Expr::StringLiteral(StrID(13))],
                                }),
                            })],
                        }
                    }
                },
            ),
        },
        parse_stmt_let_catch_binding {
            input: r#"let .Err = build_item(name, false) or(i) {
    print("built item")
    return i
}"#,
            want_var: Stmt::Let(stmt),
            want_value: assert_eq!(
                stmt,
                LetStmt {
                    pattern: Pattern::DotAccess(DotAccessPat {
                        target: None,
                        field: IdentifierPat {
                            id: 5,
                            name: StrID(2)
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            id: 11,
                            name: StrID(4)
                        })),
                        args: vec![
                            Expr::Identifier(IdentifierExpr {
                                id: 22,
                                name: StrID(6)
                            }),
                            Expr::BoolLiteral(false),
                        ],
                    }),
                    except: LetExcept::Or {
                        id: 35,
                        binding: Some(StrID(11)),
                        body: BlockStmt {
                            id: 41,
                            statements: vec![
                                Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: 47,
                                            name: StrID(13),
                                        })),
                                        args: vec![Expr::StringLiteral(StrID(14))],
                                    })
                                }),
                                Stmt::Return(ReturnStmt {
                                    value: Some(Expr::Identifier(IdentifierExpr {
                                        id: 78,
                                        name: StrID(11)
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
                    pattern: Pattern::Payload(PayloadPat {
                        pat: Box::new(Pattern::DotAccess(DotAccessPat {
                            target: None,
                            field: IdentifierPat {
                                id: 5,
                                name: StrID(2)
                            },
                        })),
                        payload: IdentifierPat {
                            id: 8,
                            name: StrID(4)
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            id: 13,
                            name: StrID(7)
                        })),
                        args: vec![],
                    }),
                    except: LetExcept::Wrap(Expr::DotAccess(DotAccessExpr {
                        target: None,
                        field: StrID(9),
                    })),
                },
            ),
        },
        parse_stmt_match {
            input: r#"match x {
    .Some(v) { print(v) }
    .None { print("none") }
}"#,
            want_var: Stmt::Match(stmt),
            want_value: assert_eq!(
                stmt,
                MatchStmt {
                    target: Expr::Identifier(IdentifierExpr {
                        id: 6,
                        name: StrID(1)
                    }),
                    arms: vec![
                        MatchArm {
                            id: 14,
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        id: 15,
                                        name: StrID(4)
                                    },
                                })),
                                payload: IdentifierPat {
                                    id: 20,
                                    name: StrID(6)
                                },
                            }),
                            body: BlockStmt {
                                id: 23,
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: 25,
                                            name: StrID(8),
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            id: 31,
                                            name: StrID(6)
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            id: 40,
                            pattern: Pattern::DotAccess(DotAccessPat {
                                target: None,
                                field: IdentifierPat {
                                    id: 41,
                                    name: StrID(12)
                                },
                            }),
                            body: BlockStmt {
                                id: 46,
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: 48,
                                            name: StrID(8)
                                        })),
                                        args: vec![Expr::StringLiteral(StrID(13))],
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
    .Success(val) { print(val) }
    .Warning(msg) { print(msg) }
    .Failed { print("error") }
}"#,
            want_var: Stmt::Match(stmt),
            want_value: assert_eq!(
                stmt,
                MatchStmt {
                    target: Expr::Identifier(IdentifierExpr {
                        id: 6,
                        name: StrID(1)
                    }),
                    arms: vec![
                        MatchArm {
                            id: 19,
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        id: 20,
                                        name: StrID(4)
                                    },
                                })),
                                payload: IdentifierPat {
                                    id: 28,
                                    name: StrID(6)
                                },
                            }),
                            body: BlockStmt {
                                id: 33,
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: 35,
                                            name: StrID(8)
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            id: 41,
                                            name: StrID(6)
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            id: 52,
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        id: 53,
                                        name: StrID(12)
                                    },
                                })),
                                payload: IdentifierPat {
                                    id: 61,
                                    name: StrID(13)
                                },
                            }),
                            body: BlockStmt {
                                id: 66,
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: 68,
                                            name: StrID(8)
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            id: 74,
                                            name: StrID(13)
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            id: 85,
                            pattern: Pattern::DotAccess(DotAccessPat {
                                target: None,
                                field: IdentifierPat {
                                    id: 86,
                                    name: StrID(14)
                                },
                            }),
                            body: BlockStmt {
                                id: 93,
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: 95,
                                            name: StrID(8)
                                        })),
                                        args: vec![Expr::StringLiteral(StrID(15))],
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
    .Ready { print("go") }
    .Idle(ts) { print("idle since", ts) }
}"#,
            want_var: Stmt::Match(stmt),
            want_value: assert_eq!(
                stmt,
                MatchStmt {
                    target: Expr::Identifier(IdentifierExpr {
                        id: 6,
                        name: StrID(1)
                    }),
                    arms: vec![
                        MatchArm {
                            id: 19,
                            pattern: Pattern::DotAccess(DotAccessPat {
                                target: None,
                                field: IdentifierPat {
                                    id: 20,
                                    name: StrID(4)
                                },
                            }),
                            body: BlockStmt {
                                id: 26,
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: 28,
                                            name: StrID(5),
                                        })),
                                        args: vec![Expr::StringLiteral(StrID(7))],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            id: 46,
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        id: 47,
                                        name: StrID(12)
                                    },
                                })),
                                payload: IdentifierPat {
                                    id: 52,
                                    name: StrID(13)
                                },
                            }),
                            body: BlockStmt {
                                id: 56,
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            id: 58,
                                            name: StrID(5)
                                        })),
                                        args: vec![
                                            Expr::StringLiteral(StrID(14)),
                                            Expr::Identifier(IdentifierExpr {
                                                id: 78,
                                                name: StrID(13)
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
                    expr: Expr::ModuleAccess(ModuleAccessExpr {
                        module: StrID(0),
                        expr: Box::new(Expr::Identifier(IdentifierExpr {
                            id: 5,
                            name: StrID(2)
                        })),
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
                    expr: Expr::ModuleAccess(ModuleAccessExpr {
                        module: StrID(0),
                        expr: Box::new(Expr::Call(CallExpr {
                            func: Box::new(Expr::Identifier(IdentifierExpr {
                                id: 5,
                                name: StrID(2)
                            })),
                            args: vec![Expr::StringLiteral(StrID(4))],
                        })),
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
                    expr: Expr::ModuleAccess(ModuleAccessExpr {
                        module: StrID(0),
                        expr: Box::new(Expr::Identifier(IdentifierExpr {
                            id: 6,
                            name: StrID(2)
                        })),
                    }),
                }
            ),
        },
    );
}
