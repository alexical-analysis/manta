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

        Ok(BlockStmt { token, statements })
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
    use crate::str_store::StrStore;
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
                        token: Token {
                            kind: TokenKind::Identifier,
                            source_id: 4,
                            lexeme_id: 1
                        },
                        name: 1
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
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 4,
                                lexeme_id: 18446744073709551603,
                            },
                            name: 18446744073709551603
                        })),
                        payload: IdentifierPat {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 9,
                                lexeme_id: 2
                            },
                            name: 2
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
                        token: Token {
                            kind: TokenKind::Identifier,
                            source_id: 4,
                            lexeme_id: 1
                        },
                        name: 1
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
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 4,
                                lexeme_id: 1
                            },
                            name: 1
                        })),
                        payload: IdentifierPat {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 11,
                                lexeme_id: 3
                            },
                            name: 3
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 19,
                                lexeme_id: 6
                            },
                            name: 6
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
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 7,
                                        lexeme_id: 1
                                    },
                                    name: 1
                                }))),
                                field: 3,
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
                        token: Token {
                            kind: TokenKind::OpenBrace,
                            source_id: 6,
                            lexeme_id: 1,
                        },
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
                        token: Token {
                            kind: TokenKind::OpenBrace,
                            source_id: 6,
                            lexeme_id: 1,
                        },
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Free(FreeExpr {
                                expr: Box::new(Expr::Identifier(IdentifierExpr {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 13,
                                        lexeme_id: 4,
                                    },
                                    name: 4
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
                        token: Token {
                            kind: TokenKind::OpenBrace,
                            source_id: 6,
                            lexeme_id: 1,
                        },
                        statements: vec![
                            Stmt::Expr(ExprStmt {
                                expr: Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        token: Token {
                                            kind: TokenKind::Identifier,
                                            source_id: 8,
                                            lexeme_id: 2,
                                        },
                                        name: 2
                                    })),
                                    args: vec![
                                        Expr::StringLiteral(4),
                                        Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 23,
                                                lexeme_id: 6,
                                            },
                                            name: 6
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
                    token: Token {
                        kind: TokenKind::OpenBrace,
                        source_id: 0,
                        lexeme_id: 0
                    },
                    statements: vec![
                        Stmt::Let(LetStmt {
                            pattern: Pattern::Identifier(IdentifierPat {
                                token: Token {
                                    kind: TokenKind::Identifier,
                                    source_id: 10,
                                    lexeme_id: 2
                                },
                                name: 2
                            }),
                            value: Expr::IntLiteral(10),
                            except: LetExcept::None,
                        }),
                        Stmt::Let(LetStmt {
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        token: Token {
                                            kind: TokenKind::Identifier,
                                            source_id: 26,
                                            lexeme_id: 7
                                        },
                                        name: 7
                                    },
                                })),
                                payload: IdentifierPat {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 29,
                                        lexeme_id: 9
                                    },
                                    name: 9
                                },
                            }),
                            value: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 34,
                                        lexeme_id: 11,
                                    },
                                    name: 11
                                })),
                                args: vec![Expr::IntLiteral(42)],
                            }),
                            except: LetExcept::Panic,
                        }),
                        Stmt::Let(LetStmt {
                            pattern: Pattern::Identifier(IdentifierPat {
                                token: Token {
                                    kind: TokenKind::Identifier,
                                    source_id: 58,
                                    lexeme_id: 14
                                },
                                name: 14
                            }),
                            value: Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::Identifier(IdentifierExpr {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 62,
                                        lexeme_id: 2,
                                    },
                                    name: 2
                                })),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Identifier(IdentifierExpr {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 66,
                                        lexeme_id: 9,
                                    },
                                    name: 9
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
                        token: Token {
                            kind: TokenKind::Identifier,
                            source_id: 0,
                            lexeme_id: 0,
                        },
                        name: 0
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
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 1,
                                lexeme_id: 1,
                            },
                            name: 1
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
                        token: Token {
                            kind: TokenKind::Identifier,
                            source_id: 0,
                            lexeme_id: 0,
                        },
                        name: 0
                    }),
                    rvalue: Expr::Call(CallExpr {
                        func: Box::new(Expr::DotAccess(DotAccessExpr {
                            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                                token: Token {
                                    kind: TokenKind::Identifier,
                                    source_id: 7,
                                    lexeme_id: 2,
                                },
                                name: 2
                            }))),
                            field: 0,
                        })),
                        args: vec![
                            Expr::Identifier(IdentifierExpr {
                                token: Token {
                                    kind: TokenKind::Identifier,
                                    source_id: 19,
                                    lexeme_id: 5,
                                },
                                name: 5
                            }),
                            Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::IntLiteral(1)),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        token: Token {
                                            kind: TokenKind::Identifier,
                                            source_id: 26,
                                            lexeme_id: 9,
                                        },
                                        name: 9
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
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 0,
                                lexeme_id: 0,
                            },
                            name: 0
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
                        token: Token {
                            kind: TokenKind::OpenBrace,
                            source_id: 8,
                            lexeme_id: 2,
                        },
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 14,
                                        lexeme_id: 3,
                                    },
                                    name: 3
                                })),
                                args: vec![Expr::StringLiteral(5)],
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
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 3,
                                lexeme_id: 1,
                            },
                            name: 1
                        })),
                        operator: BinaryOp::LessThan,
                        right: Box::new(Expr::IntLiteral(13)),
                    })),
                    success: BlockStmt {
                        token: Token {
                            kind: TokenKind::OpenBrace,
                            source_id: 10,
                            lexeme_id: 4,
                        },
                        statements: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Call(CallExpr {
                                func: Box::new(Expr::Identifier(IdentifierExpr {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 16,
                                        lexeme_id: 5,
                                    },
                                    name: 5
                                })),
                                args: vec![Expr::StringLiteral(7)],
                            }),
                        })],
                    },
                    fail: Some(BlockStmt {
                        token: Token {
                            kind: TokenKind::OpenBrace,
                            source_id: 35,
                            lexeme_id: 4
                        },
                        statements: vec![Stmt::Assign(AssignStmt {
                            lvalue: Expr::Identifier(IdentifierExpr {
                                token: Token {
                                    kind: TokenKind::Identifier,
                                    source_id: 41,
                                    lexeme_id: 1,
                                },
                                name: 1
                            }),
                            rvalue: Expr::Binary(BinaryExpr {
                                left: Box::new(Expr::IntLiteral(10)),
                                operator: BinaryOp::Add,
                                right: Box::new(Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        token: Token {
                                            kind: TokenKind::Identifier,
                                            source_id: 50,
                                            lexeme_id: 15,
                                        },
                                        name: 15
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
                        field: 2,
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
                        token: Token {
                            kind: TokenKind::Identifier,
                            source_id: 4,
                            lexeme_id: 1
                        },
                        name: 1
                    }),
                    value: Expr::Unary(UnaryExpr {
                        operator: UnaryOp::AddressOf,
                        operand: Box::new(Expr::Identifier(IdentifierExpr {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 12,
                                lexeme_id: 4,
                            },
                            name: 4
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
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 5,
                                lexeme_id: 2
                            },
                            name: 2
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 10,
                                lexeme_id: 4,
                            },
                            name: 4
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
                                token: Token {
                                    kind: TokenKind::Identifier,
                                    source_id: 4,
                                    lexeme_id: 1
                                },
                                name: 1
                            }))),
                            field: IdentifierPat {
                                token: Token {
                                    kind: TokenKind::Identifier,
                                    source_id: 8,
                                    lexeme_id: 3
                                },
                                name: 3
                            },
                        })),
                        payload: IdentifierPat {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 14,
                                lexeme_id: 5
                            },
                            name: 5
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 19,
                                lexeme_id: 8,
                            },
                            name: 8
                        })),
                        args: vec![Expr::StringLiteral(9)],
                    }),
                    except: LetExcept::Or {
                        token: Token {
                            kind: TokenKind::OrKeyword,
                            source_id: 36,
                            lexeme_id: 10,
                        },
                        binding: None,
                        body: BlockStmt {
                            token: Token {
                                kind: TokenKind::OpenBrace,
                                source_id: 39,
                                lexeme_id: 11,
                            },
                            statements: vec![Stmt::Expr(ExprStmt {
                                expr: Expr::Call(CallExpr {
                                    func: Box::new(Expr::Identifier(IdentifierExpr {
                                        token: Token {
                                            kind: TokenKind::Identifier,
                                            source_id: 45,
                                            lexeme_id: 12,
                                        },
                                        name: 12
                                    })),
                                    args: vec![Expr::StringLiteral(13)],
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
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 5,
                                lexeme_id: 2
                            },
                            name: 2
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 11,
                                lexeme_id: 4,
                            },
                            name: 4
                        })),
                        args: vec![
                            Expr::Identifier(IdentifierExpr {
                                token: Token {
                                    kind: TokenKind::Identifier,
                                    source_id: 22,
                                    lexeme_id: 6,
                                },
                                name: 6
                            }),
                            Expr::BoolLiteral(false),
                        ],
                    }),
                    except: LetExcept::Or {
                        token: Token {
                            kind: TokenKind::OrKeyword,
                            source_id: 35,
                            lexeme_id: 10,
                        },
                        binding: Some(11),
                        body: BlockStmt {
                            token: Token {
                                kind: TokenKind::OpenBrace,
                                source_id: 41,
                                lexeme_id: 12,
                            },
                            statements: vec![
                                Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 47,
                                                lexeme_id: 13,
                                            },
                                            name: 13,
                                        })),
                                        args: vec![Expr::StringLiteral(14)],
                                    })
                                }),
                                Stmt::Return(ReturnStmt {
                                    value: Some(Expr::Identifier(IdentifierExpr {
                                        token: Token {
                                            kind: TokenKind::Identifier,
                                            source_id: 78,
                                            lexeme_id: 11,
                                        },
                                        name: 11
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
                                token: Token {
                                    kind: TokenKind::Identifier,
                                    source_id: 5,
                                    lexeme_id: 2
                                },
                                name: 2
                            },
                        })),
                        payload: IdentifierPat {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 8,
                                lexeme_id: 4
                            },
                            name: 4
                        },
                    }),
                    value: Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 13,
                                lexeme_id: 7,
                            },
                            name: 7
                        })),
                        args: vec![],
                    }),
                    except: LetExcept::Wrap(Expr::DotAccess(DotAccessExpr {
                        target: None,
                        field: 9,
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
                        token: Token {
                            kind: TokenKind::Identifier,
                            source_id: 6,
                            lexeme_id: 1,
                        },
                        name: 1
                    }),
                    arms: vec![
                        MatchArm {
                            token: Token {
                                kind: TokenKind::Dot,
                                source_id: 14,
                                lexeme_id: 3,
                            },
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        token: Token {
                                            kind: TokenKind::Identifier,
                                            source_id: 15,
                                            lexeme_id: 4
                                        },
                                        name: 4
                                    },
                                })),
                                payload: IdentifierPat {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 20,
                                        lexeme_id: 6
                                    },
                                    name: 6
                                },
                            }),
                            body: BlockStmt {
                                token: Token {
                                    kind: TokenKind::OpenBrace,
                                    source_id: 23,
                                    lexeme_id: 2,
                                },
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 25,
                                                lexeme_id: 8,
                                            },
                                            name: 8,
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 31,
                                                lexeme_id: 6,
                                            },
                                            name: 6
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            token: Token {
                                kind: TokenKind::Dot,
                                source_id: 40,
                                lexeme_id: 3,
                            },
                            pattern: Pattern::DotAccess(DotAccessPat {
                                target: None,
                                field: IdentifierPat {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 41,
                                        lexeme_id: 12
                                    },
                                    name: 12
                                },
                            }),
                            body: BlockStmt {
                                token: Token {
                                    kind: TokenKind::OpenBrace,
                                    source_id: 46,
                                    lexeme_id: 2,
                                },
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 48,
                                                lexeme_id: 8,
                                            },
                                            name: 8
                                        })),
                                        args: vec![Expr::StringLiteral(13)],
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
                        token: Token {
                            kind: TokenKind::Identifier,
                            source_id: 6,
                            lexeme_id: 1,
                        },
                        name: 1
                    }),
                    arms: vec![
                        MatchArm {
                            token: Token {
                                kind: TokenKind::Dot,
                                source_id: 19,
                                lexeme_id: 3,
                            },
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        token: Token {
                                            kind: TokenKind::Identifier,
                                            source_id: 20,
                                            lexeme_id: 4
                                        },
                                        name: 4
                                    },
                                })),
                                payload: IdentifierPat {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 28,
                                        lexeme_id: 6
                                    },
                                    name: 6
                                },
                            }),
                            body: BlockStmt {
                                token: Token {
                                    kind: TokenKind::OpenBrace,
                                    source_id: 33,
                                    lexeme_id: 2,
                                },
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 35,
                                                lexeme_id: 8,
                                            },
                                            name: 8
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 41,
                                                lexeme_id: 6,
                                            },
                                            name: 6
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            token: Token {
                                kind: TokenKind::Dot,
                                source_id: 52,
                                lexeme_id: 3,
                            },
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        token: Token {
                                            kind: TokenKind::Identifier,
                                            source_id: 53,
                                            lexeme_id: 12
                                        },
                                        name: 12
                                    },
                                })),
                                payload: IdentifierPat {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 61,
                                        lexeme_id: 13
                                    },
                                    name: 13
                                },
                            }),
                            body: BlockStmt {
                                token: Token {
                                    kind: TokenKind::OpenBrace,
                                    source_id: 66,
                                    lexeme_id: 2,
                                },
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 68,
                                                lexeme_id: 8,
                                            },
                                            name: 8
                                        })),
                                        args: vec![Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 74,
                                                lexeme_id: 13,
                                            },
                                            name: 13
                                        })],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            token: Token {
                                kind: TokenKind::Dot,
                                source_id: 85,
                                lexeme_id: 3,
                            },
                            pattern: Pattern::DotAccess(DotAccessPat {
                                target: None,
                                field: IdentifierPat {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 86,
                                        lexeme_id: 14
                                    },
                                    name: 14
                                },
                            }),
                            body: BlockStmt {
                                token: Token {
                                    kind: TokenKind::OpenBrace,
                                    source_id: 93,
                                    lexeme_id: 2,
                                },
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 95,
                                                lexeme_id: 8,
                                            },
                                            name: 8
                                        })),
                                        args: vec![Expr::StringLiteral(15)],
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
                        token: Token {
                            kind: TokenKind::Identifier,
                            source_id: 6,
                            lexeme_id: 1,
                        },
                        name: 1
                    }),
                    arms: vec![
                        MatchArm {
                            token: Token {
                                kind: TokenKind::Dot,
                                source_id: 19,
                                lexeme_id: 3,
                            },
                            pattern: Pattern::DotAccess(DotAccessPat {
                                target: None,
                                field: IdentifierPat {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 20,
                                        lexeme_id: 4
                                    },
                                    name: 4
                                },
                            }),
                            body: BlockStmt {
                                token: Token {
                                    kind: TokenKind::OpenBrace,
                                    source_id: 26,
                                    lexeme_id: 2
                                },
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 28,
                                                lexeme_id: 5,
                                            },
                                            name: 5,
                                        })),
                                        args: vec![Expr::StringLiteral(7)],
                                    }),
                                })],
                            },
                        },
                        MatchArm {
                            token: Token {
                                kind: TokenKind::Dot,
                                source_id: 46,
                                lexeme_id: 3,
                            },
                            pattern: Pattern::Payload(PayloadPat {
                                pat: Box::new(Pattern::DotAccess(DotAccessPat {
                                    target: None,
                                    field: IdentifierPat {
                                        token: Token {
                                            kind: TokenKind::Identifier,
                                            source_id: 47,
                                            lexeme_id: 12
                                        },
                                        name: 12
                                    },
                                })),
                                payload: IdentifierPat {
                                    token: Token {
                                        kind: TokenKind::Identifier,
                                        source_id: 52,
                                        lexeme_id: 13
                                    },
                                    name: 13
                                },
                            }),
                            body: BlockStmt {
                                token: Token {
                                    kind: TokenKind::OpenBrace,
                                    source_id: 56,
                                    lexeme_id: 2
                                },
                                statements: vec![Stmt::Expr(ExprStmt {
                                    expr: Expr::Call(CallExpr {
                                        func: Box::new(Expr::Identifier(IdentifierExpr {
                                            token: Token {
                                                kind: TokenKind::Identifier,
                                                source_id: 58,
                                                lexeme_id: 5,
                                            },
                                            name: 5
                                        })),
                                        args: vec![
                                            Expr::StringLiteral(14),
                                            Expr::Identifier(IdentifierExpr {
                                                token: Token {
                                                    kind: TokenKind::Identifier,
                                                    source_id: 78,
                                                    lexeme_id: 13,
                                                },
                                                name: 13
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
                        module: 0,
                        expr: Box::new(Expr::Identifier(IdentifierExpr {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 5,
                                lexeme_id: 2,
                            },
                            name: 2
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
                        module: 0,
                        expr: Box::new(Expr::Call(CallExpr {
                            func: Box::new(Expr::Identifier(IdentifierExpr {
                                token: Token {
                                    kind: TokenKind::Identifier,
                                    source_id: 5,
                                    lexeme_id: 2,
                                },
                                name: 2
                            })),
                            args: vec![Expr::StringLiteral(4)],
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
                        module: 0,
                        expr: Box::new(Expr::Identifier(IdentifierExpr {
                            token: Token {
                                kind: TokenKind::Identifier,
                                source_id: 6,
                                lexeme_id: 2,
                            },
                            name: 2
                        })),
                    }),
                }
            ),
        },
    );
}
