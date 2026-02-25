mod binary_operator;
mod call;
mod dot_access;
mod group;
mod identifier;
mod index;
mod literal;
mod meta_type;
mod module_access;
mod unary_operator;

use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BinaryOp, Expr, UnaryOp};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};

use binary_operator::BinaryOperatorParselet;
use call::CallParselet;
use dot_access::{InfixDotAccessParselet, PrefixDotAccessParselet};
use group::GroupParselet;
use identifier::IdentifierParselet;
use index::IndexParselet;
use literal::LiteralParselet;
use meta_type::MetaTypeParselet;
use module_access::ModuleAccessParselet;
use unary_operator::UnaryOperatorParselet;

/// Trait for prefix expression parselets.
pub trait PrefixExprParselet {
    /// Parse a prefix expression given the consumed token.
    fn parse(
        &self,
        parser: &ExprParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Expr, ParseError>;
}

/// Trait for infix expression parselets.
pub trait InfixExprParselet {
    /// Parse an infix expression with `left` already parsed and the consumed token.
    fn parse(
        &self,
        parser: &ExprParser,
        lexer: &mut Lexer,
        left: Expr,
        token: Token,
    ) -> Result<Expr, ParseError>;

    /// Precedence of this infix operator.
    fn precedence(&self) -> Precedence;
}

// Operator precedence levels.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
    Base,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equality,
    Comparison,
    Addition,
    Multiplication,
    Prefix,
    Call,
}

pub struct ExprParser {
    prefix_parselets: HashMap<TokenKind, Rc<dyn PrefixExprParselet>>,
    infix_parselets: HashMap<TokenKind, Rc<dyn InfixExprParselet>>,
}

impl ExprParser {
    pub fn new() -> Self {
        let mut prefix_parselets: HashMap<TokenKind, Rc<dyn PrefixExprParselet>> = HashMap::new();
        prefix_parselets.insert(TokenKind::Int, Rc::new(LiteralParselet));
        prefix_parselets.insert(TokenKind::Float, Rc::new(LiteralParselet));
        prefix_parselets.insert(TokenKind::Str, Rc::new(LiteralParselet));
        prefix_parselets.insert(TokenKind::TrueLiteral, Rc::new(LiteralParselet));
        prefix_parselets.insert(TokenKind::FalseLiteral, Rc::new(LiteralParselet));
        prefix_parselets.insert(TokenKind::Identifier, Rc::new(IdentifierParselet));
        prefix_parselets.insert(TokenKind::OpenParen, Rc::new(GroupParselet));
        prefix_parselets.insert(TokenKind::Dot, Rc::new(PrefixDotAccessParselet));
        prefix_parselets.insert(TokenKind::At, Rc::new(MetaTypeParselet));
        prefix_parselets.insert(
            TokenKind::Minus,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Negate,
            }),
        );
        prefix_parselets.insert(
            TokenKind::Plus,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Positive,
            }),
        );
        prefix_parselets.insert(
            TokenKind::Bang,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Not,
            }),
        );
        prefix_parselets.insert(
            TokenKind::Star,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Dereference,
            }),
        );
        prefix_parselets.insert(
            TokenKind::And,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::AddressOf,
            }),
        );

        let mut infix_parselets: HashMap<TokenKind, Rc<dyn InfixExprParselet>> = HashMap::new();
        infix_parselets.insert(
            TokenKind::Plus,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Add,
                precedence: Precedence::Addition,
            }),
        );
        infix_parselets.insert(
            TokenKind::Minus,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Subtract,
                precedence: Precedence::Addition,
            }),
        );
        infix_parselets.insert(
            TokenKind::Star,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Multiply,
                precedence: Precedence::Multiplication,
            }),
        );
        infix_parselets.insert(
            TokenKind::Slash,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Divide,
                precedence: Precedence::Multiplication,
            }),
        );
        infix_parselets.insert(
            TokenKind::Percent,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Modulo,
                precedence: Precedence::Multiplication,
            }),
        );
        infix_parselets.insert(
            TokenKind::EqualEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Equal,
                precedence: Precedence::Equality,
            }),
        );
        infix_parselets.insert(
            TokenKind::NotEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::NotEqual,
                precedence: Precedence::Equality,
            }),
        );
        infix_parselets.insert(
            TokenKind::LessThan,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LessThan,
                precedence: Precedence::Comparison,
            }),
        );
        infix_parselets.insert(
            TokenKind::GreaterThan,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::GreaterThan,
                precedence: Precedence::Comparison,
            }),
        );
        infix_parselets.insert(
            TokenKind::LessOrEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LessThanOrEqual,
                precedence: Precedence::Comparison,
            }),
        );
        infix_parselets.insert(
            TokenKind::GreaterOrEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::GreaterThanOrEqual,
                precedence: Precedence::Comparison,
            }),
        );
        infix_parselets.insert(
            TokenKind::AndAnd,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LogicalAnd,
                precedence: Precedence::LogicalAnd,
            }),
        );
        infix_parselets.insert(
            TokenKind::PipePipe,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LogicalOr,
                precedence: Precedence::LogicalOr,
            }),
        );
        infix_parselets.insert(
            TokenKind::And,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::BitwiseAnd,
                precedence: Precedence::BitwiseAnd,
            }),
        );
        infix_parselets.insert(
            TokenKind::Pipe,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::BitwiseOr,
                precedence: Precedence::BitwiseOr,
            }),
        );
        infix_parselets.insert(
            TokenKind::Caret,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::BitwiseXor,
                precedence: Precedence::BitwiseXor,
            }),
        );
        infix_parselets.insert(TokenKind::OpenParen, Rc::new(CallParselet));
        infix_parselets.insert(TokenKind::OpenSquare, Rc::new(IndexParselet));
        infix_parselets.insert(TokenKind::Dot, Rc::new(InfixDotAccessParselet));
        infix_parselets.insert(TokenKind::ColonColon, Rc::new(ModuleAccessParselet));

        ExprParser {
            prefix_parselets,
            infix_parselets,
        }
    }

    /// Parse an expression with a minimum precedence level.
    /// This implements the Pratt parser algorithm.
    pub fn parse(&self, lexer: &mut Lexer, min_precedence: Precedence) -> Result<Expr, ParseError> {
        let token = lexer.next_token();

        let prefix_opt = self.prefix_parselets.get(&token.kind);
        if prefix_opt.is_none() {
            return Err(ParseError::UnexpectedToken(
                token,
                "No prefix parselet for token kind".to_string(),
            ));
        };

        let prefix = prefix_opt.unwrap().clone();
        let mut left = prefix.parse(self, lexer, token)?;

        // Loop while the next token's precedence is higher than or equal to min_precedence
        loop {
            let token = lexer.peek();
            if token.kind == TokenKind::Eof || token.kind == TokenKind::Semicolon {
                break;
            }

            let infix_parselet = self.infix_parselets.get(&token.kind);
            if infix_parselet.is_none() {
                break;
            }
            let infix_parselet = infix_parselet.unwrap();

            if infix_parselet.precedence() <= min_precedence {
                break;
            }

            let token = lexer.next_token();
            left = infix_parselet.parse(self, lexer, left, token)?;
        }

        Ok(left)
    }

    pub fn is_expression_prefix(&self, token: Token) -> bool {
        self.prefix_parselets.contains_key(&token.kind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        AllocExpr, ArrayType, BinaryExpr, BinaryOp, CallExpr, DotAccessExpr, Expr, FreeExpr,
        IdentifierExpr, IndexExpr, MetaTypeExpr, TypeSpec, UnaryExpr, UnaryOp,
    };
    use crate::parser::lexer::Lexer;
    use crate::str_store::StrStore;
    use pretty_assertions::assert_eq;

    macro_rules! test_parse_expressions {
        ( $( $case:ident { input: $input:expr, want_var: $want_var:pat, want_value: $want_value:expr, } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let mut str_store = StrStore::new();
                    let mut lexer = Lexer::new($input, &mut str_store);
                    let parser = ExprParser::new();

                    let expr = parser.parse(&mut lexer, Precedence::Base).unwrap();
                    match expr {
                        $want_var => $want_value,
                        _ => panic!("Expected {:?} => {{ {:?} }}, got {:?}", stringify!($want_var), stringify!($want_value), expr),
                    }
                }
            )*
        };
    }

    test_parse_expressions!(
        parse_expression_max_int {
            input: "9223372036854775807",
            want_var: Expr::IntLiteral(9223372036854775807),
            want_value: (),
        },
        parse_expression_float {
            input: "3.45",
            want_var: Expr::FloatLiteral(3.45),
            want_value: (),
        },
        parse_expression_complex_float {
            input: "1.23e4",
            want_var: Expr::FloatLiteral(12300.0),
            want_value: (),
        },
        parse_expression_true {
            input: "true",
            want_var: Expr::BoolLiteral(true),
            want_value: (),
        },
        parse_expression_false {
            input: "false",
            want_var: Expr::BoolLiteral(false),
            want_value: (),
        },
        parse_expression_empty_string {
            input: r#""""#,
            want_var: Expr::StringLiteral(s),
            want_value: assert_eq!(s, 0),
        },
        parse_expression_int_literal {
            input: "42",
            want_var: Expr::IntLiteral(42),
            want_value: (),
        },
        parse_expression_string_literal {
            input: r#""hello world""#,
            want_var: Expr::StringLiteral(s),
            want_value: assert_eq!(s, 0),
        },
        parse_expression_bool_true {
            input: "true",
            want_var: Expr::BoolLiteral(true),
            want_value: (),
        },
        parse_expression_bool_false {
            input: "false",
            want_var: Expr::BoolLiteral(false),
            want_value: (),
        },
        parse_expression_identifier {
            input: "myVariable",
            want_var: Expr::Identifier(ident),
            want_value: assert_eq!(ident.name, 0),
        },
        parse_expression_identifier_single_char {
            input: "x",
            want_var: Expr::Identifier(ident),
            want_value: assert_eq!(ident.name, 0),
        },
        parse_expression_negative_int {
            input: "-42",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Negate,
                    operand: Box::new(Expr::IntLiteral(42)),
                }
            ),
        },
        parse_expression_positive_int {
            input: "+42",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Positive,
                    operand: Box::new(Expr::IntLiteral(42)),
                },
            ),
        },
        parse_expression_positive_negative_int {
            input: "+-42",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Positive,
                    operand: Box::new(Expr::Unary(UnaryExpr {
                        operator: UnaryOp::Negate,
                        operand: Box::new(Expr::IntLiteral(42)),
                    })),
                },
            ),
        },
        parse_expression_not_bool {
            input: "!true",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Not,
                    operand: Box::new(Expr::BoolLiteral(true)),
                },
            ),
        },
        parse_expression_dereference {
            input: "*ptr",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Dereference,
                    operand: Box::new(Expr::Identifier(IdentifierExpr { id: 1, name: 1 })),
                },
            ),
        },
        parse_expression_address_of {
            input: "&abc",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::AddressOf,
                    operand: Box::new(Expr::Identifier(IdentifierExpr { id: 1, name: 1 })),
                },
            ),
        },
        parse_expression_double_dereference {
            input: "**ptr",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Dereference,
                    operand: Box::new(Expr::Unary(UnaryExpr {
                        operator: UnaryOp::Dereference,
                        operand: Box::new(Expr::Identifier(IdentifierExpr { id: 2, name: 1 })),
                    })),
                },
            ),
        },
        parse_expression_negative_not {
            input: "-!x",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Negate,
                    operand: Box::new(Expr::Unary(UnaryExpr {
                        operator: UnaryOp::Not,
                        operand: Box::new(Expr::Identifier(IdentifierExpr { id: 2, name: 2 })),
                    })),
                },
            ),
        },
        parse_expression_grouped_negative {
            input: "(-x)",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Negate,
                    operand: Box::new(Expr::Identifier(IdentifierExpr { id: 2, name: 2 })),
                },
            ),
        },
        parse_expression_negative_grouped_not {
            input: "-(!y)",
            want_var: Expr::Unary(expr),
            want_value: assert_eq!(
                expr,
                UnaryExpr {
                    operator: UnaryOp::Negate,
                    operand: Box::new(Expr::Unary(UnaryExpr {
                        operator: UnaryOp::Not,
                        operand: Box::new(Expr::Identifier(IdentifierExpr { id: 3, name: 3 })),
                    })),
                }
            ),
        },
        parse_expression_addition {
            input: "1 + 2",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(1)),
                    operator: BinaryOp::Add,
                    right: Box::new(Expr::IntLiteral(2)),
                },
            ),
        },
        parse_expression_multiplication {
            input: "3 * 4",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(3)),
                    operator: BinaryOp::Multiply,
                    right: Box::new(Expr::IntLiteral(4)),
                },
            ),
        },
        parse_expression_subtraction {
            input: "10 - 3",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(10)),
                    operator: BinaryOp::Subtract,
                    right: Box::new(Expr::IntLiteral(3)),
                },
            ),
        },
        parse_expression_division {
            input: "20 / 4",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(20)),
                    operator: BinaryOp::Divide,
                    right: Box::new(Expr::IntLiteral(4)),
                },
            ),
        },
        parse_expression_modulo {
            input: "10 % 3",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(10)),
                    operator: BinaryOp::Modulo,
                    right: Box::new(Expr::IntLiteral(3)),
                },
            ),
        },
        parse_expression_multiply_over_add {
            input: "1 + 2 * 3",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(1)),
                    operator: BinaryOp::Add,
                    right: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::IntLiteral(2)),
                        operator: BinaryOp::Multiply,
                        right: Box::new(Expr::IntLiteral(3)),
                    })),
                },
            ),
        },
        parse_expression_associative_addition {
            input: "1 + 2 + 3",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::IntLiteral(1)),
                        operator: BinaryOp::Add,
                        right: Box::new(Expr::IntLiteral(2)),
                    })),
                    operator: BinaryOp::Add,
                    right: Box::new(Expr::IntLiteral(3)),
                },
            ),
        },
        parse_expression_equality_check {
            input: "a == b",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    operator: BinaryOp::Equal,
                    right: Box::new(Expr::Identifier(IdentifierExpr { id: 5, name: 2 })),
                }
            ),
        },
        parse_expression_inequality_check {
            input: "x != y",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    operator: BinaryOp::NotEqual,
                    right: Box::new(Expr::Identifier(IdentifierExpr { id: 5, name: 2 })),
                },
            ),
        },
        parse_expression_less_than {
            input: "4 < b",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(4)),
                    operator: BinaryOp::LessThan,
                    right: Box::new(Expr::Identifier(IdentifierExpr { id: 4, name: 2 })),
                },
            ),
        },
        parse_expression_greater_than {
            input: "x > y",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    operator: BinaryOp::GreaterThan,
                    right: Box::new(Expr::Identifier(IdentifierExpr { id: 4, name: 2 })),
                },
            ),
        },
        parse_expression_less_or_equal {
            input: "a <= b",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    operator: BinaryOp::LessThanOrEqual,
                    right: Box::new(Expr::Identifier(IdentifierExpr { id: 5, name: 2 })),
                },
            ),
        },
        parse_expression_greater_or_equal {
            input: "x >= 9",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    operator: BinaryOp::GreaterThanOrEqual,
                    right: Box::new(Expr::IntLiteral(9)),
                },
            ),
        },
        parse_expression_bool_expression {
            input: "3.45 == b && true != d",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::FloatLiteral(3.45)),
                        operator: BinaryOp::Equal,
                        right: Box::new(Expr::Identifier(IdentifierExpr { id: 8, name: 2 })),
                    })),
                    operator: BinaryOp::LogicalAnd,
                    right: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::BoolLiteral(true)),
                        operator: BinaryOp::NotEqual,
                        right: Box::new(Expr::Identifier(IdentifierExpr { id: 21, name: 6 })),
                    })),
                },
            ),
        },
        parse_expression_or_over_and {
            input: "1 | 2 & b",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(1)),
                    operator: BinaryOp::BitwiseOr,
                    right: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::IntLiteral(2)),
                        operator: BinaryOp::BitwiseAnd,
                        right: Box::new(Expr::Identifier(IdentifierExpr { id: 8, name: 4 })),
                    })),
                },
            ),
        },
        parse_expression_call_no_args {
            input: "print()",
            want_var: Expr::Call(expr),
            want_value: assert_eq!(
                expr,
                CallExpr {
                    func: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    args: vec![],
                },
            ),
        },
        parse_expression_call_with_deref {
            input: "fmt_println(*p)",
            want_var: Expr::Call(expr),
            want_value: assert_eq!(
                expr,
                CallExpr {
                    func: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    args: vec![Expr::Unary(UnaryExpr {
                        operator: UnaryOp::Dereference,
                        operand: Box::new(Expr::Identifier(IdentifierExpr { id: 13, name: 3 })),
                    })],
                }
            ),
        },
        parse_expression_call_with_args {
            input: "sum(1, b, 3)",
            want_var: Expr::Call(expr),
            want_value: assert_eq!(
                expr,
                CallExpr {
                    func: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    args: vec![
                        Expr::IntLiteral(1),
                        Expr::Identifier(IdentifierExpr { id: 7, name: 4 }),
                        Expr::IntLiteral(3),
                    ],
                },
            ),
        },
        parse_expression_index_access {
            input: "arr[5]",
            want_var: Expr::Index(expr),
            want_value: assert_eq!(
                expr,
                IndexExpr {
                    target: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    index: Box::new(Expr::IntLiteral(5)),
                },
            ),
        },
        parse_expression_index_access_complex {
            input: "matrix[i + 1]",
            want_var: Expr::Index(expr),
            want_value: assert_eq!(
                expr,
                IndexExpr {
                    target: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    index: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::Identifier(IdentifierExpr { id: 7, name: 2 })),
                        operator: BinaryOp::Add,
                        right: Box::new(Expr::IntLiteral(1)),
                    })),
                },
            ),
        },
        parse_expression_index_access_nested {
            input: "data[rows[i]]",
            want_var: Expr::Index(expr),
            want_value: assert_eq!(
                expr,
                IndexExpr {
                    target: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    index: Box::new(Expr::Index(IndexExpr {
                        target: Box::new(Expr::Identifier(IdentifierExpr { id: 5, name: 2 })),
                        index: Box::new(Expr::Identifier(IdentifierExpr { id: 10, name: 3 })),
                    })),
                },
            ),
        },
        parse_expression_index_access_3d {
            input: "tensor[x][y][z]",
            want_var: Expr::Index(expr),
            want_value: assert_eq!(
                expr,
                IndexExpr {
                    target: Box::new(Expr::Index(IndexExpr {
                        target: Box::new(Expr::Index(IndexExpr {
                            target: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                            index: Box::new(Expr::Identifier(IdentifierExpr { id: 7, name: 2 })),
                        })),
                        index: Box::new(Expr::Identifier(IdentifierExpr { id: 10, name: 4 })),
                    })),
                    index: Box::new(Expr::Identifier(IdentifierExpr { id: 13, name: 5 })),
                },
            ),
        },
        parse_expression_field_access {
            input: "person.name",
            want_var: Expr::DotAccess(expr),
            want_value: assert_eq!(
                expr,
                DotAccessExpr {
                    target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                        id: 0,
                        name: 0
                    }))),
                    field: 2,
                }
            ),
        },
        parse_expression_field_access_nested {
            input: "company.ceo.name",
            want_var: Expr::DotAccess(expr),
            want_value: assert_eq!(
                expr,
                DotAccessExpr {
                    target: Some(Box::new(Expr::DotAccess(DotAccessExpr {
                        target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                            id: 0,
                            name: 0
                        }))),
                        field: 2,
                    }))),
                    field: 3,
                },
            ),
        },
        parse_expression_field_access_index {
            input: "users[0].email",
            want_var: Expr::DotAccess(expr),
            want_value: assert_eq!(
                expr,
                DotAccessExpr {
                    target: Some(Box::new(Expr::Index(IndexExpr {
                        target: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                        index: Box::new(Expr::IntLiteral(0)),
                    }))),
                    field: 5,
                },
            ),
        },
        parse_expression_field_access_call {
            input: "config.getDatabase().host",
            want_var: Expr::DotAccess(expr),
            want_value: assert_eq!(
                expr,
                DotAccessExpr {
                    target: Some(Box::new(Expr::Call(CallExpr {
                        func: Box::new(Expr::DotAccess(DotAccessExpr {
                            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                                id: 0,
                                name: 0
                            }))),
                            field: 2,
                        })),
                        args: vec![],
                    }))),
                    field: 5,
                },
            ),
        },
        parse_expression_call_new {
            input: "alloc(@i32)",
            want_var: Expr::Alloc(expr),
            want_value: assert_eq!(
                expr,
                AllocExpr {
                    meta_type: Box::new(Expr::MetaType(MetaTypeExpr {
                        type_spec: TypeSpec::Int32,
                    })),
                    options: vec![],
                }
            ),
        },
        parse_expression_call_new_slice {
            input: "alloc(@[]f64, .Len(10))",
            want_var: Expr::Alloc(expr),
            want_value: assert_eq!(
                expr,
                AllocExpr {
                    meta_type: Box::new(Expr::MetaType(MetaTypeExpr {
                        type_spec: TypeSpec::Slice(Box::new(TypeSpec::Float64)),
                    })),
                    options: vec![Expr::Call(CallExpr {
                        func: Box::new(Expr::DotAccess(DotAccessExpr {
                            target: None,
                            field: 7,
                        })),
                        args: vec![Expr::IntLiteral(10)],
                    })],
                }
            ),
        },
        parse_expression_call_new_slice_with_cap {
            input: "alloc(@[ ] bool, .Cap(15), .NoInit)",
            want_var: Expr::Alloc(expr),
            want_value: assert_eq!(
                expr,
                AllocExpr {
                    meta_type: Box::new(Expr::MetaType(MetaTypeExpr {
                        type_spec: TypeSpec::Slice(Box::new(TypeSpec::Bool)),
                    })),
                    options: vec![
                        Expr::Call(CallExpr {
                            func: Box::new(Expr::DotAccess(DotAccessExpr {
                                target: None,
                                field: 7,
                            })),
                            args: vec![Expr::IntLiteral(15)],
                        }),
                        Expr::DotAccess(DotAccessExpr {
                            target: None,
                            field: 10,
                        })
                    ],
                },
            ),
        },
        parse_expression_call_new_method {
            input: "alloc(@i32).method()",
            want_var: Expr::Call(expr),
            want_value: assert_eq!(
                expr,
                CallExpr {
                    func: Box::new(Expr::DotAccess(DotAccessExpr {
                        target: Some(Box::new(Expr::Alloc(AllocExpr {
                            meta_type: Box::new(Expr::MetaType(MetaTypeExpr {
                                type_spec: TypeSpec::Int32,
                            })),
                            options: vec![],
                        }))),
                        field: 5,
                    })),
                    args: vec![],
                }
            ),
        },
        parse_expression_call_free {
            input: "free(ptr)",
            want_var: Expr::Free(expr),
            want_value: assert_eq!(
                expr,
                FreeExpr {
                    expr: Box::new(Expr::Identifier(IdentifierExpr { id: 5, name: 2 })),
                }
            ),
        },
        parse_expression_call_free_complex {
            input: "free(data.buffer.ptr)",
            want_var: Expr::Free(expr),
            want_value: assert_eq!(
                expr,
                FreeExpr {
                    expr: Box::new(Expr::DotAccess(DotAccessExpr {
                        target: Some(Box::new(Expr::DotAccess(DotAccessExpr {
                            target: Some(Box::new(Expr::Identifier(IdentifierExpr {
                                id: 5,
                                name: 2
                            }))),
                            field: 4,
                        }))),
                        field: 5,
                    })),
                }
            ),
        },
        parse_expression_enum_variant {
            input: ".Ok",
            want_var: Expr::DotAccess(expr),
            want_value: assert_eq!(
                expr,
                DotAccessExpr {
                    target: None,
                    field: 1,
                },
            ),
        },
        parse_expression_enum_check {
            input: "ret == .Err",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    operator: BinaryOp::Equal,
                    right: Box::new(Expr::DotAccess(DotAccessExpr {
                        target: None,
                        field: 3,
                    })),
                },
            ),
        },
        parse_expression_meta_type {
            input: "@i32",
            want_var: Expr::MetaType(expr),
            want_value: assert_eq!(
                expr,
                MetaTypeExpr {
                    type_spec: TypeSpec::Int32,
                },
            ),
        },
        parse_expression_slice_meta_type {
            input: "@[]Vec3",
            want_var: Expr::MetaType(expr),
            want_value: assert_eq!(
                expr,
                MetaTypeExpr {
                    type_spec: TypeSpec::Slice(Box::new(TypeSpec::Named {
                        module: None,
                        name: 3,
                    }))
                },
            ),
        },
        parse_expression_array_module_type {
            input: "@[25]io::file",
            want_var: Expr::MetaType(expr),
            want_value: assert_eq!(
                expr,
                MetaTypeExpr {
                    type_spec: TypeSpec::Array(ArrayType {
                        type_spec: Box::new(TypeSpec::Named {
                            module: Some(4),
                            name: 6,
                        }),
                        size: 25,
                    })
                }
            ),
        },
        parse_expression_grouped_int {
            input: "(42)",
            want_var: Expr::IntLiteral(42),
            want_value: (),
        },
        parse_expression_grouped_binary_operation {
            input: "(2 + 3)",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(2)),
                    operator: BinaryOp::Add,
                    right: Box::new(Expr::IntLiteral(3)),
                }
            ),
        },
        parse_expression_grouped_with_precedence {
            input: "2 * (3 + 4)",
            want_var: Expr::Binary(expr),
            want_value: assert_eq!(
                expr,
                BinaryExpr {
                    left: Box::new(Expr::IntLiteral(2)),
                    operator: BinaryOp::Multiply,
                    right: Box::new(Expr::Binary(BinaryExpr {
                        left: Box::new(Expr::IntLiteral(3)),
                        operator: BinaryOp::Add,
                        right: Box::new(Expr::IntLiteral(4)),
                    })),
                }
            ),
        },
        parse_expression_nested_groups {
            input: "((42))",
            want_var: Expr::IntLiteral(42),
            want_value: (),
        },
        parse_expression_grouped_identifier {
            input: "(myVar)",
            want_var: Expr::Identifier(ident),
            want_value: assert_eq!(ident.name, 1),
        },
        parse_expression_alloc_single_arg {
            input: "alloc(@i32)",
            want_var: Expr::Alloc(expr),
            want_value: assert_eq!(
                expr,
                AllocExpr {
                    meta_type: Box::new(Expr::MetaType(MetaTypeExpr {
                        type_spec: TypeSpec::Int32,
                    })),
                    options: vec![],
                }
            ),
        },
        parse_expression_alloc_with_options {
            input: "alloc(@i32, true, 10)",
            want_var: Expr::Alloc(expr),
            want_value: assert_eq!(
                expr,
                AllocExpr {
                    meta_type: Box::new(Expr::MetaType(MetaTypeExpr {
                        type_spec: TypeSpec::Int32,
                    })),
                    options: vec![Expr::BoolLiteral(true), Expr::IntLiteral(10),],
                }
            ),
        },
        parse_expression_call_chained {
            input: "foo(bar(5))",
            want_var: Expr::Call(expr),
            want_value: assert_eq!(
                expr,
                CallExpr {
                    func: Box::new(Expr::Identifier(IdentifierExpr { id: 0, name: 0 })),
                    args: vec![Expr::Call(CallExpr {
                        func: Box::new(Expr::Identifier(IdentifierExpr { id: 4, name: 2 })),

                        args: vec![Expr::IntLiteral(5)],
                    })],
                }
            ),
        },
    );
}
