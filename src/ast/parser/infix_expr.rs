use crate::ast::ast::{BinaryExpr, BinaryOp, CallExpr, Expr, ExprValue, RangeExpr, RangeOp};
use crate::ast::lexer::{Lexer, Token, Ty};
use crate::ast::parser::Parser;

// Operator precedence levels.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Base,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equality,
    Range,
    Comparison,
    Addition,
    Multiplication,
    Prefix,
    Call,
}

fn get_binary_precedence(op: BinaryOp) -> Precedence {
    match op {
        BinaryOp::Plus => Precedence::Addition,
        BinaryOp::Minus => Precedence::Addition,
        BinaryOp::Multiply => Precedence::Multiplication,
        BinaryOp::Divide => Precedence::Multiplication,
        BinaryOp::Modulo => Precedence::Multiplication,
        BinaryOp::Equal => Precedence::Comparison,
        BinaryOp::NotEqual => Precedence::Comparison,
        BinaryOp::LessThan => Precedence::Comparison,
        BinaryOp::LessThanOrEqual => Precedence::Comparison,
        BinaryOp::GreaterThan => Precedence::Comparison,
        BinaryOp::GreaterThanOrEqual => Precedence::Comparison,
        BinaryOp::LogicalAnd => Precedence::LogicalAnd,
        BinaryOp::LogicalOr => Precedence::LogicalOr,
        BinaryOp::BitwiseAnd => Precedence::BitwiseAnd,
        BinaryOp::BitwiseOr => Precedence::BitwiseOr,
        BinaryOp::BitwiseXor => Precedence::BitwiseXor,
    }
}

fn parse_call_expr(parser: &mut Parser, lexer: &mut Lexer, left: Expr, token: Token) -> Expr {
    let mut args = vec![];

    loop {
        let next_arg = lexer.peek();
        if next_arg.ty == Ty::CloseParen {
            lexer.next(parser.str_store);
            break;
        }

        if next_arg.ty == Ty::Eof {
            lexer.recover_until_expr(parser.str_store);
            return parser.get_expr(next_arg, ExprValue::Invalid("unclosed argument list"));
        }

        let arg = parser.parse_expr(lexer, Precedence::Base);
        args.push(arg);

        let sep = lexer.peek();
        match sep.ty {
            Ty::Comma => {
                lexer.next(parser.str_store);
            }
            Ty::CloseParen => {
                lexer.next(parser.str_store);
                break;
            }
            _ => {
                lexer.recover_until_expr(parser.str_store);
                return parser.get_expr(
                    sep,
                    ExprValue::Invalid("expected ',' or ')' in argument list"),
                );
            }
        }
    }

    parser.get_expr(token, ExprValue::Call(CallExpr { func: left, args }))
}

fn parse_range(
    parser: &mut Parser,
    lexer: &mut Lexer,
    left: Expr,
    op: RangeOp,
    token: Token,
) -> Expr {
    let end = parser.parse_expr(lexer, Precedence::Range);
    parser.get_expr(
        token,
        ExprValue::Range(RangeExpr {
            start: left,
            op,
            end,
        }),
    )
}

fn parse_binary_expr(
    parser: &mut Parser,
    lexer: &mut Lexer,
    left: Expr,
    op: BinaryOp,
    token: Token,
) -> Expr {
    let precedence = get_binary_precedence(op);
    let right = parser.parse_expr(lexer, precedence);
    parser.get_expr(
        token,
        ExprValue::Binary(BinaryExpr {
            left,
            operator: op,
            right,
        }),
    )
}

pub fn parse_infix_expr(parser: &mut Parser, lexer: &mut Lexer, left: Expr, token: Token) -> Expr {
    match token.ty {
        Ty::OpenParen => parse_call_expr(parser, lexer, left, token),
        Ty::RangeInclusive => parse_range(parser, lexer, left, RangeOp::Inclusive, token),
        Ty::RangeExclusive => parse_range(parser, lexer, left, RangeOp::Exclusive, token),
        Ty::Plus => parse_binary_expr(parser, lexer, left, BinaryOp::Plus, token),
        Ty::Minus => parse_binary_expr(parser, lexer, left, BinaryOp::Minus, token),
        Ty::Star => parse_binary_expr(parser, lexer, left, BinaryOp::Multiply, token),
        Ty::Slash => parse_binary_expr(parser, lexer, left, BinaryOp::Divide, token),
        Ty::EqualEqual => parse_binary_expr(parser, lexer, left, BinaryOp::Equal, token),
        Ty::LessThan => parse_binary_expr(parser, lexer, left, BinaryOp::LessThan, token),
        Ty::LessOrEqual => parse_binary_expr(parser, lexer, left, BinaryOp::LessThanOrEqual, token),
        Ty::GreaterThan => parse_binary_expr(parser, lexer, left, BinaryOp::GreaterThan, token),
        Ty::GreaterOrEqual => {
            parse_binary_expr(parser, lexer, left, BinaryOp::GreaterOrEqual, token)
        }
        Ty::NotEqual => parse_binary_expr(parser, lexer, left, BinaryOp::NotEqual, token),
        _ => {
            lexer.recover_until_expr(parser.str_store);
            parser.get_expr(token, ExprValue::Invalid("unknown infix expression"))
        }
    }
}
