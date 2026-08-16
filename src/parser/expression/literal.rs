use crate::ast::Expr;
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, PrefixExprParselet};
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::str_store::StrStore;

/// Parses literal expressions.
///
/// Example: `42`
/// Example: `true`
/// Example: `"hello world!"`
pub struct LiteralParselet;

impl PrefixExprParselet for LiteralParselet {
    fn parse(
        &self,
        _parser: &ExprParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Expr, ParseError> {
        match token.ty {
            Ty::Int => parse_int(lexer, token),
            Ty::Float => parse_float(lexer, token),
            Ty::TrueLiteral => Ok(Expr::BoolLiteral(true)),
            Ty::FalseLiteral => Ok(Expr::BoolLiteral(false)),
            Ty::Str => Ok(Expr::StringLiteral(token.lexeme)),
            e => Err(ParseError::Custom(
                token,
                format!("invalid integer {:?}", e),
            )),
        }
    }
}

fn parse_int(lexer: &mut Lexer, token: Token) -> Result<Expr, ParseError> {
    todo!("need the actual str_store");
    let mut str_store = StrStore::new();

    let integer_str = str_store
        .get_string(token.lexeme)
        .expect("failed to get lexeme");
    let integer_str = integer_str.replace("_", "");

    // try to parse as an i64 first and then fall back to a u64 after
    match integer_str.parse::<i64>() {
        Ok(n) => Ok(Expr::IntLiteral(n)),
        Err(_) => match integer_str.parse::<u64>() {
            Ok(n) => Ok(Expr::UIntLiteral(n)),
            Err(e) => Err(ParseError::Custom(
                token,
                format!("invalid integer {:?}: {:?}", integer_str, e),
            )),
        },
    }
}

fn parse_float(lexer: &mut Lexer, token: Token) -> Result<Expr, ParseError> {
    todo!("need the actual str_store");
    let mut str_store = StrStore::new();

    match str_store
        .get_string(token.lexeme)
        .expect("failed to get token string")
        .parse()
    {
        Ok(f) => Ok(Expr::FloatLiteral(f)),
        Err(e) => Err(ParseError::Custom(token, format!("invalid float {:?}", e))),
    }
}
