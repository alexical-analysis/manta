use crate::ast::{Expr, MetaTypeExpr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, PrefixExprParselet};
use crate::parser::lexer::{Lexer, Token};
use crate::parser::types;
use crate::str_store::StrStore;

/// Parses unary negation expressions.
///
/// Example: `@i32`
/// Example: `@[50]Vec3`
/// Example: `@[]str`
/// Example: `@Person`
pub struct MetaTypeParselet;

impl PrefixExprParselet for MetaTypeParselet {
    fn parse(
        &self,
        _parser: &ExprParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Expr, ParseError> {
        todo!("need to figure out the str store");
        let mut str_store = StrStore::new();

        let token = lexer.next(&mut str_store);
        let type_spec = types::parse_type(lexer, token)?;

        Ok(Expr::MetaType(MetaTypeExpr { type_spec }))
    }
}
