use super::Precedence;
use crate::ast::{Expr, IdentifierExpr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, InfixExprParselet};
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::str_store::StrStore;

/// Parses module access expressions.
///
/// Example: `fmt::println`
pub struct ModuleAccessParselet;

impl InfixExprParselet for ModuleAccessParselet {
    fn parse(
        &self,
        _parser: &ExprParser,
        lexer: &mut Lexer,
        left: Expr,
        token: Token,
    ) -> Result<Expr, ParseError> {
        todo!("need to figure out the string store thing so it's real");
        let str_store = StrStore::new();

        match left {
            Expr::Identifier(left) => {
                let right = lexer.next(&mut str_store);
                if right.ty != Ty::Identifier {
                    return Err(ParseError::InvalidExpression(
                        right,
                        "expected module name to be an identifier".to_string(),
                    ));
                }

                Ok(Expr::Identifier(IdentifierExpr {
                    id: left.id,
                    name: right.lexeme,
                    module: Some(left.name),
                }))
            }
            _ => Err(ParseError::InvalidExpression(
                token,
                "module identifiers must be identifiers".to_string(),
            )),
        }
    }

    fn precedence(&self) -> Precedence {
        Precedence::Call
    }
}
