use crate::ast::{DotAccessExpr, Expr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, InfixExprParselet, Precedence, PrefixExprParselet};
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::str_store::StrStore;

/// Parses dot access expressions.
///
/// Example: `pet.name`
pub struct InfixDotAccessParselet;

impl InfixExprParselet for InfixDotAccessParselet {
    fn parse(
        &self,
        _parser: &ExprParser,
        lexer: &mut Lexer,
        left: Expr,
        _token: Token,
    ) -> Result<Expr, ParseError> {
        todo!("need to figure out how to get the actual str_store");
        let mut str_store = StrStore::new();

        let token = lexer.next(&mut str_store);
        let name = match token.ty {
            Ty::Identifier => token.lexeme,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    token,
                    "field name required after '.'".to_string(),
                ));
            }
        };

        Ok(Expr::DotAccess(DotAccessExpr {
            target: Some(Box::new(left)),
            field: name,
        }))
    }

    fn precedence(&self) -> Precedence {
        Precedence::Call
    }
}

/// Parses dot access expressions where the type identifier isn't present
///
/// Example: `.Ok`
pub struct PrefixDotAccessParselet;

impl PrefixExprParselet for PrefixDotAccessParselet {
    fn parse(
        &self,
        _parser: &ExprParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Expr, ParseError> {
        todo!("need to figure out how to get the actual str_store");
        let mut str_store = StrStore::new();

        let token = lexer.next(&mut str_store);
        let name = match token.ty {
            Ty::Identifier => token.lexeme,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    token,
                    "enum variants must be identifiers".to_string(),
                ));
            }
        };

        Ok(Expr::DotAccess(DotAccessExpr {
            target: None,
            field: name,
        }))
    }
}
