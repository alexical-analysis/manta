use super::Precedence;
use crate::ast::{Expr, ModuleAccessExpr};
use crate::parser::lexer::Token;
use crate::parser::parselets::InfixExprParselet;
use crate::parser::{ParseError, Parser, expression};

/// Parses module access expressions.
///
/// Example: `fmt::println`
pub struct ModuleAccessParselet;

impl InfixExprParselet for ModuleAccessParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, token: Token) -> Result<Expr, ParseError> {
        match left {
            Expr::Identifier(left) => {
                let expr = expression::parse_expression(parser, Precedence::Base)?;

                Ok(Expr::ModuleAccess(ModuleAccessExpr {
                    module: Box::new(left),
                    expr: Box::new(expr),
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
