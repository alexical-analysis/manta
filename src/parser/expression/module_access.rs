use super::Precedence;
use crate::ast::{Expr, ModuleAccessExpr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, InfixExprParselet};
use crate::parser::lexer::{Lexer, Token};

/// Parses module access expressions.
///
/// Example: `fmt::println`
pub struct ModuleAccessParselet;

impl InfixExprParselet for ModuleAccessParselet {
    fn parse(
        &self,
        parser: &ExprParser,
        lexer: &mut Lexer,
        left: Expr,
        token: Token,
    ) -> Result<Expr, ParseError> {
        match left {
            Expr::Identifier(left) => {
                let expr = parser.parse(lexer, Precedence::Base)?;

                Ok(Expr::ModuleAccess(ModuleAccessExpr {
                    module: left,
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
