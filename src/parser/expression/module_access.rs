use super::Precedence;
use crate::ast::{Expr, IdentifierExpr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, InfixExprParselet};
use crate::parser::lexer::{Lexer, Token, TokenKind};

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
        match left {
            Expr::Identifier(left) => {
                let module = lexer.next_token();
                if module.kind != TokenKind::Identifier {
                    return Err(ParseError::InvalidExpression(
                        module,
                        "expected module name to be an identifier".to_string(),
                    ));
                }

                Ok(Expr::Identifier(IdentifierExpr {
                    id: left.id,
                    name: left.name,
                    module: Some(module.lexeme_id),
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
