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
        let right = lexer.next_token();
        if right.kind != TokenKind::Identifier {
            return Err(ParseError::InvalidExpression(
                token,
                "expecting an identifier".to_string(),
            ));
        }

        let left = match left {
            Expr::Identifier(left) => left,
            _ => {
                return Err(ParseError::InvalidExpression(
                    token,
                    "module names must be identifiers".to_string(),
                ));
            }
        };

        Ok(Expr::Identifier(IdentifierExpr {
            id: token.source_id,
            module: Some(left.name),
            name: right.lexeme_id,
        }))
    }

    fn precedence(&self) -> Precedence {
        Precedence::Call
    }
}
