use crate::ast::{Expr, IdentifierExpr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, PrefixExprParselet};
use crate::parser::lexer::{Lexer, Token};

/// Parses identifier expressions.
///
/// Example: `foo`, `myVariable`, `count`
pub struct IdentifierParselet;

impl PrefixExprParselet for IdentifierParselet {
    fn parse(
        &self,
        _parser: &ExprParser,
        _lexer: &mut Lexer,
        token: Token,
    ) -> Result<Expr, ParseError> {
        let name = token.lexeme_id;
        Ok(Expr::Identifier(IdentifierExpr {
            id: token.source_id,
            name,
        }))
    }
}
