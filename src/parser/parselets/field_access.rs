use super::Precedence;
use crate::ast::{Expr, FieldAccessExpr, IdentifierExpr};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::InfixExprParselet;
use crate::parser::{ParseError, Parser};

/// Parses field access expressions.
///
/// Example: `pet.name`
pub struct FieldAccessParselet;

impl InfixExprParselet for FieldAccessParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> Result<Expr, ParseError> {
        let field_token = parser.consume()?;

        let field_name = match field_token.kind {
            TokenKind::Identifier => IdentifierExpr {
                name: field_token.lexeme,
            },
            _ => {
                return Err(ParseError::UnexpectedToken(
                    "field name required after '.'".to_string(),
                ));
            }
        };

        Ok(Expr::FieldAccess(FieldAccessExpr {
            target: Some(Box::new(left)),
            field: Box::new(field_name),
        }))
    }

    fn precedence(&self) -> Precedence {
        Precedence::Call
    }
}
