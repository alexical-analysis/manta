use super::Precedence;
use crate::ast::{Expr, FieldAccess, IdentifierExpr};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::InfixParselet;
use crate::parser::{ParseError, Parser};

/// Parses field access expressions.
///
//// Example: `pet.name`
pub struct FieldAccessParselet;

impl InfixParselet for FieldAccessParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> Result<Expr, ParseError> {
        let field_token = parser.consume()?;

        let field_name = match field_token.kind {
            TokenKind::Ident => {
                let name = field_token
                    .lexeme
                    .ok_or_else(|| ParseError::Custom("Field name missing lexeme".to_string()))?;
                IdentifierExpr { name }
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    "field name required after '.'".to_string(),
                ));
            }
        };

        Ok(Expr::FieldAccess(FieldAccess {
            target: Box::new(left),
            field: Box::new(field_name),
        }))
    }

    fn precedence(&self) -> Precedence {
        Precedence::Call
    }
}
