use super::Precedence;
use crate::ast::{DotAccessExpr, Expr, IdentifierExpr};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::{InfixExprParselet, PrefixExprParselet};
use crate::parser::{ParseError, Parser};

/// Parses dot access expressions.
///
/// Example: `pet.name`
pub struct DotAccessParselet;

impl InfixExprParselet for DotAccessParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> Result<Expr, ParseError> {
        let field_token = parser.consume()?;
        let field_name = match field_token.kind {
            TokenKind::Identifier => IdentifierExpr {
                name: field_token.lexeme,
            },
            _ => {
                return Err(ParseError::UnexpectedToken(
                    field_token,
                    "field name required after '.'".to_string(),
                ));
            }
        };

        Ok(Expr::DotAccess(DotAccessExpr {
            target: Some(Box::new(left)),
            field: Box::new(field_name),
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
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Expr, ParseError> {
        let field_token = parser.consume()?;
        let field_name = match field_token.kind {
            TokenKind::Identifier => field_token.lexeme.clone(),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    field_token.clone(),
                    "enum variants must be identifiers".to_string(),
                ));
            }
        };

        Ok(Expr::DotAccess(DotAccessExpr {
            target: None,
            field: Box::new(IdentifierExpr { name: field_name }),
        }))
    }
}
