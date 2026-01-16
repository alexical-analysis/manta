use crate::ast::{DotAccessExpr, Expr, IdentifierExpr};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixExprParselet;
use crate::parser::{ParseError, Parser};

/// Parses variants expressions where the enum is inferred from context.
/// It is parsed as a field access because other enum variant expressions will also be
/// parsed in the same way
///
/// Example: `.Ok`
pub struct InferedVariantParselet;

impl PrefixExprParselet for InferedVariantParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Expr, ParseError> {
        let next = parser.lookahead(0)?;
        let variant_name = match next.kind {
            TokenKind::Identifier => next.lexeme.clone(),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    next.clone(),
                    "enum variants must be identifiers".to_string(),
                ));
            }
        };
        parser.consume()?;

        Ok(Expr::DotAccess(DotAccessExpr {
            target: None,
            field: Box::new(IdentifierExpr { name: variant_name }),
        }))
    }
}
