use crate::ast::{DotAccessExpr, Expr};
use crate::parser::ParseError;
use crate::parser::expression::{ExprParser, InfixExprParselet, Precedence, PrefixExprParselet};
use crate::parser::lexer::{Lexer, Token, TokenKind};

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
        let token = lexer.next_token();
        let name = match token.kind {
            TokenKind::Identifier => token.lexeme_id,
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
        let token = lexer.next_token();
        let name = match token.kind {
            TokenKind::Identifier => token.lexeme_id,
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
