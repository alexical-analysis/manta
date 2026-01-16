use crate::ast::{Pattern, PayloadPat};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::InfixPatternParselet;
use crate::parser::{ParseError, Parser};

/// Parses pattern payloads.
///
/// Example: `(e)`
pub struct PayloadPatternParselet;

impl InfixPatternParselet for PayloadPatternParselet {
    fn parse(
        &self,
        parser: &mut Parser,
        left: Pattern,
        _token: Token,
    ) -> Result<Pattern, ParseError> {
        let ident = parser.consume()?;
        if ident.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                ident,
                "pattern payload must be an identifier".to_string(),
            ));
        }

        let matched = parser.match_token(TokenKind::CloseParen)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                ident,
                "pattern payload was not closed".to_string(),
            ));
        }

        Ok(Pattern::Payload(PayloadPat {
            pat: Box::new(left),
            payload: ident.lexeme,
        }))
    }
}
