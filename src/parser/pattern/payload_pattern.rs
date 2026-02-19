use crate::ast::{IdentifierPat, Pattern, PayloadPat};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::{InfixPatternParselet, PatternParser};

/// Parses pattern payloads.
///
/// Example: `(e)`
pub struct PayloadPatternParselet;

impl InfixPatternParselet for PayloadPatternParselet {
    fn parse(
        &self,
        _parser: &PatternParser,
        lexer: &mut Lexer,
        left: Pattern,
        _token: Token,
    ) -> Result<Pattern, ParseError> {
        let ident = lexer.next_token();
        if ident.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                ident,
                "pattern payload must be an identifier".to_string(),
            ));
        }

        match lexer.peek().kind {
            TokenKind::CloseParen => {
                lexer.next_token();

                Ok(Pattern::Payload(PayloadPat {
                    pat: Box::new(left),
                    payload: IdentifierPat {
                        token: ident,
                        name: ident.lexeme_id,
                    },
                }))
            }
            _ => Err(ParseError::UnexpectedToken(
                ident,
                "pattern payload was not closed".to_string(),
            )),
        }
    }
}
