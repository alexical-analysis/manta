use crate::ast::{IdentifierPat, Pattern};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::PrefixPatternParselet;

/// Parses identifier patterns.
///
/// Example: `foo(bar)`, `myVariable`, `count`
pub struct IdentifierPatternParselet;

impl PrefixPatternParselet for IdentifierPatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        let mut payload = None;
        if lexer.peek().kind == TokenKind::OpenParen {
            lexer.next_token();
            let payload_token = lexer.next_token();
            if payload_token.kind != TokenKind::Identifier {
                return Err(ParseError::InvalidExpression(
                    payload_token,
                    "invalid payload for enum constructor".to_string(),
                ));
            }

            let close = lexer.next_token();
            if close.kind != TokenKind::CloseParen {
                return Err(ParseError::InvalidExpression(
                    payload_token,
                    "missing closing paran for pattern payload".to_string(),
                ));
            }

            payload = Some(payload_token.lexeme_id);
        }

        let name = token.lexeme_id;

        match lexer.lexeme(name).as_str() {
            "_" => Ok(Pattern::Default),
            _ => Ok(Pattern::Identifier(IdentifierPat {
                id: token.source_id,
                module: None,
                name,
                payload,
            })),
        }
    }
}
