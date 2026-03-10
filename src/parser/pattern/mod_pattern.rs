use crate::ast::{IdentifierPat, Pattern};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::{InfixPatternParselet, PatternParser};

/// Parses module access patterns
///
/// Example: `math::Vec3`
pub struct ModPatternParselet;

impl InfixPatternParselet for ModPatternParselet {
    fn parse(
        &self,
        _parser: &PatternParser,
        lexer: &mut Lexer,
        left: Pattern,
        token: Token,
    ) -> Result<Pattern, ParseError> {
        let left = match left {
            Pattern::Identifier(ident) => {
                if ident.payload.is_some() {
                    return Err(ParseError::UnexpectedToken(
                        token,
                        "unexpected payload on module name".to_string(),
                    ));
                }

                if ident.module.is_some() {
                    return Err(ParseError::UnexpectedToken(
                        token,
                        "unexpected module on modulel name".to_string(),
                    ));
                }

                ident.name
            }
            _ => {
                return Err(ParseError::InvalidExpression(
                    token,
                    "left hand side of an enum pattern must be an identifier".to_string(),
                ));
            }
        };

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

        Ok(Pattern::Identifier(IdentifierPat {
            id: token.source_id,
            module: Some(left),
            name,
            payload,
        }))
    }
}
