use crate::ast::{EnumVariantPat, IdentifierExpr, IdentifierPat, Pattern};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::{InfixPatternParselet, PatternParser, PrefixPatternParselet};

/// Parses dot patterns where the dot is the prefix.
///
/// Example: `.Ok`
/// Example: `.Err`
pub struct PrefixDotPatternParselet;

impl PrefixPatternParselet for PrefixDotPatternParselet {
    fn parse(&self, lexer: &mut Lexer, token: Token) -> Result<Pattern, ParseError> {
        let field_token = lexer.next_token();

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

            payload = Some(payload_token.lexeme_id)
        }

        match field_token.kind {
            TokenKind::Identifier => Ok(Pattern::EnumVariant(EnumVariantPat {
                id: token.source_id,
                enum_name: None,
                variant: field_token.lexeme_id,
                payload,
            })),
            _ => Err(ParseError::UnexpectedToken(
                field_token,
                "field name required after '.'".to_string(),
            )),
        }
    }
}

/// Parses dot patterns where the dot is the infix.
///
/// Example: `Ret.Ok`
/// Example: `Ret.Err`
pub struct InfixDotPatternParselet;

impl InfixPatternParselet for InfixDotPatternParselet {
    fn parse(
        &self,
        _parser: &PatternParser,
        lexer: &mut Lexer,
        left: Pattern,
        token: Token,
    ) -> Result<Pattern, ParseError> {
        let left = match left {
            Pattern::Identifier(ident) => ident,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    token,
                    "enum variant pattern must use an identifier for the variant".to_string(),
                ));
            }
        };

        let field_token = lexer.next_token();

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

            payload = Some(payload_token.lexeme_id)
        }

        match field_token.kind {
            TokenKind::Identifier => Ok(Pattern::EnumVariant(EnumVariantPat {
                id: token.source_id,
                enum_name: Some(IdentifierExpr {
                    id: left.id,
                    module: left.module,
                    name: left.name,
                }),
                variant: field_token.lexeme_id,
                payload,
            })),
            _ => Err(ParseError::UnexpectedToken(
                field_token,
                "field name required after '.'".to_string(),
            )),
        }
    }
}
