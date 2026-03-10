use crate::ast::{EnumVariantPat, IdentifierExpr, Pattern};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::pattern::{InfixPatternParselet, PatternParser, PrefixPatternParselet};

/// Parses dot patterns where the dot is the prefix.
///
/// Example: `.Ok`
/// Example: `.Err`
pub struct PrefixEnumVariantParselet;

impl PrefixPatternParselet for PrefixEnumVariantParselet {
    fn parse(&self, lexer: &mut Lexer, _token: Token) -> Result<Pattern, ParseError> {
        let variant_token = lexer.next_token();
        if variant_token.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                variant_token,
                "variant name required after '.'".to_string(),
            ));
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

        Ok(Pattern::EnumVariant(EnumVariantPat {
            target: None,
            variant: variant_token.lexeme_id,
            payload,
        }))
    }
}

/// Parses enum variant patterns where the dot is the infix.
///
/// Example: `Ret.Ok`
/// Example: `Ret.Err`
pub struct InfixEnumVariantParselet;

impl InfixPatternParselet for InfixEnumVariantParselet {
    fn parse(
        &self,
        _parser: &PatternParser,
        lexer: &mut Lexer,
        left: Pattern,
        token: Token,
    ) -> Result<Pattern, ParseError> {
        eprintln!("parsing enum variant {:?} {:?}", left, token);
        let variant_token = lexer.next_token();
        if variant_token.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                variant_token,
                "variant name required after '.'".to_string(),
            ));
        };

        let left = match left {
            Pattern::Identifier(ident) => {
                if ident.payload.is_some() {
                    return Err(ParseError::UnexpectedToken(
                        token,
                        "unexpected payload on variant target".to_string(),
                    ));
                }

                IdentifierExpr {
                    id: ident.id,
                    module: ident.module,
                    name: ident.name,
                }
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

        Ok(Pattern::EnumVariant(EnumVariantPat {
            target: Some(left),
            variant: variant_token.lexeme_id,
            payload,
        }))
    }
}
