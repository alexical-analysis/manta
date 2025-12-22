use crate::ast::{MatchArm, MatchStmt, Pattern, Stmt};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixStmtParselet;
use crate::parser::{ParseError, Parser, statement};

/// Parses match statements
///
/// Example: `match x { .Some(v) { print(v) } .None { print("none") } }`
pub struct MatchParselet;

impl PrefixStmtParselet for MatchParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Stmt, ParseError> {
        let target = parser.parse_expression()?;

        let matched = parser.match_token(TokenKind::OpenBrace)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                "Expected '{' after match expression".to_string(),
            ));
        }

        let mut arms = vec![];

        loop {
            let matched = parser.match_token(TokenKind::CloseBrace)?;
            if matched {
                break;
            }

            let matches = parser.match_token(TokenKind::Eof)?;
            if matches {
                return Err(ParseError::UnexpectedToken(
                    "missing closing '}' in match block".to_string(),
                ));
            }

            let pattern = parse_pattern(parser)?;

            let matched = parser.match_token(TokenKind::OpenBrace)?;
            if !matched {
                return Err(ParseError::UnexpectedToken(
                    "Expected '{' after pattern in match arm".to_string(),
                ));
            }

            let body = statement::parse_block(parser)?;

            arms.push(MatchArm { pattern, body });
        }

        if arms.is_empty() {
            return Err(ParseError::UnexpectedToken(
                "match statement must have at least one arm".to_string(),
            ));
        }

        Ok(Stmt::Match(MatchStmt { target, arms }))
    }
}

/// Parse a pattern in a match arm
fn parse_pattern(parser: &mut Parser) -> Result<Pattern, ParseError> {
    let token = parser.lookahead(0)?;

    match token.kind {
        TokenKind::Dot => {
            parser.consume()?;
            parse_enum_variant(parser)
        }
        _ => Err(ParseError::UnexpectedToken(
            "Expected pattern (e.g., '.Variant' or '.Variant(binding)')".to_string(),
        )),
    }
}

/// Parse enum variant pattern: .Variant or .Variant(binding)
fn parse_enum_variant(parser: &mut Parser) -> Result<Pattern, ParseError> {
    let token = parser.lookahead(0)?;

    if token.kind != TokenKind::Identifier {
        return Err(ParseError::UnexpectedToken(
            "Expected identifier after '.' in enum variant pattern".to_string(),
        ));
    }

    let variant_token = parser.consume()?;
    let name = variant_token.lexeme;

    let mut payload_binding = None;

    // Check for optional payload binding: (identifier)
    if parser.match_token(TokenKind::OpenParen)? {
        let binding_token = parser.lookahead(0)?;

        if binding_token.kind == TokenKind::CloseParen {
            // Empty parens case
            parser.consume()?;
        } else if binding_token.kind == TokenKind::Identifier {
            let ident_token = parser.consume()?;
            payload_binding = Some(ident_token.lexeme);

            let matched = parser.match_token(TokenKind::CloseParen)?;
            if !matched {
                return Err(ParseError::UnexpectedToken(
                    "Expected ')' after binding identifier in enum pattern".to_string(),
                ));
            }
        } else {
            return Err(ParseError::UnexpectedToken(
                "Expected identifier or ')' in enum pattern payload".to_string(),
            ));
        }
    }

    Ok(Pattern::EnumVariant {
        name,
        payload_binding,
    })
}
