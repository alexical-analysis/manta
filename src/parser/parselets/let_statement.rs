use crate::ast::{LetStmt, Stmt};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixStmtParselet;
use crate::parser::types;
use crate::parser::{ParseError, Parser};

/// Parses let statements.
///
/// Example: `let x i32`
pub struct LetParselet;

impl PrefixStmtParselet for LetParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Stmt, ParseError> {
        let ident = parser.consume()?;
        if ident.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                "let statment required an identifier".to_string(),
            ));
        }

        let type_spec;
        let mut matched = parser.match_token(TokenKind::Equal)?;
        if matched {
            type_spec = None;
        } else {
            type_spec = Some(types::parse_type(parser)?);
            matched = parser.match_token(TokenKind::Equal)?;
        }

        if !matched && type_spec.is_none() {
            return Err(ParseError::UnexpectedToken(
                "let statement must have either a type spec or initialization value".to_string(),
            ));
        }

        // this means the statment follows the format of
        // `let x i32`
        if !matched {
            return Ok(Stmt::Let(LetStmt {
                name: ident.lexeme,
                type_annotation: type_spec,
                value: None,
            }));
        }

        let value = parser.parse_expression()?;

        // this covers statements like `let x = 5` as well as
        // `let x bool = true`
        Ok(Stmt::Let(LetStmt {
            name: ident.lexeme,
            type_annotation: type_spec,
            value: Some(value),
        }))
    }

    fn matches(&self, _parser: &mut Parser) -> bool {
        true
    }
}
