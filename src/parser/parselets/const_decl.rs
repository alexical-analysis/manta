use crate::ast::{ConstDecl, Decl};
use crate::parser::expression;
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::Precedence;
use crate::parser::parselets::PrefixDeclParselet;
use crate::parser::{ParseError, Parser};

/// Parses top-level const declarations
///
/// Example: `const PI = 3.14159`
pub struct ConstDeclParselet;

impl PrefixDeclParselet for ConstDeclParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Decl, ParseError> {
        // Expect const name
        let name_token = parser.lookahead(0)?;
        if name_token.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                name_token.clone(),
                "Expected const name".to_string(),
            ));
        }

        let name = parser.consume()?.lexeme;

        // Expect '='
        let matched = parser.match_token(TokenKind::Equal)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                parser.lookahead(0)?.clone(),
                "Expected '=' after const name".to_string(),
            ));
        }

        // Parse the expression
        let value = expression::parse_expression(parser, Precedence::Base)?;

        Ok(Decl::Const(ConstDecl { name, value }))
    }
}
