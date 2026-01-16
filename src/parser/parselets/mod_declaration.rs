use crate::ast::{Decl, ModDecl};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixDeclParselet;
use crate::parser::{ParseError, Parser};

/// Parses top-level import declarations
///
/// Example: `mod main`
/// Example: `mod math`
pub struct ModDeclParselet;

impl PrefixDeclParselet for ModDeclParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Decl, ParseError> {
        let name = parser.consume()?;
        if name.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                name.clone(),
                "Missing module name".to_string(),
            ));
        }

        Ok(Decl::Mod(ModDecl { name: name.lexeme }))
    }
}
