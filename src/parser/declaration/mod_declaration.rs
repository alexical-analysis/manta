use crate::ast::{Decl, ModDecl};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, TokenKind};

/// Parses top-level import declarations
///
/// Example: `mod main`
/// Example: `mod math`
pub struct ModDeclParselet;

impl DeclParselet for ModDeclParselet {
    fn parse(
        &self,
        _parser: &DeclParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Decl, ParseError> {
        let token = lexer.next_token();
        if token.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                token,
                "Missing module name".to_string(),
            ));
        }

        let name = lexer.lexeme(token);
        Ok(Decl::Mod(ModDecl { name }))
    }
}
