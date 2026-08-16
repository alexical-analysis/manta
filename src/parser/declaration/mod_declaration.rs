use crate::ast::{Decl, ModDecl};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::str_store::StrStore;

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
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let token = lexer.next(&mut str_store);
        if token.ty != Ty::Identifier {
            return Err(ParseError::UnexpectedToken(
                token,
                "Missing module name".to_string(),
            ));
        }

        let name = token.lexeme;
        Ok(Decl::Mod(ModDecl { name }))
    }
}
