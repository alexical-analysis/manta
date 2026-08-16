use crate::ast::{ConstDecl, Decl};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::str_store::{self, StrStore};

/// Parses top-level const declarations
///
/// Example: `const PI = 3.14159`
pub struct ConstDeclParselet {
    pub public: bool,
}

impl DeclParselet for ConstDeclParselet {
    fn parse(
        &self,
        parser: &DeclParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Decl, ParseError> {
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let ident = lexer.next(&mut str_store);
        if ident.ty != Ty::Identifier {
            return Err(ParseError::UnexpectedToken(
                ident,
                "Expected const name".to_string(),
            ));
        }

        let name = ident.lexeme;

        // Expect '='
        let equal = lexer.next(&mut str_store);
        if equal.ty != Ty::Equal {
            return Err(ParseError::UnexpectedToken(
                equal,
                "Expected '=' after const name".to_string(),
            ));
        }

        let value = parser.parse_expression(lexer)?;

        Ok(Decl::Const(ConstDecl {
            public: self.public,
            id: ident.pos,
            name,
            value,
        }))
    }
}
