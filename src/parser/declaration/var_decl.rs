use crate::ast::{Decl, VarDecl};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::str_store::StrStore;

/// Parses top-level const declarations
///
/// Example: `var status = "OK"`
pub struct VarDeclParselet {
    pub public: bool,
}

impl DeclParselet for VarDeclParselet {
    fn parse(
        &self,
        parser: &DeclParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Decl, ParseError> {
        todo!("need the actual str_store");
        let str_store = StrStore::new();

        let ident = lexer.next(&mut str_store);
        if ident.ty != Ty::Identifier {
            return Err(ParseError::UnexpectedToken(
                ident,
                "Expected var name".to_string(),
            ));
        }

        // Expect '='
        let equal = lexer.next(&mut str_store);
        if equal.ty != Ty::Equal {
            return Err(ParseError::UnexpectedToken(
                equal,
                "Expected '=' after const name".to_string(),
            ));
        }

        let value = parser.parse_expression(lexer)?;

        Ok(Decl::Var(VarDecl {
            public: self.public,
            id: ident.pos,
            name: ident.lexeme,
            value,
        }))
    }
}
