use crate::ast::Decl;
use crate::parser::ParseError;
use crate::parser::declaration::const_decl::ConstDeclParselet;
use crate::parser::declaration::function_declaration::FunctionDeclParselet;
use crate::parser::declaration::type_decl::TypeDeclParselet;
use crate::parser::declaration::var_decl::VarDeclParselet;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::str_store::StrStore;

/// Parses top-level `pub` declarations
///
/// Example: `pub fn test() { .. }`
/// Example: `pub type Vec2 struct { .. }`
pub struct PubParselet {
    fn_parselet: FunctionDeclParselet,
    type_parselet: TypeDeclParselet,
    const_parselet: ConstDeclParselet,
    var_parselet: VarDeclParselet,
}

impl PubParselet {
    pub fn new() -> Self {
        PubParselet {
            fn_parselet: FunctionDeclParselet { public: true },
            type_parselet: TypeDeclParselet { public: true },
            const_parselet: ConstDeclParselet { public: true },
            var_parselet: VarDeclParselet { public: true },
        }
    }
}

impl DeclParselet for PubParselet {
    fn parse(
        &self,
        parser: &DeclParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Decl, ParseError> {
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let token = lexer.next(&mut str_store);
        match token.ty {
            Ty::FnKeyword => self.fn_parselet.parse(parser, lexer, token),
            Ty::TypeKeyword => self.type_parselet.parse(parser, lexer, token),
            Ty::ConstKeyword => self.const_parselet.parse(parser, lexer, token),
            // Ty::VarKeyword => self.var_parselet.parse(parser, lexer, token),
            _ => Err(ParseError::UnexpectedToken(
                token,
                "pub keyword can only appear in specific places".to_string(),
            )),
        }
    }
}
