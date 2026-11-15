use crate::ast::Decl;
use crate::parser::ParseError;
use crate::parser::declaration::const_decl::ConstDeclParselet;
use crate::parser::declaration::function_declaration::FunctionDeclParselet;
use crate::parser::declaration::type_decl::TypeDeclParselet;
use crate::parser::declaration::var_decl::VarDeclParselet;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, TokenKind};

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
        let token = lexer.next_token();
        match token.kind {
            TokenKind::FnKeyword => self.fn_parselet.parse(parser, lexer, token),
            TokenKind::TypeKeyword => self.type_parselet.parse(parser, lexer, token),
            TokenKind::ConstKeyword => self.const_parselet.parse(parser, lexer, token),
            TokenKind::VarKeyword => self.var_parselet.parse(parser, lexer, token),
            _ => Err(ParseError::UnexpectedToken(
                token,
                "pub keyword can only appear in specific places".to_string(),
            )),
        }
    }
}
