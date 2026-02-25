use crate::ast::{ConstDecl, Decl};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, TokenKind};

/// Parses top-level const declarations
///
/// Example: `const PI = 3.14159`
pub struct ConstDeclParselet;

impl DeclParselet for ConstDeclParselet {
    fn parse(
        &self,
        parser: &DeclParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Decl, ParseError> {
        let ident = lexer.next_token();
        if ident.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                ident,
                "Expected const name".to_string(),
            ));
        }

        let name = ident.lexeme_id;

        // Expect '='
        let equal = lexer.next_token();
        if equal.kind != TokenKind::Equal {
            return Err(ParseError::UnexpectedToken(
                equal,
                "Expected '=' after const name".to_string(),
            ));
        }

        let value = parser.parse_expression(lexer)?;

        Ok(Decl::Const(ConstDecl {
            id: ident.source_id,
            name,
            value,
        }))
    }
}
