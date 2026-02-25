use crate::ast::{Decl, VarDecl};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, TokenKind};

/// Parses top-level const declarations
///
/// Example: `var status = "OK"`
pub struct VarDeclParselet;

impl DeclParselet for VarDeclParselet {
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
                "Expected var name".to_string(),
            ));
        }

        // Expect '='
        let equal = lexer.next_token();
        if equal.kind != TokenKind::Equal {
            return Err(ParseError::UnexpectedToken(
                equal,
                "Expected '=' after const name".to_string(),
            ));
        }

        let value = parser.parse_expression(lexer)?;

        Ok(Decl::Var(VarDecl {
            id: ident.source_id,
            name: ident.lexeme_id,
            value,
        }))
    }
}
