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
        let token = lexer.next_token();
        if token.kind != TokenKind::Identifier {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected var name".to_string(),
            ));
        }

        let name = lexer.lexeme(token);

        // Expect '='
        let token = lexer.next_token();
        if token.kind != TokenKind::Equal {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected '=' after const name".to_string(),
            ));
        }

        let value = parser.parse_expression(lexer)?;

        Ok(Decl::Var(VarDecl { name, value }))
    }
}
