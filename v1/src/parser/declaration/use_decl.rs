use crate::ast::{Decl, ImportStatement, UseDecl};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::str_store::StrID;

/// Parses top-level import declarations
///
/// Example: `import ("math"\n"io")`
pub struct UseDeclParselet;

impl DeclParselet for UseDeclParselet {
    fn parse(
        &self,
        _parser: &DeclParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Decl, ParseError> {
        let token = lexer.next_token();
        if token.kind != TokenKind::OpenParen {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected '(' after import keyword".to_string(),
            ));
        }

        let modules = parse_import_modules(lexer)?;

        let token = lexer.next_token();
        if token.kind != TokenKind::CloseParen {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected ')' after import modules".to_string(),
            ));
        }

        Ok(Decl::Use(UseDecl { modules }))
    }
}

/// Parse the list of module names in an import statement
/// Syntax: STRING+
/// Similar to Go imports, no commas needed
fn parse_import_modules(lexer: &mut Lexer) -> Result<Vec<ImportStatement>, ParseError> {
    let mut modules = vec![];

    // Parse module names until closing paren
    loop {
        if lexer.peek().kind != TokenKind::Str {
            break;
        }

        let token = lexer.next_token();
        let import_path = token.lexeme_id;

        // check if this import is aliased
        let mut alias = None;
        if lexer.peek().kind == TokenKind::AsKeyword {
            lexer.next_token();
            let alias_token = lexer.next_token();
            if alias_token.kind != TokenKind::Identifier {
                return Err(ParseError::UnexpectedToken(
                    alias_token,
                    "Expected identifer as package alias".to_string(),
                ));
            }

            alias = Some(alias_token.lexeme_id);
        }

        let token = lexer.next_token();
        if token.kind != TokenKind::Semicolon {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected ';'".to_string(),
            ));
        }

        modules.push(ImportStatement {
            path: import_path,
            alias,
        });
    }

    if modules.is_empty() {
        return Err(ParseError::UnexpectedToken(
            lexer.peek(),
            "Expected at least one module name string in import".to_string(),
        ));
    }

    Ok(modules)
}
