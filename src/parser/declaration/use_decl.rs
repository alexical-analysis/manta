use crate::ast::{Decl, ImportStatement, UseDecl};
use crate::parser::ParseError;
use crate::parser::declaration::{DeclParselet, DeclParser};
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::str_store::{StrID, StrStore};

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
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let token = lexer.next(&mut str_store);
        if token.ty != Ty::OpenParen {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected '(' after import keyword".to_string(),
            ));
        }

        let modules = parse_import_modules(lexer)?;

        let token = lexer.next(&mut str_store);
        if token.ty != Ty::CloseParen {
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
    todo!("need to figure this out");
    let mut str_store = StrStore::new();

    // Parse module names until closing paren
    loop {
        if lexer.peek().ty != Ty::Str {
            break;
        }

        let token = lexer.next(&mut str_store);
        let import_path = token.lexeme;

        // check if this import is aliased
        let mut alias = None;
        if lexer.peek().ty == Ty::AsKeyword {
            lexer.next(&mut str_store);
            let alias_token = lexer.next(&mut str_store);
            if alias_token.ty != Ty::Identifier {
                return Err(ParseError::UnexpectedToken(
                    alias_token,
                    "Expected identifer as package alias".to_string(),
                ));
            }

            alias = Some(alias_token.lexeme);
        }

        let token = lexer.next(&mut str_store);
        if token.ty != Ty::Semicolon {
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
            lexer.peek().clone(),
            "Expected at least one module name string in import".to_string(),
        ));
    }

    Ok(modules)
}
