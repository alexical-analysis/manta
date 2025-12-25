use crate::ast::{Decl, ImportDecl};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixDeclParselet;
use crate::parser::{ParseError, Parser};

/// Parses top-level import declarations
///
/// Example: `import ("math"\n"io")`
pub struct ImportDeclParselet;

impl PrefixDeclParselet for ImportDeclParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> Result<Decl, ParseError> {
        // Expect opening paren
        let matched = parser.match_token(TokenKind::OpenParen)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                "Expected '(' after import keyword".to_string(),
            ));
        }

        // Parse module names
        let modules = parse_import_modules(parser)?;

        // Expect closing paren
        let matched = parser.match_token(TokenKind::CloseParen)?;
        if !matched {
            return Err(ParseError::UnexpectedToken(
                "Expected ')' after import modules".to_string(),
            ));
        }

        Ok(Decl::Import(ImportDecl { modules }))
    }
}

/// Parse the list of module names in an import statement
/// Syntax: STRING+
/// Similar to Go imports, no commas needed
fn parse_import_modules(parser: &mut Parser) -> Result<Vec<String>, ParseError> {
    let mut modules = vec![];

    // Parse module names until closing paren
    loop {
        let tok = parser.lookahead(0)?;
        if tok.kind != TokenKind::Str {
            break;
        }

        modules.push(parser.consume()?.lexeme);
    }

    if modules.is_empty() {
        return Err(ParseError::UnexpectedToken(
            "Expected at least one module name string in import".to_string(),
        ));
    }

    Ok(modules)
}
