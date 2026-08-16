use crate::ast::{DeferStmt, Stmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};
use crate::str_store::StrStore;

/// Parses defer statements.
///
/// Example: `defer { free(ptr) }`
pub struct DeferParselet;

impl PrefixStmtParselet for DeferParselet {
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Stmt, ParseError> {
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let token = lexer.next(&mut str_store);
        if token.ty != Ty::OpenBrace {
            return Err(ParseError::UnexpectedToken(
                token,
                "block must start with '{'".to_string(),
            ));
        }

        let block = parser.parse_block(lexer, token)?;

        Ok(Stmt::Defer(DeferStmt { block }))
    }
}
