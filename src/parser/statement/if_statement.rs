use crate::ast::{IfStmt, Stmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};
use crate::str_store::StrStore;

/// Parses if statements
///
/// Example: `if 10 < 20 { print("ok") }`
pub struct IfParselet;

impl PrefixStmtParselet for IfParselet {
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Stmt, ParseError> {
        let check = parser.parse_no_struct_expression(lexer)?;
        let check = Box::new(check);

        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let token = lexer.next(&mut str_store);
        if token.ty != Ty::OpenBrace {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected '{' after if check".to_string(),
            ));
        }

        let success = parser.parse_block(lexer, token)?;

        let next = lexer.peek();
        let fail = if next.ty == Ty::ElseKeyword {
            lexer.next(&mut str_store);
            let open = lexer.next(&mut str_store);
            if open.ty != Ty::OpenBrace {
                return Err(ParseError::UnexpectedToken(
                    next.clone(),
                    "Expected '{' after else keyword".to_string(),
                ));
            }
            let block = parser.parse_block(lexer, open)?;
            Some(block)
        } else {
            None
        };

        Ok(Stmt::If(IfStmt {
            check,
            success,
            fail,
        }))
    }
}
