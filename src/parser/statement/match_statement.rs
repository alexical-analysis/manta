use crate::ast::{MatchArm, MatchStmt, Stmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};
use crate::str_store::StrStore;

/// Parses match statements
///
/// Example: `match x { .Some(v) { print(v) } .None { print("none") } }`
pub struct MatchParselet;

impl PrefixStmtParselet for MatchParselet {
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Stmt, ParseError> {
        let target = parser.parse_no_struct_expression(lexer)?;
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let token = lexer.next(&mut str_store);
        if token.ty != Ty::OpenBrace {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected '{' after match expression".to_string(),
            ));
        }

        let mut arms = vec![];

        loop {
            let token = lexer.peek();
            if token.ty == Ty::CloseBrace {
                lexer.next(&mut str_store);
                break;
            }
            if token.ty == Ty::Eof {
                return Err(ParseError::UnexpectedToken(
                    token.clone(),
                    "missing closing '}' in match block".to_string(),
                ));
            }

            let pattern = parser.parse_pattern(lexer)?;

            let next = lexer.next(&mut str_store);
            if next.ty != Ty::OpenBrace {
                return Err(ParseError::UnexpectedToken(
                    next,
                    "Expected '{' after pattern in match arm".to_string(),
                ));
            }

            let body = parser.parse_block(lexer, next)?;

            let next = lexer.next(&mut str_store);
            if next.ty != Ty::Semicolon {
                return Err(ParseError::UnexpectedToken(
                    token.clone(),
                    "Expected ';' after body in match arm".to_string(),
                ));
            }

            arms.push(MatchArm {
                id: token.pos,
                pattern,
                body,
            });
        }

        if arms.is_empty() {
            return Err(ParseError::UnexpectedToken(
                lexer.peek().clone(),
                "match statement must have at least one arm".to_string(),
            ));
        }

        Ok(Stmt::Match(MatchStmt { target, arms }))
    }
}
