use crate::ast::{Expr, ForStmt, LoopStmt, Stmt, WhileStmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, Ty};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};
use crate::str_store::StrStore;

/// Parses a loop statement
///
/// Example `loop { .. }`
pub struct LoopParselet;

impl PrefixStmtParselet for LoopParselet {
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Stmt, ParseError> {
        // parse_block expects the first opening brace to already be consumed
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let open = lexer.next(&mut str_store);
        if open.ty != Ty::OpenBrace {
            return Err(ParseError::Custom(
                open,
                "the loop keyword must be followed by a block".to_string(),
            ));
        }

        let body = parser.parse_block(lexer, token)?;

        Ok(Stmt::Loop(LoopStmt { body }))
    }
}

/// Parses a while statement
///
/// Example `while ok { .. }`
pub struct WhileParselet;

impl PrefixStmtParselet for WhileParselet {
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        token: Token,
    ) -> Result<Stmt, ParseError> {
        let check = parser.parse_no_struct_expression(lexer)?;
        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        // parse_block expects the first opening brace to already be consumed
        let open = lexer.next(&mut str_store);
        if open.ty != Ty::OpenBrace {
            return Err(ParseError::Custom(
                open,
                "the loop keyword must be followed by a block".to_string(),
            ));
        }

        let body = parser.parse_block(lexer, token)?;

        Ok(Stmt::While(WhileStmt {
            check: Box::new(check),
            body,
        }))
    }
}

/// Parse a for statement
///
/// Example `for x in 0..=10 { .. }`
/// Example `for i in 3..<7 { .. }`
pub struct ForParselet;

impl PrefixStmtParselet for ForParselet {
    fn parse(
        &self,
        parser: &StmtParser,
        lexer: &mut Lexer,
        _token: Token,
    ) -> Result<Stmt, ParseError> {
        let token = lexer.peek().clone();
        let binding = parser.parse_expression(lexer)?;
        let binding = match binding {
            Expr::Identifier(ident) => ident,
            _ => {
                return Err(ParseError::InvalidExpression(
                    token.clone(),
                    "for look requires a simple binding identifier".to_string(),
                ));
            }
        };

        todo!("need to figure this out");
        let mut str_store = StrStore::new();

        let in_keyword = lexer.next(&mut str_store);
        if in_keyword.ty != Ty::InKeyword {
            return Err(ParseError::UnexpectedToken(
                in_keyword,
                "missing in keyword in for loop".to_string(),
            ));
        }

        // get the range expression
        let token = lexer.peek();
        let range = parser.parse_no_struct_expression(lexer)?;
        let range = match range {
            Expr::Range(range) => range,
            _ => {
                return Err(ParseError::InvalidExpression(
                    token.clone(),
                    "for loops require range expressions".to_string(),
                ));
            }
        };

        // parse_block expects the first opening brace to already be consumed
        let open = lexer.next(&mut str_store);
        if open.ty != Ty::OpenBrace {
            return Err(ParseError::Custom(
                open,
                "the loop keyword must be followed by a block".to_string(),
            ));
        }

        let body = parser.parse_block(lexer, token.clone())?;

        Ok(Stmt::For(ForStmt {
            binding,
            range,
            body,
        }))
    }
}
