use crate::ast::{LoopStmt, Stmt, WhileStmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};

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
        let open = lexer.next_token();
        if open.kind != TokenKind::OpenBrace {
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

        // parse_block expects the first opening brace to already be consumed
        let open = lexer.next_token();
        if open.kind != TokenKind::OpenBrace {
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
