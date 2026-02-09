use crate::ast::{IfStmt, Stmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};

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
        let check = parser.parse_expression(lexer)?;
        let check = Box::new(check);

        let token = lexer.next_token();
        if token.kind != TokenKind::OpenBrace {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected '{' after if check".to_string(),
            ));
        }

        let success = parser.parse_block(lexer, token)?;

        let next = lexer.peek();
        let fail = if next.kind == TokenKind::ElseKeyword {
            lexer.next_token();
            let open = lexer.next_token();
            if open.kind != TokenKind::OpenBrace {
                return Err(ParseError::UnexpectedToken(
                    next,
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
