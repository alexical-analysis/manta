use crate::ast::{AssignStmt, ShortLetStmt, Stmt};
use crate::parser::lexer::{Token, TokenKind};
use crate::parser::parselets::PrefixStmtParselet;
use crate::parser::{ParseError, Parser};

/// Parses assignment statements
///
/// Example: `x = 10`
pub struct AssignParselet;

impl PrefixStmtParselet for AssignParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> Result<Stmt, ParseError> {
        // eat the '='/':=' token which are check in the matches method
        let assign_token = parser.consume()?;

        let expr = parser.parse_expression()?;

        match assign_token.kind {
            TokenKind::Equal => Ok(Stmt::Assign(AssignStmt {
                name: token.lexeme,
                value: expr,
            })),
            TokenKind::ColonEqual => Ok(Stmt::ShortLet(ShortLetStmt {
                name: token.lexeme,
                value: expr,
            })),
            _ => unreachable!("this code should be unreachable"),
        }
    }

    fn matches(&self, parser: &mut Parser) -> bool {
        let first = parser.lookahead(0);
        let first = if first.is_ok() {
            first.unwrap()
        } else {
            return false;
        };

        if first.kind != TokenKind::Identifier {
            return false;
        }

        let second = parser.lookahead(1);
        let second = if second.is_ok() {
            second.unwrap()
        } else {
            return false;
        };

        match second.kind {
            TokenKind::Equal => true,
            TokenKind::ColonEqual => true,
            _ => false,
        }
    }
}
