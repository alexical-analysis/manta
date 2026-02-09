use crate::ast::{MatchArm, MatchStmt, Stmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};

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
        let target = parser.parse_expression(lexer)?;

        let token = lexer.next_token();
        if token.kind != TokenKind::OpenBrace {
            return Err(ParseError::UnexpectedToken(
                token,
                "Expected '{' after match expression".to_string(),
            ));
        }

        let mut arms = vec![];

        loop {
            let token = lexer.peek();
            if token.kind == TokenKind::CloseBrace {
                lexer.next_token();
                break;
            }
            if token.kind == TokenKind::Eof {
                return Err(ParseError::UnexpectedToken(
                    token,
                    "missing closing '}' in match block".to_string(),
                ));
            }

            let pattern = parser.parse_pattern(lexer)?;

            let token = lexer.next_token();
            if token.kind != TokenKind::OpenBrace {
                return Err(ParseError::UnexpectedToken(
                    token,
                    "Expected '{' after pattern in match arm".to_string(),
                ));
            }

            let body = parser.parse_block(lexer, token)?;

            let token = lexer.next_token();
            if token.kind != TokenKind::Semicolon {
                return Err(ParseError::UnexpectedToken(
                    token,
                    "Expected ';' after body in match arm".to_string(),
                ));
            }

            arms.push(MatchArm { pattern, body });
        }

        if arms.is_empty() {
            return Err(ParseError::UnexpectedToken(
                lexer.peek(),
                "match statement must have at least one arm".to_string(),
            ));
        }

        Ok(Stmt::Match(MatchStmt { target, arms }))
    }
}
