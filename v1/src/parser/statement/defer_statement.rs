use crate::ast::{DeferStmt, Stmt};
use crate::parser::ParseError;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use crate::parser::statement::{PrefixStmtParselet, StmtParser};

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
        let token = lexer.next_token();
        if token.kind != TokenKind::OpenBrace {
            return Err(ParseError::UnexpectedToken(
                token,
                "block must start with '{'".to_string(),
            ));
        }

        let block = parser.parse_block(lexer, token)?;

        Ok(Stmt::Defer(DeferStmt { block }))
    }
}
