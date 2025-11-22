pub mod expression;
pub mod lexer;
pub mod parselets;
pub mod statements;
pub mod types;

use crate::ast::Expr;
use crate::parser::lexer::{Lexer, Token, TokenKind};
use parselets::{InfixParselet, PrefixParselet};
use serde::{Deserialize, Serialize};

use std::collections::HashMap;

/// Parse error type for the parser core.
#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken(String),
    UnexpectedEof(String),
    Custom(String),
}

// Operator precedence levels.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    ASSIGNMENT,
    LOGICAL_OR,
    LOGICAL_AND,
    EQUALITY,
    COMPARISON,
    ADDITIVE,
    MULTIPLICATIVE,
    EXPONENT,
    PREFIX,
    CALL,
}

/// A minimal Parser core scaffolding. This implements a buffered token stream
/// with lookahead and simple parselet registration. The parselet registries
/// are intentionally simple (Vec-based) to avoid requiring `TokenKind: Hash`.
pub struct Parser {
    lexer: Lexer,
    // small read buffer for lookahead
    read: Vec<Token>,
    // prefix and infix registries stored as HashMaps
    prefix_parselets: HashMap<TokenKind, Box<dyn PrefixParselet>>,
    infix_parselets: HashMap<TokenKind, Box<dyn InfixParselet>>,
}

impl Parser {
    /// Create a new parser from a lexer
    pub fn new(lexer: Lexer) -> Self {
        Parser {
            lexer,
            read: Vec::new(),
            prefix_parselets: HashMap::new(),
            infix_parselets: HashMap::new(),
        }
    }

    /// Ensure we have at least `distance + 1` tokens buffered and return a reference
    /// to the token at `distance` (0-based).
    pub fn lookahead(&mut self, distance: usize) -> Result<&Token, ParseError> {
        while self.read.len() <= distance {
            match self.lexer.next_token() {
                Ok(tok) => self.read.push(tok),
                Err(e) => return Err(ParseError::Custom(format!("Lexer error: {}", e))),
            }
        }
        Ok(&self.read[distance])
    }

    /// Consume and return the next token.
    pub fn consume(&mut self) -> Result<Token, ParseError> {
        // ensure at least one token
        self.lookahead(0)?;
        Ok(self.read.remove(0))
    }

    /// Match current token kind against expected; if matches consume and return true.
    pub fn match_token(&mut self, kind: TokenKind) -> Result<bool, ParseError> {
        let tk = self.lookahead(0)?;
        if tk.kind == kind {
            let _ = self.consume()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Register a prefix parselet for a token kind.
    pub fn register_prefix(&mut self, kind: TokenKind, parselet: Box<dyn PrefixParselet>) {
        self.prefix_parselets.insert(kind, parselet);
    }

    /// Register an infix parselet for a token kind.
    pub fn register_infix(&mut self, kind: TokenKind, parselet: Box<dyn InfixParselet>) {
        self.infix_parselets.insert(kind, parselet);
    }

    // Placeholder for parse_expression - will be implemented in Phase 2+.
    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        Err(ParseError::Custom(
            "parse_expression not implemented".to_string(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookahead_and_consume_basic() {
        let src = "let x = 1";
        let lexer = Lexer::new(src);
        let mut parser = Parser::new(lexer);

        // lookahead should provide first token
        let t0 = parser.lookahead(0).expect("lookahead0");
        assert_eq!(t0.kind, TokenKind::LetKeyword);

        // consume and then next token
        let _ = parser.consume().expect("consume");
        let t1 = parser.lookahead(0).expect("lookahead1");
        assert_eq!(t1.kind, TokenKind::Ident);
    }
}
