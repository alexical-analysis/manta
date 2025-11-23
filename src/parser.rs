pub mod lexer;
pub mod parselets;
pub mod types;

use crate::ast::{Expr, UnaryOp};
use crate::parser::lexer::{Lexer, Token, TokenKind};
use parselets::{
    BoolLiteralParselet, FloatLiteralParselet, IdentifierParselet, InfixParselet,
    IntLiteralParselet, NilLiteralParselet, Precedence, PrefixParselet, StringLiteralParselet,
    UnaryOperatorParselet,
};
use std::collections::HashMap;
use std::rc::Rc;

/// Parse error type for the parser core.
#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken(String),
    UnexpectedEof(String),
    Custom(String),
}

// More specific error variants for Phase 2 and beyond
impl ParseError {
    pub fn invalid_integer(lexeme: &str) -> Self {
        ParseError::Custom(format!("Invalid integer literal: {}", lexeme))
    }

    pub fn invalid_float(lexeme: &str) -> Self {
        ParseError::Custom(format!("Invalid float literal: {}", lexeme))
    }

    pub fn invalid_string(msg: &str) -> Self {
        ParseError::Custom(format!("Invalid string literal: {}", msg))
    }
}

/// A minimal Parser core scaffolding. This implements a buffered token stream
/// with lookahead and simple parselet registration. The parselet registries
/// are intentionally simple (Vec-based) to avoid requiring `TokenKind: Hash`.
pub struct Parser {
    lexer: Lexer,
    // small read buffer for lookahead
    read: Vec<Token>,
    // prefix and infix registries stored as HashMaps
    prefix_parselets: HashMap<TokenKind, Rc<dyn PrefixParselet>>,
    infix_parselets: HashMap<TokenKind, Rc<dyn InfixParselet>>,
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
    pub fn register_prefix(&mut self, kind: TokenKind, parselet: Rc<dyn PrefixParselet>) {
        self.prefix_parselets.insert(kind, parselet);
    }

    /// Register an infix parselet for a token kind.
    pub fn register_infix(&mut self, kind: TokenKind, parselet: Rc<dyn InfixParselet>) {
        self.infix_parselets.insert(kind, parselet);
    }

    /// Parse an expression, starting with minimum precedence 0.
    /// This is the public API for expression parsing.
    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_expression_precedence(Precedence::Base)
    }

    /// Parse an expression with a minimum precedence level.
    /// This implements the Pratt parser algorithm.
    pub fn parse_expression_precedence(
        &mut self,
        min_precedence: Precedence,
    ) -> Result<Expr, ParseError> {
        let token = self.consume()?;

        let prefix_opt = self.prefix_parselets.get(&token.kind);
        if prefix_opt.is_none() {
            return Err(ParseError::UnexpectedToken(format!(
                "No prefix parselet for token kind: {:?}",
                token.kind
            )));
        };
        let prefix = prefix_opt.unwrap().clone();

        let mut left = prefix.parse(self, token)?;

        // Loop while the next token's precedence is higher than min_precedence
        loop {
            let next_token = self.lookahead(0)?.clone();
            if next_token.kind == TokenKind::Eof {
                break;
            }

            let next_precedence = self.get_precedence(&next_token.kind)?;

            if next_precedence < min_precedence {
                break;
            }

            let token = self.consume()?;
            let infix_opt = self.infix_parselets.get(&token.kind);
            if infix_opt.is_none() {
                return Err(ParseError::UnexpectedToken(format!(
                    "No infix parselet for token kind: {:?}",
                    token.kind
                )));
            }
            let infix = infix_opt.unwrap().clone();

            left = infix.parse(self, left, token)?;
        }

        Ok(left)
    }

    /// Get the precedence of a token kind
    fn get_precedence(&self, kind: &TokenKind) -> Result<Precedence, ParseError> {
        match self.infix_parselets.get(kind) {
            Some(parselet) => Ok(parselet.precedence()),
            None => Err(ParseError::UnexpectedToken(format!(
                "Unknown token precedence for kind: {:?}",
                kind
            ))),
        }
    }

    /// Register all the parselets for the parser
    pub fn register_parselets(&mut self) {
        // Register prefix parselets for literals
        self.register_prefix(TokenKind::Int, Rc::new(IntLiteralParselet));
        self.register_prefix(TokenKind::Float, Rc::new(FloatLiteralParselet));
        self.register_prefix(TokenKind::Str, Rc::new(StringLiteralParselet));
        self.register_prefix(TokenKind::TrueLiteral, Rc::new(BoolLiteralParselet));
        self.register_prefix(TokenKind::FalseLiteral, Rc::new(BoolLiteralParselet));
        self.register_prefix(TokenKind::NilLiteral, Rc::new(NilLiteralParselet));
        self.register_prefix(TokenKind::Ident, Rc::new(IdentifierParselet));
        self.register_prefix(
            TokenKind::Minus,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Negate,
            }),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, UnaryOp};

    #[test]
    fn lookahead_and_consume_basic() {
        let input = "42";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let token0 = parser.lookahead(0).unwrap();
        assert_eq!(token0.kind, TokenKind::Int);

        let consumed = parser.consume().unwrap();
        assert_eq!(consumed.kind, TokenKind::Int);
    }

    #[test]
    fn lookahead_multiple() {
        let input = "42 + 3.14";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let token0 = parser.lookahead(0).unwrap();
        assert_eq!(token0.kind, TokenKind::Int);

        let token1 = parser.lookahead(1).unwrap();
        assert_eq!(token1.kind, TokenKind::Plus);

        let token2 = parser.lookahead(2).unwrap();
        assert_eq!(token2.kind, TokenKind::Float);
    }

    #[test]
    fn match_token_success() {
        let input = "42";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let matched = parser.match_token(TokenKind::Int).unwrap();
        assert!(matched);

        let eof_or_next = parser.lookahead(0).unwrap();
        assert_eq!(eof_or_next.kind, TokenKind::Eof);
    }

    #[test]
    fn match_token_failure() {
        let input = "42";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let matched = parser.match_token(TokenKind::Str).unwrap();
        assert!(!matched);

        let token = parser.lookahead(0).unwrap();
        assert_eq!(token.kind, TokenKind::Int);
    }

    #[test]
    fn parse_expression_int_literal() {
        let input = "42";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.register_parselets();

        let expr = parser.parse_expression().unwrap();
        match expr {
            Expr::IntLiteral(val) => assert_eq!(val, 42),
            _ => panic!("Expected IntLiteral, got {:?}", expr),
        }
    }

    #[test]
    fn parse_expression_float_literal() {
        let input = "3.14";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.register_parselets();

        let expr = parser.parse_expression().unwrap();
        match expr {
            Expr::FloatLiteral(val) => assert_eq!(val, 3.14),
            _ => panic!("Expected FloatLiteral, got {:?}", expr),
        }
    }

    #[test]
    fn parse_expression_string_literal() {
        let input = r#""hello world""#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.register_parselets();

        let expr = parser.parse_expression().unwrap();
        match expr {
            Expr::StringLiteral(val) => assert_eq!(val, "hello world"),
            _ => panic!("Expected StringLiteral, got {:?}", expr),
        }
    }

    #[test]
    fn parse_expression_bool_true() {
        let input = "true";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.register_parselets();

        let expr = parser.parse_expression().unwrap();
        match expr {
            Expr::BoolLiteral(val) => assert_eq!(val, true),
            _ => panic!("Expected BoolLiteral(true), got {:?}", expr),
        }
    }

    #[test]
    fn parse_expression_bool_false() {
        let input = "false";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.register_parselets();

        let expr = parser.parse_expression().unwrap();
        match expr {
            Expr::BoolLiteral(val) => assert_eq!(val, false),
            _ => panic!("Expected BoolLiteral(false), got {:?}", expr),
        }
    }

    #[test]
    fn parse_expression_nil_literal() {
        let input = "nil";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.register_parselets();

        let expr = parser.parse_expression().unwrap();
        match expr {
            Expr::NilLiteral => (),
            _ => panic!("Expected NilLiteral, got {:?}", expr),
        }
    }

    #[test]
    fn parse_expression_identifier() {
        let input = "myVariable";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.register_parselets();

        let expr = parser.parse_expression().unwrap();
        match expr {
            Expr::Identifier(name) => assert_eq!(name, "myVariable"),
            _ => panic!("Expected Identifier, got {:?}", expr),
        }
    }

    #[test]
    fn parse_expression_identifier_single_char() {
        let input = "x";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.register_parselets();

        let expr = parser.parse_expression().unwrap();
        match expr {
            Expr::Identifier(name) => assert_eq!(name, "x"),
            _ => panic!("Expected Identifier, got {:?}", expr),
        }
    }

    #[test]
    fn parse_expression_negative_int() {
        let input = "-42";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.register_parselets();

        let expr = parser.parse_expression().unwrap();
        match expr {
            Expr::UnaryExpr(unary) => {
                assert_eq!(unary.operator, UnaryOp::Negate);
                match *unary.operand {
                    Expr::IntLiteral(val) => assert_eq!(val, 42),
                    _ => panic!("Expected IntLiteral in UnaryExpr"),
                }
            }
            _ => panic!("Expected UnaryExpr, got {:?}", expr),
        }
    }

    #[test]
    fn parse_expression_no_prefix_parselet() {
        let input = "+";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.register_parselets();

        let result = parser.parse_expression();
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::UnexpectedToken(msg) => {
                assert!(msg.contains("No prefix parselet"));
            }
            _ => panic!("Expected UnexpectedToken error"),
        }
    }
}
