pub mod expression;
pub mod lexer;
pub mod parselets;
pub mod statement;
pub mod types;

use crate::ast::{BinaryOp, Expr, Stmt, UnaryOp};
use lexer::{Lexer, Token, TokenKind};
use parselets::{
    BinaryOperatorParselet, BoolLiteralParselet, CallParselet, FieldAccessParselet,
    FloatLiteralParselet, GroupParselet, IdentifierParselet, IndexParselet, InfixParselet,
    IntLiteralParselet, NilLiteralParselet, Precedence, PrefixParselet, StringLiteralParselet,
    UnaryOperatorParselet,
};
use std::collections::HashMap;
use std::rc::Rc;

/// Parse error type for the parser core.
#[derive(Debug, Clone)]
pub enum ParseError {
    Custom(String),
    UnexpectedToken(String),
    UnexpectedEof(String),
    MissingExpression(String),
    InvalidTypeSpec(String),
    InvalidArguments(String),
    UnknownStatement(String),
}

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
        let mut parser = Parser {
            lexer,
            read: Vec::new(),
            prefix_parselets: HashMap::new(),
            infix_parselets: HashMap::new(),
        };

        // Register all prefix parselets
        parser.register_prefix(TokenKind::Int, Rc::new(IntLiteralParselet));
        parser.register_prefix(TokenKind::Float, Rc::new(FloatLiteralParselet));
        parser.register_prefix(TokenKind::Str, Rc::new(StringLiteralParselet));
        parser.register_prefix(TokenKind::TrueLiteral, Rc::new(BoolLiteralParselet));
        parser.register_prefix(TokenKind::FalseLiteral, Rc::new(BoolLiteralParselet));
        parser.register_prefix(TokenKind::NilLiteral, Rc::new(NilLiteralParselet));
        parser.register_prefix(TokenKind::Ident, Rc::new(IdentifierParselet));
        parser.register_prefix(
            TokenKind::Minus,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Negate,
            }),
        );
        parser.register_prefix(
            TokenKind::Plus,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Positive,
            }),
        );
        parser.register_prefix(
            TokenKind::Bang,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Not,
            }),
        );
        parser.register_prefix(
            TokenKind::Star,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Dereference,
            }),
        );
        parser.register_prefix(
            TokenKind::And,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::AddressOf,
            }),
        );
        parser.register_prefix(TokenKind::OpenParen, Rc::new(GroupParselet {}));

        // Register infix parselets for binary operators
        parser.register_infix(
            TokenKind::Plus,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Add,
                precedence: Precedence::Addition,
            }),
        );
        parser.register_infix(
            TokenKind::Minus,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Subtract,
                precedence: Precedence::Addition,
            }),
        );
        parser.register_infix(
            TokenKind::Star,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Multiply,
                precedence: Precedence::Multiplication,
            }),
        );
        parser.register_infix(
            TokenKind::Slash,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Divide,
                precedence: Precedence::Multiplication,
            }),
        );
        parser.register_infix(
            TokenKind::Percent,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Modulo,
                precedence: Precedence::Multiplication,
            }),
        );
        parser.register_infix(
            TokenKind::EqualEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Equal,
                precedence: Precedence::Equality,
            }),
        );
        parser.register_infix(
            TokenKind::NotEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::NotEqual,
                precedence: Precedence::Equality,
            }),
        );
        parser.register_infix(
            TokenKind::LessThan,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LessThan,
                precedence: Precedence::Comparison,
            }),
        );
        parser.register_infix(
            TokenKind::GreaterThan,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::GreaterThan,
                precedence: Precedence::Comparison,
            }),
        );
        parser.register_infix(
            TokenKind::LessOrEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LessThanOrEqual,
                precedence: Precedence::Comparison,
            }),
        );
        parser.register_infix(
            TokenKind::GreaterOrEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::GreaterThanOrEqual,
                precedence: Precedence::Comparison,
            }),
        );
        parser.register_infix(
            TokenKind::AndAnd,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LogicalAnd,
                precedence: Precedence::LogicalAnd,
            }),
        );
        parser.register_infix(
            TokenKind::PipePipe,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LogicalOr,
                precedence: Precedence::LogicalOr,
            }),
        );
        parser.register_infix(
            TokenKind::And,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::BitwiseAnd,
                precedence: Precedence::BitwiseAnd,
            }),
        );
        parser.register_infix(
            TokenKind::Pipe,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::BitwiseOr,
                precedence: Precedence::BitwiseOr,
            }),
        );
        parser.register_infix(
            TokenKind::Caret,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::BitwiseXor,
                precedence: Precedence::BitwiseXor,
            }),
        );
        parser.register_infix(TokenKind::OpenParen, Rc::new(CallParselet {}));
        parser.register_infix(TokenKind::OpenSquare, Rc::new(IndexParselet {}));
        parser.register_infix(TokenKind::Dot, Rc::new(FieldAccessParselet {}));

        parser
    }

    /// Returns true if the given token is a valid prefix for a parselet
    pub fn is_expression_prefix(&self, token_kind: &TokenKind) -> bool {
        self.prefix_parselets.contains_key(token_kind)
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
            self.consume()?;
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

    /// Parse an expression, starting with minimum precedence.
    /// This is the public API for expression parsing.
    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        expression::parse_expression(self, Precedence::Base)
    }

    pub fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        statement::parse_statement(self)
    }

    /// Check if the expression is done based on the next token kind
    fn expression_done(&self, kind: &TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Eof | TokenKind::CloseParen | TokenKind::CloseSquare | TokenKind::Comma
        )
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, UnaryOp};
    use pretty_assertions::assert_eq;

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
    fn parse_expression_no_prefix_parselet() {
        let input = "|";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

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
