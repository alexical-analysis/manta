pub mod declaration;
pub mod expression;
pub mod lexer;
pub mod parselets;
pub mod statement;
pub mod types;

use crate::ast::{BinaryOp, Decl, Expr, UnaryOp};
use crate::parser::parselets::{
    BlockParselet, InfixStmtParselet, PrefixDeclParselet, ShortLetParselet,
};
use lexer::{Lexer, Token, TokenKind};
use parselets::{
    AssignParselet, BinaryOperatorParselet, BoolLiteralParselet, CallParselet, ConstDeclParselet,
    DeferParselet, FieldAccessParselet, FloatLiteralParselet, GroupParselet, IdentifierParselet,
    IfParselet, ImportDeclParselet, IndexParselet, InferedVariantParselet, InfixExprParselet,
    IntLiteralParselet, LetParselet, MatchParselet, NilLiteralParselet, Precedence,
    PrefixExprParselet, PrefixStmtParselet, ReturnParselet, StringLiteralParselet, TryParselet,
    TypeDeclParselet, UnaryOperatorParselet,
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
    InvalidExpression(String),
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

    // prefix and infix expression parselets
    prefix_expr_parselets: HashMap<TokenKind, Rc<dyn PrefixExprParselet>>,
    infix_expr_parselets: HashMap<TokenKind, Rc<dyn InfixExprParselet>>,

    // prefix and infix statement parselets
    prefix_stmt_parselets: HashMap<TokenKind, Rc<dyn PrefixStmtParselet>>,
    infix_stmt_parselets: HashMap<TokenKind, Rc<dyn InfixStmtParselet>>,

    // top-level declaration parselets
    prefix_decl_parselets: HashMap<TokenKind, Rc<dyn PrefixDeclParselet>>,
}

impl Parser {
    /// Create a new parser from a lexer
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            read: Vec::new(),
            prefix_expr_parselets: HashMap::new(),
            infix_expr_parselets: HashMap::new(),
            prefix_stmt_parselets: HashMap::new(),
            infix_stmt_parselets: HashMap::new(),
            prefix_decl_parselets: HashMap::new(),
        };

        // Register all prefix parselets
        parser.register_expr_prefix(TokenKind::Int, Rc::new(IntLiteralParselet));
        parser.register_expr_prefix(TokenKind::Float, Rc::new(FloatLiteralParselet));
        parser.register_expr_prefix(TokenKind::Str, Rc::new(StringLiteralParselet));
        parser.register_expr_prefix(TokenKind::TrueLiteral, Rc::new(BoolLiteralParselet));
        parser.register_expr_prefix(TokenKind::FalseLiteral, Rc::new(BoolLiteralParselet));
        parser.register_expr_prefix(TokenKind::NilLiteral, Rc::new(NilLiteralParselet));
        parser.register_expr_prefix(TokenKind::Identifier, Rc::new(IdentifierParselet));
        parser.register_expr_prefix(
            TokenKind::Minus,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Negate,
            }),
        );
        parser.register_expr_prefix(
            TokenKind::Plus,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Positive,
            }),
        );
        parser.register_expr_prefix(
            TokenKind::Bang,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Not,
            }),
        );
        parser.register_expr_prefix(
            TokenKind::Star,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::Dereference,
            }),
        );
        parser.register_expr_prefix(
            TokenKind::And,
            Rc::new(UnaryOperatorParselet {
                operator: UnaryOp::AddressOf,
            }),
        );
        parser.register_expr_prefix(TokenKind::OpenParen, Rc::new(GroupParselet {}));
        parser.register_expr_prefix(TokenKind::Dot, Rc::new(InferedVariantParselet {}));

        // Register infix parselets for binary operators
        parser.register_expr_infix(
            TokenKind::Plus,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Add,
                precedence: Precedence::Addition,
            }),
        );
        parser.register_expr_infix(
            TokenKind::Minus,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Subtract,
                precedence: Precedence::Addition,
            }),
        );
        parser.register_expr_infix(
            TokenKind::Star,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Multiply,
                precedence: Precedence::Multiplication,
            }),
        );
        parser.register_expr_infix(
            TokenKind::Slash,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Divide,
                precedence: Precedence::Multiplication,
            }),
        );
        parser.register_expr_infix(
            TokenKind::Percent,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Modulo,
                precedence: Precedence::Multiplication,
            }),
        );
        parser.register_expr_infix(
            TokenKind::EqualEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::Equal,
                precedence: Precedence::Equality,
            }),
        );
        parser.register_expr_infix(
            TokenKind::NotEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::NotEqual,
                precedence: Precedence::Equality,
            }),
        );
        parser.register_expr_infix(
            TokenKind::LessThan,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LessThan,
                precedence: Precedence::Comparison,
            }),
        );
        parser.register_expr_infix(
            TokenKind::GreaterThan,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::GreaterThan,
                precedence: Precedence::Comparison,
            }),
        );
        parser.register_expr_infix(
            TokenKind::LessOrEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LessThanOrEqual,
                precedence: Precedence::Comparison,
            }),
        );
        parser.register_expr_infix(
            TokenKind::GreaterOrEqual,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::GreaterThanOrEqual,
                precedence: Precedence::Comparison,
            }),
        );
        parser.register_expr_infix(
            TokenKind::AndAnd,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LogicalAnd,
                precedence: Precedence::LogicalAnd,
            }),
        );
        parser.register_expr_infix(
            TokenKind::PipePipe,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::LogicalOr,
                precedence: Precedence::LogicalOr,
            }),
        );
        parser.register_expr_infix(
            TokenKind::And,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::BitwiseAnd,
                precedence: Precedence::BitwiseAnd,
            }),
        );
        parser.register_expr_infix(
            TokenKind::Pipe,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::BitwiseOr,
                precedence: Precedence::BitwiseOr,
            }),
        );
        parser.register_expr_infix(
            TokenKind::Caret,
            Rc::new(BinaryOperatorParselet {
                operator: BinaryOp::BitwiseXor,
                precedence: Precedence::BitwiseXor,
            }),
        );
        parser.register_expr_infix(TokenKind::OpenParen, Rc::new(CallParselet {}));
        parser.register_expr_infix(TokenKind::OpenSquare, Rc::new(IndexParselet {}));
        parser.register_expr_infix(TokenKind::Dot, Rc::new(FieldAccessParselet {}));

        // Register prefix statement parselets
        parser.register_stmt_prefix(TokenKind::LetKeyword, Rc::new(LetParselet));
        parser.register_stmt_prefix(TokenKind::ReturnKeyword, Rc::new(ReturnParselet));
        parser.register_stmt_prefix(TokenKind::DeferKeyword, Rc::new(DeferParselet));
        parser.register_stmt_prefix(TokenKind::OpenBrace, Rc::new(BlockParselet));
        parser.register_stmt_prefix(TokenKind::IfKeyword, Rc::new(IfParselet));
        parser.register_stmt_prefix(TokenKind::TryKeyword, Rc::new(TryParselet));
        parser.register_stmt_prefix(TokenKind::MatchKeyword, Rc::new(MatchParselet));

        // Register infix statement parselets
        parser.register_stmt_infix(TokenKind::Equal, Rc::new(AssignParselet));
        parser.register_stmt_infix(TokenKind::ColonEqual, Rc::new(ShortLetParselet));

        // Register prefix declaration parselets
        parser.register_decl_prefix(
            TokenKind::FnKeyword,
            Rc::new(parselets::FunctionDeclParselet),
        );
        parser.register_decl_prefix(TokenKind::TypeKeyword, Rc::new(TypeDeclParselet));
        parser.register_decl_prefix(TokenKind::ConstKeyword, Rc::new(ConstDeclParselet));
        parser.register_decl_prefix(TokenKind::ImportKeyword, Rc::new(ImportDeclParselet));

        parser
    }

    /// Returns true if the given token is a valid prefix for a parselet
    pub fn is_expression_prefix(&self, token_kind: &TokenKind) -> bool {
        self.prefix_expr_parselets.contains_key(token_kind)
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

    /// Register a expr prefix parselet for a token kind.
    pub fn register_expr_prefix(&mut self, kind: TokenKind, parselet: Rc<dyn PrefixExprParselet>) {
        self.prefix_expr_parselets.insert(kind, parselet);
    }

    /// Register an expr infix parselet for a token kind.
    pub fn register_expr_infix(&mut self, kind: TokenKind, parselet: Rc<dyn InfixExprParselet>) {
        self.infix_expr_parselets.insert(kind, parselet);
    }

    /// Register a stmt prefix parser for a token kind.
    pub fn register_stmt_prefix(&mut self, kind: TokenKind, parselet: Rc<dyn PrefixStmtParselet>) {
        self.prefix_stmt_parselets.insert(kind, parselet);
    }

    /// Register an infix stmt parselet for a token kind.
    pub fn register_stmt_infix(&mut self, kind: TokenKind, parselets: Rc<dyn InfixStmtParselet>) {
        self.infix_stmt_parselets.insert(kind, parselets);
    }

    /// Register a prefix decl parselet for a token kind.
    pub fn register_decl_prefix(&mut self, kind: TokenKind, parselet: Rc<dyn PrefixDeclParselet>) {
        self.prefix_decl_parselets.insert(kind, parselet);
    }

    /// Parse an expression, starting with minimum precedence.
    /// This is the public API for expression parsing.
    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        expression::parse_expression(self, Precedence::Base)
    }

    /// Parse a complete Manta program, returning a list of top-level declarations.
    pub fn parse_program(&mut self) -> Result<Vec<Decl>, ParseError> {
        let mut declarations = vec![];

        loop {
            let token_kind = self.lookahead(0)?.kind;
            if token_kind == TokenKind::Eof {
                break;
            }
            let decl = declaration::parse_declaration(self)?;
            declarations.push(decl);
        }

        Ok(declarations)
    }

    /// Get the precedence of a token kind
    fn get_precedence(&self, kind: &TokenKind) -> Result<Precedence, ParseError> {
        match self.infix_expr_parselets.get(kind) {
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
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::path::Path;

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

        let semicolon_or_next = parser.lookahead(0).unwrap();
        assert_eq!(semicolon_or_next.kind, TokenKind::Semicolon);
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

    #[test]
    fn parse_file_tests() {
        let test_dir = Path::new("tests/src");
        let parser_dir = Path::new("tests/parser");

        if !test_dir.exists() {
            panic!(
                "Test directory does not exist. Please create a '{:?}' with test .manta files.",
                test_dir
            );
        }

        let entries = fs::read_dir(test_dir).expect("Failed to read tests/parser directory");

        // Read all .manta files from tests/parser and check them against expected parser output
        for entry in entries {
            assert_file_eq(entry, test_dir, parser_dir);
        }
    }

    fn assert_file_eq(
        entry: Result<std::fs::DirEntry, std::io::Error>,
        test_dir: &std::path::Path,
        parser_dir: &std::path::Path,
    ) {
        let entry =
            entry.expect(format!("Failed to read entry in '{:?}' directory", test_dir).as_str());

        let path = entry.path();
        let ext = path.extension().expect("Failed to get file extension");
        if ext != "manta" {
            // Skip over non-manta files
            return;
        }

        let file_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        let source =
            fs::read_to_string(&path).expect(&format!("Failed to read {}", path.display()));

        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer);

        let ast: Result<Vec<crate::ast::Decl>, ParseError> = parser.parse_program();

        let ast = match ast {
            Ok(a) => a,
            Err(e) => {
                panic!("Parser error for {}: {:?}", file_name, e);
            }
        };

        let json_output =
            serde_json::to_string_pretty(&ast).expect("Failed to serialize AST to JSON");

        let parser_file = parser_dir.join(format!("{}.json", file_name));

        if parser_file.exists() {
            let expected_json = fs::read_to_string(&parser_file)
                .expect(&format!("Failed to read {}", parser_file.display()));

            assert_eq!(
                json_output, expected_json,
                "Parser output mismatch for {}",
                file_name
            );
        } else {
            // Create the parser directory if it doesn't exist
            fs::create_dir_all(parser_dir).expect("Failed to create parser test directory");

            // Write the output if the file does not exist
            fs::write(&parser_file, &json_output).expect(&format!(
                "Failed to write parser output to {:?}",
                parser_file
            ));

            // If we generated the output file, fail the test to prompt the user to verify it's correctness
            panic!(
                "Generated new parser output file: {:?}. Please verify its correctness.",
                parser_file
            );
        }
    }
}
