pub mod declaration;
pub mod expression;
pub mod lexer;
pub mod pattern;
pub mod statement;
pub mod types;

use crate::ast::Decl;

use declaration::DeclParser;
use lexer::{Lexer, Token, TokenKind};

/// Parse error type for the parser core.
#[derive(Debug, Clone)]
pub enum ParseError {
    Custom(String),
    UnexpectedToken(Token, String),
    MissingExpression(String),
    InvalidTypeSpec(String),
    InvalidArguments(String),
    InvalidExpression(Token, String),
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
    source: String,
    decl_parser: DeclParser,
}

impl Parser {
    /// Create a new parser for a piece of source code
    pub fn new(source: String) -> Self {
        Parser {
            source,
            decl_parser: DeclParser::new(),
        }
    }

    /// Parse a complete Manta program, returning a list of top-level declarations.
    pub fn parse_program(&self) -> Result<Vec<Decl>, ParseError> {
        let mut declarations = vec![];
        let mut lexer = Lexer::new(&self.source);

        loop {
            let token_kind = lexer.peek().kind;
            if token_kind == TokenKind::Eof {
                break;
            }
            let decl = self.decl_parser.parse(&mut lexer)?;
            declarations.push(decl);
        }

        Ok(declarations)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Decl;
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::path::Path;

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
        let entry = match entry {
            Ok(dir) => dir,
            Err(_) => panic!("Failed to read entry in '{:?}' directory", test_dir),
        };

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

        let source = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => format!("Failed to read {}", path.display()),
        };

        let parser = Parser::new(source);
        let ast: Result<Vec<Decl>, ParseError> = parser.parse_program();

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
            let expected_json = match fs::read_to_string(&parser_file) {
                Ok(s) => s,
                Err(_) => format!("Failed to read {}", parser_file.display()),
            };

            assert_eq!(
                json_output, expected_json,
                "Parser output mismatch for {}",
                file_name
            );
        } else {
            // Create the parser directory if it doesn't exist
            fs::create_dir_all(parser_dir).expect("Failed to create parser test directory");

            // Write the output if the file does not exist
            match fs::write(&parser_file, &json_output) {
                Ok(_) => (),
                Err(_) => panic!("Failed to write parser output to {:?}", parser_file),
            };

            // If we generated the output file, fail the test to prompt the user to verify it's correctness
            panic!(
                "Generated new parser output file: {:?}. Please verify its correctness.",
                parser_file
            );
        }
    }
}
