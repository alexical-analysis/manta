pub mod declaration;
pub mod expression;
pub mod lexer;
pub mod module;
pub mod pattern;
pub mod statement;
pub mod types;

use crate::ast::Decl;
use crate::str_store::StrStore;

use declaration::DeclParser;
use lexer::{Lexer, Token, TokenKind};
use module::Module;
use serde::{Deserialize, Serialize};

/// Parse error type for the parser core.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParseError {
    Custom(Token, String),
    UnexpectedToken(Token, String),
    MissingExpression(Token, String),
    InvalidTypeSpec(Token, String),
    InvalidArguments(Token, String),
    InvalidExpression(Token, String),
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

    /// Parse a Manta module
    pub fn parse_module(&self, str_store: &mut StrStore) -> Module {
        let mut lexer = Lexer::new(&self.source, str_store);

        let mut declarations = vec![];
        let mut errors = vec![];
        loop {
            let token_kind = lexer.peek().kind;
            if token_kind == TokenKind::Eof {
                break;
            }
            // TODO: the parse method should return both the Decl as well as a vector of errors. We
            // want to try and compile no matter what so having some of the declaration info is
            // important and for decls like a function its possible to have way more than a single
            // ParseError that needs to be reported
            let decl = match self.decl_parser.parse(&mut lexer) {
                Ok(decl) => decl,
                Err(err) => {
                    errors.push(err);
                    Decl::Invalid
                }
            };
            declarations.push(decl);
        }

        Module::new(errors, declarations)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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

        let mut str_store = StrStore::new();
        let parser = Parser::new(source);
        let ast = parser.parse_module(&mut str_store);

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
