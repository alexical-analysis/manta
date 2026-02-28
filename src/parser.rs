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
use serde::Serialize;

/// Parse error type for the parser core.
#[derive(Debug, Clone, PartialEq, Serialize)]
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

    fn assert_file_path_eq(path: &std::path::Path, parser_dir: &Path) {
        let ext = path.extension().expect("Failed to get file extension");
        if ext != "manta" {
            // Skip over non-manta files
            return;
        }

        let file_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        let source = match fs::read_to_string(path) {
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

    fn assert_case_file_eq(path: &Path) {
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => panic!("Failed to read {}", path.display()),
        };

        // parse named tests in file: <name>: .. expect: ..
        let mut cases: Vec<(String, String, String)> = Vec::new();
        let mut cur_name: Option<String> = None;
        let mut cur_input = String::new();
        let mut cur_expect = String::new();
        let mut reading_expect = false;

        for line in source.lines() {
            if line.trim_end().ends_with(':') {
                let label = &line.trim_end()[..line.trim_end().len() - 1];
                let label = label.trim();
                if label.eq_ignore_ascii_case("expect") {
                    reading_expect = true;
                    continue;
                } else {
                    if let Some(name) = cur_name.take() {
                        cases.push((name, cur_input.clone(), cur_expect.clone()));
                        cur_input.clear();
                        cur_expect.clear();
                        reading_expect = false;
                    }
                    cur_name = Some(label.to_string());
                    reading_expect = false;
                    continue;
                }
            }

            if cur_name.is_some() {
                if reading_expect {
                    cur_expect.push_str(line);
                    cur_expect.push('\n');
                } else {
                    cur_input.push_str(line);
                    cur_input.push('\n');
                }
            }
        }

        if let Some(name) = cur_name.take() {
            cases.push((name, cur_input.clone(), cur_expect.clone()));
        }

        if cases.is_empty() {
            panic!("No parser unit tests found in {}", path.display());
        }

        let mut needs_write = false;

        for (name, input, expect) in &cases {
            let mut str_store = StrStore::new();
            let wrapped_input = format!("fn main() {{\n{}\n}}", input);
            let parser = Parser::new(wrapped_input);
            let ast = parser.parse_module(&mut str_store);
            let json_output =
                serde_json::to_string_pretty(&ast).expect("Failed to serialize AST to JSON");

            if expect.trim().is_empty() {
                needs_write = true;
            } else {
                let expected = expect.trim_end_matches('\n');
                assert_eq!(
                    json_output, expected,
                    "Parser unit test mismatch for {} in file {:?}",
                    name, path
                );
            }
        }

        if needs_write {
            let mut rebuilt = String::new();
            for (name, input, _) in &cases {
                rebuilt.push_str(&format!("{}:\n", name));
                rebuilt.push_str(input);
                if !input.ends_with('\n') {
                    rebuilt.push('\n');
                }
                rebuilt.push_str("expect:\n");
                let mut str_store = StrStore::new();
                let wrapped_input = format!("fn main {{\n{}\n}}", input);
                let parser = Parser::new(wrapped_input);
                let ast = parser.parse_module(&mut str_store);
                let json_output =
                    serde_json::to_string_pretty(&ast).expect("Failed to serialize AST to JSON");
                rebuilt.push_str(&json_output);
                rebuilt.push('\n');
                rebuilt.push('\n');
            }
            fs::write(&path, rebuilt).expect("Failed to write regenerated parser unit tests");
            panic!(
                "Generated new parser unit test expected output in {:?}. Please verify",
                path
            );
        }
    }

    include!(concat!(env!("OUT_DIR"), "/generated_parser_tests.rs"));
    include!(concat!(env!("OUT_DIR"), "/generated_parser_unit_tests.rs"));
}
