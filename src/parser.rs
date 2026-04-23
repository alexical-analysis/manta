pub mod declaration;
pub mod expression;
pub mod lexer;
pub mod module;
pub mod pattern;
pub mod statement;
pub mod types;

use std::path::PathBuf;

use crate::ast::Decl;
use crate::file_set::FileSet;
use crate::str_store::StrStore;

use declaration::DeclParser;
use lexer::{Lexer, Token, TokenKind};
use module::{File, Module};
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
pub struct Parser<'s, 'fs> {
    import_path: &'s PathBuf,
    file_set: &'fs FileSet,
    decl_parser: DeclParser,
}

impl<'s, 'fs> Parser<'s, 'fs> {
    /// Create a new parser for a piece of source code
    pub fn new(import_path: &'s PathBuf, file_set: &'fs FileSet) -> Self {
        Parser {
            import_path,
            file_set,
            decl_parser: DeclParser::new(),
        }
    }

    /// Parse a Manta module
    pub fn parse_module(&self, str_store: &mut StrStore) -> Module {
        let mut files = vec![];

        for file in self.file_set.files() {
            let parsed = self.parse_file(str_store, &file.source, file.base());
            files.push(parsed)
        }

        // TODO: this is probably not the right place to compute the public prefix. It would maybe be
        // better to pipe the import_path through the pipeline and then compute the prefix durring codegen
        // that's tricy though since I try to inter all my strings early so the str_store does not need
        // to be mutated after lexing/parsing. It's fine to keep this here for now and we'll maybe rethink
        // this approach in the future

        // create an escaped use path that can be inserted into exported symbols
        let mut import_path = self.import_path.to_string_lossy().to_string();

        // escape the import path if it's non-empty
        if !import_path.is_empty() {
            import_path = import_path.replace("_", "_0");
            import_path = import_path.replace("/", "__");
            import_path = import_path + "_";
        }

        let public_prefix = "manta_".to_string() + import_path.as_str();

        let public_prefix_id = str_store.get_id(&public_prefix);
        Module::new(public_prefix_id, files)
    }

    fn parse_file(&self, str_store: &mut StrStore, source: &String, base: usize) -> File {
        let mut lexer = Lexer::new(source, str_store, base);

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

        File::new(errors, declarations)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_set::File;
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
        let file = File::new(file_name.to_string(), source);
        let file_set = FileSet::new_from_files(std::path::PathBuf::new(), vec![file]);
        let test_use_path = PathBuf::from("test");
        let parser = Parser::new(&test_use_path, &file_set);
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

    include!(concat!(env!("OUT_DIR"), "/generated_parser_tests.rs"));
}
