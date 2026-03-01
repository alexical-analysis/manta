use crate::str_store::{StrID, StrStore};
use serde::Serialize;
use strum_macros::{Display, EnumString};

// SourceID is the uniqe identifier of the token in the source code
pub type SourceID = usize;

/// The kind of Token produced by the lexer.
#[derive(Debug, Display, EnumString, Copy, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum TokenKind {
    Identifier,
    Int,
    Float,
    Str,
    MalformedStr, // this is used for unterminated strings
    TrueLiteral,
    FalseLiteral,
    FnKeyword,
    IfKeyword,
    ReturnKeyword,
    ElseKeyword,
    WhileKeyword,
    ForKeyword,
    BreakKeyword,
    ContinueKey,
    DeferKeyword,
    StructKeyword,
    EnumKeyword,
    SwitchKeyword,
    MatchKeyword,
    LetKeyword,
    ConstKeyword,
    TypeKeyword,
    ModKeyword,
    UseKeyword,
    MutKeyword,
    VarKeyword,
    OrKeyword,
    WrapKeyword,
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    Comma,
    Colon,
    ColonColon,
    Semicolon,
    PlusEqual,
    MinusEqual,
    Equal,
    EqualEqual,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterOrEqual,
    LessOrEqual,
    At,
    Pipe,
    PipePipe,
    And,
    AndAnd,
    Dot,
    Star,
    Plus,
    Bang,
    Minus,
    Caret,
    Slash,
    SlashSlash,
    Percent,
    Underscore,
    SlashStar,
    StarSlash,
    Arrow,
    Eof,
}

/// A token produced by the lexer. `lexeme` contains raw text for ids/numbers/strings
/// (strings are returned unescaped), or None for simple punctuation/EOF.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize)]
pub struct Token {
    pub kind: TokenKind,
    pub source_id: SourceID,
    pub lexeme_id: StrID,
}

/// Minimal lexer. Uses a byte cursor but iterates by `char`s for UTF-8 correctness.
pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
    prev_kind: TokenKind,
    next: Token,
    str_store: &'a mut StrStore,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer from source text.
    pub fn new(source: &'a str, str_store: &'a mut StrStore) -> Self {
        let mut lexer = Lexer {
            source,
            pos: 0,
            prev_kind: TokenKind::Identifier,
            next: Token {
                kind: TokenKind::Identifier,
                source_id: 0,
                lexeme_id: StrID(0),
            },
            str_store,
        };
        lexer.next_token();

        lexer
    }

    pub fn lexeme(&self, lexeme_id: StrID) -> String {
        match self.str_store.get_string(lexeme_id) {
            Some(s) => s.to_string(),
            // TODO: should this hand back an option?
            None => panic!("invalid str id {}", lexeme_id.to_usize()),
        }
    }

    /// Non-consuming peek of the next token kind (may return LexError if scanning fails).
    pub fn peek(&self) -> Token {
        self.next
    }

    /// Return the next token in input source
    pub fn next_token(&mut self) -> Token {
        let token = self.lex_token();
        self.prev_kind = token.kind;

        let ret = self.next;
        self.next = token;
        ret
    }

    fn lex_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        // determine by first char
        let ch = self.current_char();
        if ch.is_none() {
            let lexeme_id = self.str_store.get_id(&self.source[self.pos..self.pos]);
            if self.is_end_of_statement() {
                return Token {
                    kind: TokenKind::Semicolon,
                    source_id: self.pos,
                    lexeme_id,
                };
            }

            return Token {
                kind: TokenKind::Eof,
                source_id: self.pos,
                lexeme_id,
            };
        }
        let ch = ch.unwrap();

        if ch == '\n' {
            // if this newline wasn't skipped it's because we need to insert a semicolon
            let source_id = self.pos;
            self.bump();
            let lexeme_id = self.str_store.get_id(&self.source[source_id..self.pos]);

            return Token {
                kind: TokenKind::Semicolon,
                source_id,
                lexeme_id,
            };
        }

        if ch == '}' && self.is_end_of_statement() {
            let lexeme_id = self.str_store.get_id(&self.source[self.pos..self.pos]);

            return Token {
                kind: TokenKind::Semicolon,
                source_id: self.pos,
                lexeme_id,
            };
        }

        if is_ident_start(ch) {
            return self.read_ident_or_keyword();
        }

        if ch.is_ascii_digit() {
            return self.read_number();
        }

        if ch == '"' {
            return self.read_string();
        }

        // operators and punctuation
        self.read_operator_or_punct()
    }

    fn current_char(&self) -> Option<char> {
        self.source[self.pos..].chars().next()
    }

    fn peek_char_n(&self, n: usize) -> Option<char> {
        let mut it = self.source[self.pos..].chars();
        (0..n).for_each(|_| {
            let _ = it.next();
        });
        it.next()
    }

    fn bump(&mut self) -> Option<char> {
        if let Some(ch) = self.current_char() {
            let adv = ch.len_utf8();
            self.pos += adv;
            Some(ch)
        } else {
            None
        }
    }

    fn eat_while<F>(&mut self, mut cond: F) -> String
    where
        F: FnMut(char) -> bool,
    {
        let start = self.pos;
        while let Some(ch) = self.current_char() {
            if cond(ch) {
                self.bump();
            } else {
                break;
            }
        }
        self.source[start..self.pos].to_string()
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            // check if this should be a synthetic semicolon
            if let Some(ch) = self.current_char() {
                if ch == '\n' && self.is_end_of_statement() {
                    break;
                }
            };

            // skip whitespace
            let mut progressed = false;
            while let Some(ch) = self.current_char() {
                if ch.is_whitespace() {
                    progressed = true;
                    self.bump();
                } else {
                    break;
                }
            }

            // comments
            if self.source[self.pos..].starts_with("//") {
                progressed = true;
                // consume until newline or EOF
                self.pos += 2;
                while let Some(ch) = self.current_char() {
                    self.bump();
                    if ch == '\n' {
                        break;
                    }
                }
            } else if self.source[self.pos..].starts_with("/*") {
                progressed = true;
                // consume block comment until */
                self.pos += 2;
                loop {
                    if self.current_char().is_none() {
                        // technically this is a malformed block comment but it's not really worth
                        // calling that out as an error. Instead we just consider it succesfully
                        // closed
                        return;
                    }
                    if self.source[self.pos..].starts_with("*/") {
                        self.pos += 2;
                        break;
                    }
                    self.bump();
                }
            }

            if !progressed {
                break;
            }
        }
    }

    fn is_end_of_statement(&mut self) -> bool {
        matches!(
            self.prev_kind,
            TokenKind::Identifier
                | TokenKind::Int
                | TokenKind::Float
                | TokenKind::Str
                | TokenKind::TrueLiteral
                | TokenKind::FalseLiteral
                | TokenKind::ReturnKeyword
                | TokenKind::BreakKeyword
                | TokenKind::ContinueKey
                | TokenKind::CloseBrace
                | TokenKind::CloseParen
                | TokenKind::CloseSquare
                | TokenKind::Bang
        )
    }

    fn read_ident_or_keyword(&mut self) -> Token {
        let start = self.pos;
        let lex = self.eat_while(is_ident_continue);
        let end = self.pos;

        let kind = match lex.as_str() {
            "let" => TokenKind::LetKeyword,
            "fn" => TokenKind::FnKeyword,
            "if" => TokenKind::IfKeyword,
            "else" => TokenKind::ElseKeyword,
            "while" => TokenKind::WhileKeyword,
            "for" => TokenKind::ForKeyword,
            "return" => TokenKind::ReturnKeyword,
            "true" => TokenKind::TrueLiteral,
            "false" => TokenKind::FalseLiteral,
            "match" => TokenKind::MatchKeyword,
            "switch" => TokenKind::SwitchKeyword,
            "enum" => TokenKind::EnumKeyword,
            "struct" => TokenKind::StructKeyword,
            "const" => TokenKind::ConstKeyword,
            "break" => TokenKind::BreakKeyword,
            "continue" => TokenKind::ContinueKey,
            "defer" => TokenKind::DeferKeyword,
            "type" => TokenKind::TypeKeyword,
            "mod" => TokenKind::ModKeyword,
            "use" => TokenKind::UseKeyword,
            "mut" => TokenKind::MutKeyword,
            "var" => TokenKind::VarKeyword,
            "or" => TokenKind::OrKeyword,
            "wrap" => TokenKind::WrapKeyword,
            _ => TokenKind::Identifier,
        };

        let lexeme_id = self.str_store.get_id(&self.source[start..end]);
        Token {
            kind,
            source_id: start,
            lexeme_id,
        }
    }

    fn read_number(&mut self) -> Token {
        let start = self.pos;
        let mut seen_dot = false;
        let mut seen_exp = false;

        while let Some(ch) = self.current_char() {
            if ch == '_' {
                // allow underscores between digits
                self.bump();
                continue;
            }
            if ch.is_ascii_digit() {
                self.bump();
                continue;
            }
            if ch == '.' && !seen_dot {
                // check next char to avoid treating '.' in '..' as float
                if let Some(nc) = self.peek_char_n(1) {
                    if nc.is_ascii_digit() {
                        seen_dot = true;
                        self.bump();
                        continue;
                    }
                }
            }
            break;
        }

        // exponent part of the float
        if let Some(ch) = self.current_char() {
            if ch == 'e' || ch == 'E' {
                seen_exp = true;
                self.bump();
                if let Some(sign) = self.current_char() {
                    if sign == '+' || sign == '-' {
                        self.bump();
                    }
                }

                // digits of the exponent
                self.eat_while(|c| c.is_ascii_digit() || c == '_');
            }
        }

        let end = self.pos;
        let kind = if seen_dot || seen_exp {
            TokenKind::Float
        } else {
            TokenKind::Int
        };
        let lexeme_id = self.str_store.get_id(&self.source[start..end]);
        Token {
            kind,
            source_id: start,
            lexeme_id,
        }
    }

    fn read_string(&mut self) -> Token {
        let start = self.pos;
        // consume the opening quote char
        let quote = self.bump().unwrap();
        debug_assert!(quote == '"');

        while let Some(ch) = self.current_char() {
            self.bump();
            if ch == '"' {
                let end = self.pos;
                let string_content = &self.source[start + 1..end - 1];
                let lexeme_id = self.str_store.get_id(string_content);
                return Token {
                    kind: TokenKind::Str,
                    source_id: start,
                    lexeme_id,
                };
            }
            if ch == '\\' {
                match self.current_char() {
                    Some(_) => {
                        self.bump();
                        continue;
                    }
                    None => {
                        let end = self.pos;
                        let malformed_str = &self.source[start + 1..end - 1];
                        let lexeme_id = self.str_store.get_id(malformed_str);
                        return Token {
                            kind: TokenKind::MalformedStr,
                            source_id: self.pos,
                            lexeme_id,
                        };
                    }
                }
            }
        }

        let end = self.pos;
        let malformed_str = &self.source[start + 1..end - 1];
        let lexeme_id = self.str_store.get_id(malformed_str);
        Token {
            kind: TokenKind::MalformedStr,
            source_id: start,
            lexeme_id,
        }
    }

    fn read_operator_or_punct(&mut self) -> Token {
        let start = self.pos;
        let ch = self.bump().unwrap();
        let next = self.current_char();

        // multi-char operators
        if let Some(nc) = next {
            let two = format!("{}{}", ch, nc);
            let kind = match two.as_str() {
                "==" => Some(TokenKind::EqualEqual),
                "!=" => Some(TokenKind::NotEqual),
                "<=" => Some(TokenKind::LessOrEqual),
                ">=" => Some(TokenKind::GreaterOrEqual),
                "=>" => Some(TokenKind::Arrow),
                "&&" => Some(TokenKind::AndAnd),
                "||" => Some(TokenKind::PipePipe),
                "::" => Some(TokenKind::ColonColon),
                "+=" => Some(TokenKind::PlusEqual),
                "-=" => Some(TokenKind::MinusEqual),
                _ => None,
            };

            if let Some(kind) = kind {
                self.bump();
                let end = self.pos;
                let lexeme_id = self.str_store.get_id(&self.source[start..end]);
                return Token {
                    kind,
                    source_id: start,
                    lexeme_id,
                };
            }
        }

        // single char punctuation operator
        let kind = match ch {
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '[' => TokenKind::OpenSquare,
            ']' => TokenKind::CloseSquare,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            ':' => TokenKind::Colon,
            '.' => TokenKind::Dot,
            '=' => TokenKind::Equal,
            '|' => TokenKind::Pipe,
            '&' => TokenKind::And,
            '*' => TokenKind::Star,
            '+' => TokenKind::Plus,
            '!' => TokenKind::Bang,
            '-' => TokenKind::Minus,
            '/' => TokenKind::Slash,
            '%' => TokenKind::Percent,
            '_' => TokenKind::Underscore,
            '<' => TokenKind::LessThan,
            '>' => TokenKind::GreaterThan,
            '^' => TokenKind::Caret,
            '@' => TokenKind::At,
            _ => panic!("Unknown character for single-char operator: {}", ch),
        };

        let end = self.pos;
        let lexeme_id = self.str_store.get_id(&self.source[start..end]);
        // otherwise treat as operator
        Token {
            kind,
            source_id: start,
            lexeme_id,
        }
    }
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || ch.is_alphabetic()
}

fn is_ident_continue(ch: char) -> bool {
    ch == '_' || ch.is_alphanumeric()
}

#[cfg(test)]
mod tests {
    use crate::str_store;

    use super::*;
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::path::Path;

    fn assert_file_path_eq(path: &Path, lex_dir: &Path) {
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
            Err(_) => panic!("Failed to read {}", path.display()),
        };

        let mut str_store = StrStore::new();
        let mut lexer = Lexer::new(&source, &mut str_store);
        let mut tokens = vec![];
        loop {
            let token = lexer.peek();
            tokens.push(token);
            if token.kind == TokenKind::Eof {
                break;
            }
            lexer.next_token();
        }

        let json_output =
            serde_json::to_string_pretty(&tokens).expect("Failed to serialize tokens to JSON");

        let lex_file = lex_dir.join(format!("{}.json", file_name));

        if lex_file.exists() {
            let expected_json = match fs::read_to_string(&lex_file) {
                Ok(s) => s,
                Err(_) => panic!("Failed to read {}", lex_file.display()),
            };

            assert_eq!(
                json_output, expected_json,
                "Lexer output mismatch for {}",
                file_name
            );
        } else {
            fs::create_dir_all(lex_dir).expect("Failed to create lexer test directory");

            match fs::write(&lex_file, &json_output) {
                Ok(_) => (),
                Err(_) => panic!("Failed to write lexer output to {:?}", lex_file),
            };

            panic!(
                "Generated new lexer output file: {:?}. Please verify its correctness.",
                lex_file
            );
        }
    }

    include!(concat!(env!("OUT_DIR"), "/generated_lexer_tests.rs"));

    macro_rules! test_lex_inputs {
        ( $( $case:ident { input: $input:expr, want: $want:expr,  } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let source = $input;
                    let mut str_store = StrStore::new();
                    let mut lexer = Lexer::new(source, &mut str_store);
                    let mut toks = vec![];
                    loop {
                        let token = lexer.peek();
                        toks.push(token);
                        if token.kind == TokenKind::Eof {
                            break;
                        }
                        lexer.next_token();
                    }
                    assert_eq!(toks, $want);
                }
            )*
        };
    }

    test_lex_inputs! {
        lex_input_simple_let{
            input: "let x = 42",
            want: vec![
                Token{
                    kind: TokenKind::LetKeyword,
                    source_id: 0,
                    lexeme_id: StrID(0),
                },
                Token{kind: TokenKind::Identifier, source_id: 4, lexeme_id: StrID(1)},
                Token{kind: TokenKind::Equal, source_id: 6, lexeme_id: StrID(2)},
                Token{kind: TokenKind::Int, source_id: 8, lexeme_id: StrID(3)},
                Token{kind: TokenKind::Semicolon, source_id: 10, lexeme_id: StrID(4)},
                Token{kind: TokenKind::Eof, source_id: 10, lexeme_id: StrID(4)},
            ],
        },
        lex_input_assign_with_new_line{
            input: "x = 5\n",
            want: vec![
                Token{kind: TokenKind::Identifier, source_id: 0, lexeme_id: StrID(0)},
                Token{kind: TokenKind::Equal, source_id: 2, lexeme_id: StrID(1)},
                Token{kind: TokenKind::Int, source_id: 4, lexeme_id: StrID(2)},
                Token{kind: TokenKind::Semicolon, source_id: 5, lexeme_id: StrID(3)},
                Token{kind: TokenKind::Eof, source_id: 6, lexeme_id: StrID(4)},
            ],
        },
        lex_input_string_and_escape {
            input: "\"hello\\n\"",
            want: vec![
                Token{kind: TokenKind::Str, source_id: 0, lexeme_id: StrID(0)},
                Token{kind: TokenKind::Semicolon, source_id: 9, lexeme_id: StrID(1)},
                Token{kind: TokenKind::Eof, source_id: 9, lexeme_id: StrID(1)},
            ],
        },
        lex_input_int_with_semicolon {
            input: "20;",
            want: vec![
                Token{kind: TokenKind::Int, source_id: 0, lexeme_id: StrID(0)},
                Token{kind: TokenKind::Semicolon, source_id: 2, lexeme_id: StrID(1)},
                Token{kind: TokenKind::Eof, source_id: 3, lexeme_id: StrID(2)},
            ],
        },
        lex_input_comments_and_whitespace {
            input: "  // top comment\nlet/*block*/ x = 1 // end\n",
            want: vec![
                Token{
                    kind: TokenKind::LetKeyword,
                    source_id: 17,
                    lexeme_id: StrID(0),
                },
                Token{kind: TokenKind::Identifier, source_id: 30, lexeme_id: StrID(1)},
                Token{kind: TokenKind::Equal, source_id: 32, lexeme_id: StrID(2)},
                Token{kind: TokenKind::Int, source_id: 34, lexeme_id: StrID(3)},
                Token{kind: TokenKind::Semicolon, source_id: 43, lexeme_id: StrID(4)},
                Token{kind: TokenKind::Eof, source_id: 43, lexeme_id: StrID(4)},
            ],
        },
        lex_input_ints_and_floats {
            input: "3.14 1e10 2.5e-3 1_000",
            want: vec![
                Token{kind: TokenKind::Float, source_id: 0, lexeme_id: StrID(0)},
                Token{kind: TokenKind::Float, source_id: 5, lexeme_id: StrID(1)},
                Token{
                    kind: TokenKind::Float,
                    source_id: 10,
                    lexeme_id: StrID(2),
                },
                Token{kind: TokenKind::Int, source_id: 17, lexeme_id: StrID(3)},
                Token{kind: TokenKind::Semicolon, source_id: 22, lexeme_id: StrID(4)},
                Token{kind: TokenKind::Eof, source_id: 22, lexeme_id: StrID(4)},
            ],
        },
        lex_input_multi_char_operations {
            input: "a == b && c != d <= e >= f  = += *",
            want: vec![
                Token{kind: TokenKind::Identifier, source_id: 0, lexeme_id: StrID(0)},
                Token{
                    kind: TokenKind::EqualEqual,
                    source_id: 2,
                    lexeme_id: StrID(1),
                },
                Token{kind: TokenKind::Identifier, source_id: 5, lexeme_id: StrID(2)},
                Token{kind: TokenKind::AndAnd, source_id: 7, lexeme_id: StrID(3)},
                Token{kind: TokenKind::Identifier, source_id: 10, lexeme_id: StrID(4)},
                Token{
                    kind: TokenKind::NotEqual,
                    source_id: 12,
                    lexeme_id: StrID(5),
                },
                Token{kind: TokenKind::Identifier, source_id: 15, lexeme_id: StrID(6)},
                Token{
                    kind: TokenKind::LessOrEqual,
                    source_id: 17,
                    lexeme_id: StrID(7),
                },
                Token{kind: TokenKind::Identifier, source_id: 20, lexeme_id: StrID(8)},
                Token{
                    kind: TokenKind::GreaterOrEqual,
                    source_id: 22,
                    lexeme_id: StrID(9),
                },
                Token{kind: TokenKind::Identifier, source_id: 25, lexeme_id: StrID(10)},
                Token{
                    kind: TokenKind::Equal,
                    source_id: 28,
                    lexeme_id: StrID(11),
                },
                Token{
                    kind: TokenKind::PlusEqual,
                    source_id: 30,
                    lexeme_id: StrID(12),
                },
                Token{
                    kind: TokenKind::Star,
                    source_id: 33,
                    lexeme_id: StrID(13),
                },
                Token{kind: TokenKind::Eof, source_id: 34, lexeme_id: StrID(14)},
            ],
        },
        lex_input_variants_and_assignemnt {
            input: ".Ok x = 1",
            want: vec![
                Token { kind: TokenKind::Dot, source_id: 0, lexeme_id: StrID(0 )},
                Token { kind: TokenKind::Identifier, source_id: 1, lexeme_id: StrID(1 )},
                Token { kind: TokenKind::Identifier, source_id: 4, lexeme_id: StrID(2 )},
                Token { kind: TokenKind::Equal, source_id: 6, lexeme_id: StrID(3 )},
                Token { kind: TokenKind::Int, source_id: 8, lexeme_id: StrID(4 )},
                Token { kind: TokenKind::Semicolon, source_id: 9, lexeme_id: StrID(5 )},
                Token { kind: TokenKind::Eof, source_id: 9, lexeme_id: StrID(5 )},
            ],
        },
        lex_input_type_enum_tokens {
            input: "type ErrWrite enum { Ok; IOError }",
            want: vec![
                Token { kind: TokenKind::TypeKeyword, source_id: 0, lexeme_id: StrID(0 )},
                Token { kind: TokenKind::Identifier, source_id: 5, lexeme_id: StrID(1 )},
                Token { kind: TokenKind::EnumKeyword, source_id: 14, lexeme_id: StrID(2 )},
                Token { kind: TokenKind::OpenBrace, source_id: 19, lexeme_id: StrID(3 )},

                Token { kind: TokenKind::Identifier, source_id: 21, lexeme_id: StrID(4 )},
                Token { kind: TokenKind::Semicolon, source_id: 23, lexeme_id: StrID(5 )},
                Token { kind: TokenKind::Identifier, source_id: 25, lexeme_id: StrID(6 )},
                Token { kind: TokenKind::Semicolon, source_id: 33, lexeme_id: StrID(7 )},
                Token { kind: TokenKind::CloseBrace, source_id: 33, lexeme_id: StrID(8 )},
                Token { kind: TokenKind::Semicolon, source_id: 34, lexeme_id: StrID(7 )},
                Token { kind: TokenKind::Eof, source_id: 34, lexeme_id: StrID(7 )},
            ],
        },
        lex_input_fn_decl {
            input: "fn write_and_cleanup(path str) WriteAndCleanup { }",
            want: vec![
                Token { kind: TokenKind::FnKeyword, source_id: 0, lexeme_id: StrID(0 )},
                Token { kind: TokenKind::Identifier, source_id: 3, lexeme_id: StrID(1 )},
                Token { kind: TokenKind::OpenParen, source_id: 20, lexeme_id: StrID(2 )},
                Token { kind: TokenKind::Identifier, source_id: 21, lexeme_id: StrID(3 )},
                Token { kind: TokenKind::Identifier, source_id: 26, lexeme_id: str_store::STR},
                Token { kind: TokenKind::CloseParen, source_id: 29, lexeme_id: StrID(4 )},
                Token { kind: TokenKind::Identifier, source_id: 31, lexeme_id: StrID(5 )},
                Token { kind: TokenKind::OpenBrace, source_id: 47, lexeme_id: StrID(6 )},
                Token { kind: TokenKind::CloseBrace, source_id: 49, lexeme_id: StrID(7 )},
                Token { kind: TokenKind::Semicolon, source_id: 50, lexeme_id: StrID(8 )},
                Token { kind: TokenKind::Eof, source_id: 50, lexeme_id: StrID(8 )},
            ],
        },
        lex_input_let_or {
            input: "let .Ok(f) = os::open(path) or { return .IOError }",
            want: vec![
                Token { kind: TokenKind::LetKeyword, source_id: 0, lexeme_id: StrID(0 )},
                Token { kind: TokenKind::Dot, source_id: 4, lexeme_id: StrID(1 )},
                Token { kind: TokenKind::Identifier, source_id: 5, lexeme_id: StrID(2 )},
                Token { kind: TokenKind::OpenParen, source_id: 7, lexeme_id: StrID(3 )},
                Token { kind: TokenKind::Identifier, source_id: 8, lexeme_id: StrID(4 )},
                Token { kind: TokenKind::CloseParen, source_id: 9, lexeme_id: StrID(5 )},
                Token { kind: TokenKind::Equal, source_id: 11, lexeme_id: StrID(6 )},
                Token { kind: TokenKind::Identifier, source_id: 13, lexeme_id: StrID(7 )},
                Token { kind: TokenKind::ColonColon, source_id: 15, lexeme_id: StrID(8 )},
                Token { kind: TokenKind::Identifier, source_id: 17, lexeme_id: StrID(9 )},
                Token { kind: TokenKind::OpenParen, source_id: 21, lexeme_id: StrID(3 )},
                Token { kind: TokenKind::Identifier, source_id: 22, lexeme_id: StrID(10 )},
                Token { kind: TokenKind::CloseParen, source_id: 26, lexeme_id: StrID(5 )},
                Token { kind: TokenKind::OrKeyword, source_id: 28, lexeme_id: StrID(11 )},
                Token { kind: TokenKind::OpenBrace, source_id: 31, lexeme_id: StrID(12 )},
                Token { kind: TokenKind::ReturnKeyword, source_id: 33, lexeme_id: StrID(13 )},
                Token { kind: TokenKind::Dot, source_id: 40, lexeme_id: StrID(1 )},
                Token { kind: TokenKind::Identifier, source_id: 41, lexeme_id: StrID(14 )},
                Token { kind: TokenKind::Semicolon, source_id: 49, lexeme_id: StrID(15 )},
                Token { kind: TokenKind::CloseBrace, source_id: 49, lexeme_id: StrID(16 )},
                Token { kind: TokenKind::Semicolon, source_id: 50, lexeme_id: StrID(15 )},
                Token { kind: TokenKind::Eof, source_id: 50, lexeme_id: StrID(15 )},
            ],
        },
        lex_input_pointer {
            input: "let .Ok(p) = maybe_alloc(false) !
print(*p)
free(p)
",
            want: vec![
                Token { kind: TokenKind::LetKeyword, source_id: 0, lexeme_id: StrID(0 )},
                Token { kind: TokenKind::Dot, source_id: 4, lexeme_id: StrID(1 )},
                Token { kind: TokenKind::Identifier, source_id: 5, lexeme_id: StrID(2 )},
                Token { kind: TokenKind::OpenParen, source_id: 7, lexeme_id: StrID(3 )},
                Token { kind: TokenKind::Identifier, source_id: 8, lexeme_id: StrID(4 )},
                Token { kind: TokenKind::CloseParen, source_id: 9, lexeme_id: StrID(5 )},
                Token { kind: TokenKind::Equal, source_id: 11, lexeme_id: StrID(6 )},
                Token { kind: TokenKind::Identifier, source_id: 13, lexeme_id: StrID(7 )},
                Token { kind: TokenKind::OpenParen, source_id: 24, lexeme_id: StrID(3 )},
                Token { kind: TokenKind::FalseLiteral, source_id: 25, lexeme_id: StrID(8 )},
                Token { kind: TokenKind::CloseParen, source_id: 30, lexeme_id: StrID(5 )},
                Token { kind: TokenKind::Bang, source_id: 32, lexeme_id: StrID(9 )},
                Token { kind: TokenKind::Semicolon, source_id: 33, lexeme_id: StrID(10 )},
                Token { kind: TokenKind::Identifier, source_id: 34, lexeme_id: StrID(11 )},
                Token { kind: TokenKind::OpenParen, source_id: 39, lexeme_id: StrID(3 )},
                Token { kind: TokenKind::Star, source_id: 40, lexeme_id: StrID(12 )},
                Token { kind: TokenKind::Identifier, source_id: 41, lexeme_id: StrID(4 )},
                Token { kind: TokenKind::CloseParen, source_id: 42, lexeme_id: StrID(5 )},
                Token { kind: TokenKind::Semicolon, source_id: 43, lexeme_id: StrID(10 )},
                Token { kind: TokenKind::Identifier, source_id: 44, lexeme_id: StrID(13 )},
                Token { kind: TokenKind::OpenParen, source_id: 48, lexeme_id: StrID(3 )},
                Token { kind: TokenKind::Identifier, source_id: 49, lexeme_id: StrID(4 )},
                Token { kind: TokenKind::CloseParen, source_id: 50, lexeme_id: StrID(5 )},
                Token { kind: TokenKind::Semicolon, source_id: 51, lexeme_id: StrID(10 )},
                Token { kind: TokenKind::Eof, source_id: 52, lexeme_id: StrID(14 )},
            ],
        },
        lex_unterminated_string {
            input: r#""no end"#,
            want: vec![
                Token { kind: TokenKind::MalformedStr, source_id: 0, lexeme_id: StrID(0 )},
                Token { kind: TokenKind::Eof, source_id: 7, lexeme_id: StrID(1 )},
            ],
        },
    }
}
