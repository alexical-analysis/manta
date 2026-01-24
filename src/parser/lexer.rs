use serde::{Deserialize, Serialize};
use strum_macros::{Display, EnumString};

/// Byte span in the source (start..end)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

/// The kind of Token produced by the lexer.
#[derive(Debug, Display, EnumString, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// Minimal lexer. Uses a byte cursor but iterates by `char`s for UTF-8 correctness.
pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
    done: bool,
    prev_kind: TokenKind,
    next: Token,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer from source text.
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer {
            source,
            pos: 0,
            done: false,
            prev_kind: TokenKind::Identifier,
            next: Token {
                kind: TokenKind::Identifier,
                span: Span::new(0, 0),
            },
        };
        lexer.next_token();

        lexer
    }

    // TODO: add tests for this function
    pub fn lexeme(&self, token: Token) -> String {
        match token.kind {
            TokenKind::Str => self.source[token.span.start + 1..token.span.end - 1].to_string(),
            _ => self.source[token.span.start..token.span.end].to_string(),
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
            let span = Span::new(self.pos, self.pos);
            if self.is_end_of_statement() {
                return Token {
                    kind: TokenKind::Semicolon,
                    span,
                };
            }

            return Token {
                kind: TokenKind::Eof,
                span,
            };
        }
        let ch = ch.unwrap();

        if ch == '\n' {
            // if this newline wasn't skipped it's because we need to insert a semicolon
            let start = self.pos;
            self.bump();
            let end = self.pos;

            return Token {
                kind: TokenKind::Semicolon,
                span: Span::new(start, end),
            };
        }

        if ch == '}' && self.is_end_of_statement() {
            let pos = self.pos;
            return Token {
                kind: TokenKind::Semicolon,
                span: Span::new(pos, pos),
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
        // TODO: this should probably just be a simple loop
        // having this method that takes a closure is overkill
        // I think. I want to wait till I can test things again though
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

        Token {
            kind,
            span: Span::new(start, end),
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

                // TODO: this should probably just be a simple loop

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
        Token {
            kind,
            span: Span::new(start, end),
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
                return Token {
                    kind: TokenKind::Str,
                    span: Span::new(start, end),
                };
            }
            if ch == '\\' {
                match self.current_char() {
                    Some(_) => {
                        self.bump();
                        continue;
                    }
                    None => {
                        return Token {
                            kind: TokenKind::MalformedStr,
                            span: Span::new(self.pos, self.pos),
                        };
                    }
                }
            }
        }

        Token {
            kind: TokenKind::MalformedStr,
            span: Span::new(start, self.pos),
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
                return Token {
                    kind,
                    span: Span::new(start, self.pos),
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

        // otherwise treat as operator
        Token {
            kind,
            span: Span::new(start, self.pos),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let token = self.next_token();
        if token.kind == TokenKind::Eof {
            self.done = true;
        }

        Some(token)
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
    use super::*;
    use pretty_assertions::assert_eq;
    use std::fs::{self, DirEntry};
    use std::path::Path;

    #[test]
    fn lex_file_tests() {
        let test_dir = Path::new("tests/src");
        let lex_dir = Path::new("tests/lexer");

        if !test_dir.exists() {
            panic!(
                "Test directory does not exist. Please create a '{:?}' with test .manta files.",
                test_dir
            );
        }

        let entries = fs::read_dir(test_dir).expect("Failed to read tests/src directory");

        // Read all .manta files from tests/src and check them against expected lexer output in tests/lexer
        for entry in entries {
            assert_file_eq(entry, test_dir, lex_dir);
        }
    }

    fn assert_file_eq(entry: Result<DirEntry, std::io::Error>, test_dir: &Path, lex_dir: &Path) {
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
            Err(_) => panic!("Failed to read {}", path.display()),
        };

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.into_iter().collect();

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
            // Create the lex directory if it doesn't exist
            fs::create_dir_all(lex_dir).expect("Failed to create lexer test directory");

            // Write the output if the file does not exist
            match fs::write(&lex_file, &json_output) {
                Ok(_) => (),
                Err(_) => panic!("Failed to write lexer output to {:?}", lex_file),
            };

            // If we generated the output file, fail the test to prompt the user to verify it's correctness
            panic!(
                "Generated new lexer output file: {:?}. Please verify its correctness.",
                lex_file
            );
        }
    }

    macro_rules! test_lex_inputs {
        ( $( $case:ident { input: $input:expr, want: $want:expr,  } ),*, ) => {
            $(
                #[test]
                fn $case() {
                    let toks: Vec<_> = Lexer::new($input)
                        .into_iter()
                        .collect();
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
                    span: Span::new(0, 3),
                },
                Token{kind: TokenKind::Identifier, span: Span::new(4, 5)},
                Token{kind: TokenKind::Equal,  span: Span::new(6, 7)},
                Token{kind: TokenKind::Int, span: Span::new(8, 10)},
                Token{kind: TokenKind::Semicolon, span: Span::new(10, 10)},
                Token{kind: TokenKind::Eof, span: Span::new(10, 10)},
            ],
        },
        lex_input_assign_with_new_line{
            input: "x = 5\n",
            want: vec![
                Token{kind: TokenKind::Identifier, span: Span::new(0,1)},
                Token{kind: TokenKind::Equal, span: Span::new(2,3)},
                Token{kind: TokenKind::Int, span: Span::new(4,5)},
                Token{kind: TokenKind::Semicolon, span: Span::new(5,6)},
                Token{kind: TokenKind::Eof, span: Span::new(6,6)},
            ],
        },
        lex_input_string_and_escape {
            input: "\"hello\\n\"",
            want: vec![
                Token{kind: TokenKind::Str, span: Span::new(0, 9)},
                Token{kind: TokenKind::Semicolon, span: Span::new(9, 9)},
                Token{kind: TokenKind::Eof, span: Span::new(9, 9)},
            ],
        },
        lex_input_int_with_semicolon {
            input: "20;",
            want: vec![
                Token{kind: TokenKind::Int, span: Span::new(0, 2)},
                Token{kind: TokenKind::Semicolon, span: Span::new(2, 3)},
                Token{kind: TokenKind::Eof, span: Span::new(3, 3)},
            ],
        },
        lex_input_comments_and_whitespace {
            input: "  // top comment\nlet/*block*/ x = 1 // end\n",
            want: vec![
                Token{
                    kind: TokenKind::LetKeyword,
                    span: Span::new(17, 20),
                },
                Token{kind: TokenKind::Identifier, span: Span::new(30, 31)},
                Token{kind: TokenKind::Equal, span: Span::new(32, 33)},
                Token{kind: TokenKind::Int, span: Span::new(34, 35)},
                Token{kind: TokenKind::Semicolon, span: Span::new(43, 43)},
                Token{kind: TokenKind::Eof, span: Span::new(43, 43)},
            ],
        },
        lex_input_ints_and_floats {
            input: "3.14 1e10 2.5e-3 1_000",
            want: vec![
                Token{kind: TokenKind::Float, span: Span::new(0, 4)},
                Token{kind: TokenKind::Float, span: Span::new(5, 9)},
                Token{
                    kind: TokenKind::Float,
                    span: Span::new(10, 16),
                },
                Token{kind: TokenKind::Int,span: Span::new(17, 22)},
                Token{kind: TokenKind::Semicolon, span: Span::new(22, 22)},
                Token{kind: TokenKind::Eof,span: Span::new(22, 22)},
            ],
        },
        lex_input_multi_char_operations {
            input: "a == b && c != d <= e >= f  = += *",
            want: vec![
                Token{kind: TokenKind::Identifier,  span: Span::new(0, 1)},
                Token{
                    kind: TokenKind::EqualEqual,
                    span: Span::new(2, 4),
                },
                Token{kind: TokenKind::Identifier, span: Span::new(5, 6)},
                Token{kind: TokenKind::AndAnd, span: Span::new(7, 9)},
                Token{kind: TokenKind::Identifier, span: Span::new(10, 11)},
                Token{
                    kind: TokenKind::NotEqual,
                    span: Span::new(12, 14),
                },
                Token{kind: TokenKind::Identifier, span: Span::new(15, 16)},
                Token{
                    kind: TokenKind::LessOrEqual,
                    span: Span::new(17, 19),
                },
                Token{kind: TokenKind::Identifier, span: Span::new(20, 21)},
                Token{
                    kind: TokenKind::GreaterOrEqual,
                    span: Span::new(22, 24),
                },
                Token{kind: TokenKind::Identifier, span: Span::new(25, 26)},
                Token{
                    kind: TokenKind::Equal,
                    span: Span::new(28, 29),
                },
                Token{
                    kind: TokenKind::PlusEqual,
                    span: Span::new(30, 32),
                },
                Token{
                    kind: TokenKind::Star,
                    span: Span::new(33, 34),
                },
                Token{kind: TokenKind::Eof, span: Span::new(34, 34)},
            ],

        },
        lex_input_variants_and_assignemnt {
            input: ".Ok x = 1",
            want: vec![
                Token { kind: TokenKind::Dot, span: Span::new(0, 1) },
                Token { kind: TokenKind::Identifier, span: Span::new(1, 3) },
                Token { kind: TokenKind::Identifier, span: Span::new(4, 5) },
                Token { kind: TokenKind::Equal, span: Span::new(6, 7) },
                Token { kind: TokenKind::Int, span: Span::new(8, 9) },
                Token { kind: TokenKind::Semicolon, span: Span::new(9, 9) },
                Token { kind: TokenKind::Eof, span: Span::new(9, 9) },
            ],
        },
        lex_input_type_enum_tokens {
            input: "type ErrWrite enum { Ok; IOError }",
            want: vec![
                Token { kind: TokenKind::TypeKeyword, span: Span::new(0, 4) },
                Token { kind: TokenKind::Identifier, span: Span::new(5, 13) },
                Token { kind: TokenKind::EnumKeyword, span: Span::new(14, 18) },
                Token { kind: TokenKind::OpenBrace, span: Span::new(19, 20) },
                Token { kind: TokenKind::Identifier, span: Span::new(21, 23) },
                Token { kind: TokenKind::Semicolon, span: Span::new(23, 24) },
                Token { kind: TokenKind::Identifier, span: Span::new(25, 32) },
                Token { kind: TokenKind::Semicolon, span: Span::new(33, 33) },
                Token { kind: TokenKind::CloseBrace, span: Span::new(33, 34) },
                Token { kind: TokenKind::Semicolon, span: Span::new(34, 34) },
                Token { kind: TokenKind::Eof, span: Span::new(34, 34) },
            ],
        },
        lex_input_fn_decl {
            input: "fn write_and_cleanup(path str) WriteAndCleanup { }",
            want: vec![
                Token { kind: TokenKind::FnKeyword, span: Span::new(0, 2) },
                Token { kind: TokenKind::Identifier, span: Span::new(3, 20) },
                Token { kind: TokenKind::OpenParen, span: Span::new(20, 21) },
                Token { kind: TokenKind::Identifier, span: Span::new(21, 25) },
                Token { kind: TokenKind::Identifier, span: Span::new(26, 29) },
                Token { kind: TokenKind::CloseParen, span: Span::new(29, 30) },
                Token { kind: TokenKind::Identifier, span: Span::new(31, 46) },
                Token { kind: TokenKind::OpenBrace, span: Span::new(47, 48) },
                Token { kind: TokenKind::CloseBrace, span: Span::new(49, 50) },
                Token { kind: TokenKind::Semicolon, span: Span::new(50, 50) },
                Token { kind: TokenKind::Eof, span: Span::new(50, 50) },
            ],
        },
        lex_input_let_or {
            input: "let .Ok(f) = os::open(path) or { return .IOError }",
            want: vec![
                Token { kind: TokenKind::LetKeyword, span: Span::new(0, 3) },
                Token { kind: TokenKind::Dot, span: Span::new(4, 5) },
                Token { kind: TokenKind::Identifier, span: Span::new(5, 7) },
                Token { kind: TokenKind::OpenParen, span: Span::new(7, 8) },
                Token { kind: TokenKind::Identifier, span: Span::new(8, 9) },
                Token { kind: TokenKind::CloseParen, span: Span::new(9, 10) },
                Token { kind: TokenKind::Equal, span: Span::new(11, 12) },
                Token { kind: TokenKind::Identifier, span: Span::new(13, 15) },
                Token { kind: TokenKind::ColonColon, span: Span::new(15, 17) },
                Token { kind: TokenKind::Identifier, span: Span::new(17, 21) },
                Token { kind: TokenKind::OpenParen, span: Span::new(21, 22) },
                Token { kind: TokenKind::Identifier, span: Span::new(22, 26) },
                Token { kind: TokenKind::CloseParen, span: Span::new(26, 27) },
                Token { kind: TokenKind::OrKeyword, span: Span::new(28, 30) },
                Token { kind: TokenKind::OpenBrace, span: Span::new(31, 32) },
                Token { kind: TokenKind::ReturnKeyword, span: Span::new(33, 39) },
                Token { kind: TokenKind::Dot, span: Span::new(40, 41) },
                Token { kind: TokenKind::Identifier, span: Span::new(41, 48) },
                Token { kind: TokenKind::Semicolon, span: Span::new(49, 49) },
                Token { kind: TokenKind::CloseBrace, span: Span::new(49, 50) },
                Token { kind: TokenKind::Semicolon, span: Span::new(50, 50) },
                Token { kind: TokenKind::Eof, span: Span::new(50, 50) },
            ],
        },
        lex_input_pointer {
            input: "let .Ok(p) = maybe_alloc(false) !
print(*p)
free(p)
",
            want: vec![
                Token { kind: TokenKind::LetKeyword, span: Span::new(0, 3) },
                Token { kind: TokenKind::Dot, span: Span::new(4, 5) },
                Token { kind: TokenKind::Identifier, span: Span::new(5, 7) },
                Token { kind: TokenKind::OpenParen, span: Span::new(7, 8) },
                Token { kind: TokenKind::Identifier, span: Span::new(8, 9) },
                Token { kind: TokenKind::CloseParen, span: Span::new(9, 10) },
                Token { kind: TokenKind::Equal, span: Span::new(11, 12) },
                Token { kind: TokenKind::Identifier, span: Span::new(13, 24) },
                Token { kind: TokenKind::OpenParen, span: Span::new(24, 25) },
                Token { kind: TokenKind::FalseLiteral, span: Span::new(25, 30) },
                Token { kind: TokenKind::CloseParen, span: Span::new(30, 31) },
                Token { kind: TokenKind::Bang, span: Span::new(32, 33) },
                Token { kind: TokenKind::Semicolon, span: Span::new(33, 34) },
                Token { kind: TokenKind::Identifier, span: Span::new(34, 39) },
                Token { kind: TokenKind::OpenParen, span: Span::new(39, 40) },
                Token { kind: TokenKind::Star, span: Span::new(40, 41) },
                Token { kind: TokenKind::Identifier, span: Span::new(41, 42) },
                Token { kind: TokenKind::CloseParen, span: Span::new(42, 43) },
                Token { kind: TokenKind::Semicolon, span: Span::new(43, 44) },
                Token { kind: TokenKind::Identifier, span: Span::new(44, 48) },
                Token { kind: TokenKind::OpenParen, span: Span::new(48, 49) },
                Token { kind: TokenKind::Identifier, span: Span::new(49, 50) },
                Token { kind: TokenKind::CloseParen, span: Span::new(50, 51) },
                Token { kind: TokenKind::Semicolon, span: Span::new(51, 52) },
                Token { kind: TokenKind::Eof, span: Span::new(52, 52) },
            ],
        },
        lex_unterminated_string {
            input: r#""no end"#,
            want: vec![
                Token { kind: TokenKind::MalformedStr, span: Span::new(0, 7) },
                Token { kind: TokenKind::Eof, span: Span::new(7, 7) },
            ],
        },
    }
}
