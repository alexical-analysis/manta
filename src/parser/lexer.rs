use serde::{Deserialize, Serialize};
use std::fmt;
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span,
}

impl Token {
    /// Create a new token with the given kind.
    pub fn new(kind: TokenKind, lexeme: String, span: Span) -> Self {
        Token { kind, lexeme, span }
    }

    /// Create an EOF token at the given span.
    pub fn eof(span: Span) -> Self {
        Token {
            kind: TokenKind::Eof,
            lexeme: String::new(),
            span,
        }
    }
}

#[derive(Debug)]
pub struct LexError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "LexError at {}..{}: {}",
            self.span.start, self.span.end, self.message
        )
    }
}

impl std::error::Error for LexError {}

/// Minimal lexer. Uses a byte cursor but iterates by `char`s for UTF-8 correctness.
#[derive(Clone)]
pub struct Lexer {
    input: String,
    pos: usize,
    len: usize,
    last_token_kind: Option<TokenKind>,
}

impl Lexer {
    /// Create a new lexer from source text.
    pub fn new(source: &str) -> Self {
        Lexer {
            input: source.to_string(),
            pos: 0,
            len: source.len(),
            last_token_kind: None,
        }
    }

    /// Non-consuming peek of the next token kind (may return LexError if scanning fails).
    pub fn peek(&self) -> Result<TokenKind, LexError> {
        let mut cloned = self.clone();
        let tok = cloned.next_token()?;
        Ok(tok.kind)
    }

    /// Return the next token.
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace_and_comments()?;

        if self.eof() {
            let span = Span::new(self.pos, self.pos);
            if self.is_end_of_statement() {
                self.last_token_kind = Some(TokenKind::Semicolon);
                return Ok(Token::new(TokenKind::Semicolon, String::new(), span));
            }

            self.last_token_kind = Some(TokenKind::Eof);
            return Ok(Token::eof(span));
        }

        // determine by first char
        let ch = self.current_char().unwrap();

        if ch == '\n' {
            // if this newline wasn't skipped it's because we need to insert a semicolon
            let start = self.pos;
            self.bump();
            let end = self.pos;

            self.last_token_kind = Some(TokenKind::Semicolon);
            return Ok(Token::new(
                TokenKind::Semicolon,
                "\n".to_string(),
                Span::new(start, end),
            ));
        }

        if ch == '}' && self.is_end_of_statement() {
            let pos = self.pos;
            self.last_token_kind = Some(TokenKind::Semicolon);
            return Ok(Token::new(
                TokenKind::Semicolon,
                String::new(),
                Span::new(pos, pos),
            ));
        }

        if is_ident_start(ch) {
            let token = self.read_ident_or_keyword();
            self.last_token_kind = Some(token.kind);
            return Ok(token);
        }

        if ch.is_ascii_digit() {
            let token = self.read_number();
            self.last_token_kind = Some(token.kind);
            return Ok(token);
        }

        if ch == '"' {
            let token = self.read_string()?;
            self.last_token_kind = Some(token.kind);
            return Ok(token);
        }

        // operators and punctuation
        let token = self.read_operator_or_punct();
        self.last_token_kind = Some(token.kind);
        Ok(token)
    }

    // internal helpers for the lexer

    fn eof(&self) -> bool {
        self.pos >= self.len
    }

    fn current_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn peek_char_n(&self, n: usize) -> Option<char> {
        let mut it = self.input[self.pos..].chars();
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
        self.input[start..self.pos].to_string()
    }

    fn skip_whitespace_and_comments(&mut self) -> Result<(), LexError> {
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
            if self.input[self.pos..].starts_with("//") {
                progressed = true;
                // consume until newline or EOF
                self.pos += 2;
                while let Some(ch) = self.current_char() {
                    self.bump();
                    if ch == '\n' {
                        break;
                    }
                }
            } else if self.input[self.pos..].starts_with("/*") {
                progressed = true;
                // consume block comment until */
                self.pos += 2;
                loop {
                    if self.eof() {
                        return Err(LexError {
                            message: "Unterminated block comment".to_string(),
                            span: Span::new(self.pos, self.pos),
                        });
                    }
                    if self.input[self.pos..].starts_with("*/") {
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
        Ok(())
    }

    fn is_end_of_statement(&mut self) -> bool {
        if let Some(kind) = self.last_token_kind {
            kind == TokenKind::Identifier
                || kind == TokenKind::Int
                || kind == TokenKind::Float
                || kind == TokenKind::Str
                || kind == TokenKind::TrueLiteral
                || kind == TokenKind::FalseLiteral
                || kind == TokenKind::ReturnKeyword
                || kind == TokenKind::BreakKeyword
                || kind == TokenKind::ContinueKey
                || kind == TokenKind::CloseBrace
                || kind == TokenKind::CloseParen
                || kind == TokenKind::CloseSquare
                || kind == TokenKind::Bang
        } else {
            false
        }
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

        Token::new(kind, lex, Span::new(start, end))
    }

    fn read_number(&mut self) -> Token {
        let start = self.pos;
        let mut seen_dot = false;
        let mut seen_exp = false;

        let mut s = String::new();
        while let Some(ch) = self.current_char() {
            if ch == '_' {
                // allow underscores between digits
                self.bump();
                s.push('_');
                continue;
            }
            if ch.is_ascii_digit() {
                self.bump();
                s.push(ch);
                continue;
            }
            if ch == '.' && !seen_dot {
                // check next char to avoid treating '.' in '..' as float
                if let Some(nc) = self.peek_char_n(1) {
                    if nc.is_ascii_digit() {
                        seen_dot = true;
                        self.bump();
                        s.push('.');
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
                s.push('e');
                if let Some(sign) = self.current_char() {
                    if sign == '+' || sign == '-' {
                        self.bump();
                        s.push(sign);
                    }
                }
                // digits of the exponent
                let digits = self.eat_while(|c| c.is_ascii_digit() || c == '_');
                s.push_str(&digits);
            }
        }

        let end = self.pos;
        let kind = if seen_dot || seen_exp {
            TokenKind::Float
        } else {
            TokenKind::Int
        };
        Token::new(kind, s, Span::new(start, end))
    }

    fn read_string(&mut self) -> Result<Token, LexError> {
        let start = self.pos;
        // consume the opening quote char
        let quote = self.bump().unwrap();
        debug_assert!(quote == '"');

        let mut out = String::new();
        while let Some(ch) = self.current_char() {
            self.bump();
            if ch == '"' {
                let end = self.pos;
                return Ok(Token::new(TokenKind::Str, out, Span::new(start, end)));
            }
            if ch == '\\' {
                if let Some(esc) = self.current_char() {
                    self.bump();
                    match esc {
                        'n' => out.push('\n'),
                        't' => out.push('\t'),
                        'r' => out.push('\r'),
                        '\\' => out.push('\\'),
                        '"' => out.push('"'),
                        other => {
                            // Unknown escape; keep it verbatim
                            out.push(other);
                        }
                    }
                    continue;
                } else {
                    return Err(LexError {
                        message: "Unfinished escape in string".to_string(),
                        span: Span::new(self.pos, self.pos),
                    });
                }
            }
            out.push(ch);
        }

        Err(LexError {
            message: "Unterminated string literal".to_string(),
            span: Span::new(start, self.pos),
        })
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
                return Token::new(kind, two, Span::new(start, self.pos));
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
        Token::new(kind, ch.to_string(), Span::new(start, self.pos))
    }
}

/// Iterator adapter that yields tokens including EOF, then terminates.
pub struct LexerIntoIter {
    lexer: Lexer,
    done: bool,
}

impl IntoIterator for Lexer {
    type Item = Result<Token, LexError>;
    type IntoIter = LexerIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        LexerIntoIter {
            lexer: self,
            done: false,
        }
    }
}

impl Iterator for LexerIntoIter {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        match self.lexer.next_token() {
            Ok(tok) => {
                if tok.kind == TokenKind::Eof {
                    self.done = true;
                    Some(Ok(tok))
                } else {
                    Some(Ok(tok))
                }
            }
            Err(e) => {
                self.done = true;
                Some(Err(e))
            }
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
        let tokens: Result<Vec<Token>, LexError> = lexer.into_iter().collect();

        let tokens = match tokens {
            Ok(t) => t,
            Err(e) => {
                panic!("Lexer error for {}: {}", file_name, e);
            }
        };

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
                        .map(|r| r.unwrap())
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
                Token::new(
                    TokenKind::LetKeyword,
                    "let".to_string(),
                    Span::new(0, 3),
                ),
                Token::new(TokenKind::Identifier, "x".to_string(), Span::new(4, 5)),
                Token::new(TokenKind::Equal, "=".to_string(), Span::new(6, 7)),
                Token::new(TokenKind::Int, "42".to_string(), Span::new(8, 10)),
                Token::new(TokenKind::Semicolon, String::new(), Span::new(10, 10)),
                Token::new(TokenKind::Eof, String::new(), Span::new(10, 10)),
            ],
        },
        lex_input_assign_with_new_line{
            input: "x = 5\n",
            want: vec![
            Token::new(TokenKind::Identifier, "x".to_string(), Span::new(0,1)),
            Token::new(TokenKind::Equal, "=".to_string(), Span::new(2,3)),
            Token::new(TokenKind::Int, "5".to_string(), Span::new(4,5)),
            Token::new(TokenKind::Semicolon, "\n".to_string(), Span::new(5,6)),
            Token::new(TokenKind::Eof, String::new(), Span::new(6,6)),
            ],
        },
        lex_input_string_and_escape {
            input: "\"hello\\n\"",
            want: vec![
                Token::new(TokenKind::Str, "hello\n".to_string(), Span::new(0, 9)),
                Token::new(TokenKind::Semicolon, String::new(), Span::new(9, 9)),
                Token::new(TokenKind::Eof, String::new(), Span::new(9, 9)),
            ],
        },
        lex_input_int_with_semicolon {
            input: "20;",
            want: vec![
                Token::new(TokenKind::Int, "20".to_string(), Span::new(0, 2)),
                Token::new(TokenKind::Semicolon, ";".to_string(), Span::new(2, 3)),
                Token::new(TokenKind::Eof, String::new(), Span::new(3, 3)),
            ],
        },
        lex_input_comments_and_whitespace {
            input: "  // top comment\nlet/*block*/ x = 1 // end\n",
            want: vec![
                Token::new(
                    TokenKind::LetKeyword,
                    "let".to_string(),
                    Span::new(17, 20),
                ),
                Token::new(TokenKind::Identifier, "x".to_string(), Span::new(30, 31)),
                Token::new(TokenKind::Equal, "=".to_string(), Span::new(32, 33)),
                Token::new(TokenKind::Int, "1".to_string(), Span::new(34, 35)),
                Token::new(TokenKind::Semicolon, String::new(), Span::new(43, 43)),
                Token::new(TokenKind::Eof, String::new(), Span::new(43, 43)),
            ],
        },
        lex_input_ints_and_floats {
            input: "3.14 1e10 2.5e-3 1_000",
            want: vec![
                Token::new(TokenKind::Float, "3.14".to_string(), Span::new(0, 4)),
                Token::new(TokenKind::Float, "1e10".to_string(), Span::new(5, 9)),
                Token::new(
                    TokenKind::Float,
                    "2.5e-3".to_string(),
                    Span::new(10, 16),
                ),
                Token::new(TokenKind::Int, "1_000".to_string(), Span::new(17, 22)),
                Token::new(TokenKind::Semicolon, String::new(), Span::new(22, 22)),
                Token::new(TokenKind::Eof, String::new(), Span::new(22, 22)),
            ],
        },
        lex_input_multi_char_operations {
            input: "a == b && c != d <= e >= f  = += *",
            want: vec![
                Token::new(TokenKind::Identifier, "a".to_string(), Span::new(0, 1)),
                Token::new(
                    TokenKind::EqualEqual,
                    "==".to_string(),
                    Span::new(2, 4),
                ),
                Token::new(TokenKind::Identifier, "b".to_string(), Span::new(5, 6)),
                Token::new(TokenKind::AndAnd, "&&".to_string(), Span::new(7, 9)),
                Token::new(TokenKind::Identifier, "c".to_string(), Span::new(10, 11)),
                Token::new(
                    TokenKind::NotEqual,
                    "!=".to_string(),
                    Span::new(12, 14),
                ),
                Token::new(TokenKind::Identifier, "d".to_string(), Span::new(15, 16)),
                Token::new(
                    TokenKind::LessOrEqual,
                    "<=".to_string(),
                    Span::new(17, 19),
                ),
                Token::new(TokenKind::Identifier, "e".to_string(), Span::new(20, 21)),
                Token::new(
                    TokenKind::GreaterOrEqual,
                    ">=".to_string(),
                    Span::new(22, 24),
                ),
                Token::new(TokenKind::Identifier, "f".to_string(), Span::new(25, 26)),
                Token::new(
                    TokenKind::Equal,
                    "=".to_string(),
                    Span::new(28, 29),
                ),
                Token::new(
                    TokenKind::PlusEqual,
                    "+=".to_string(),
                    Span::new(30, 32),
                ),
                Token::new(
                    TokenKind::Star,
                    "*".to_string(),
                    Span::new(33, 34),
                ),
                Token::new(TokenKind::Eof, String::new(), Span::new(34, 34)),
            ],

        },
        lex_input_variants_and_assignemnt {
            input: ".Ok x = 1",
            want: vec![
                Token::new(TokenKind::Dot, ".".to_string(), Span::new(0, 1)),
                Token::new(TokenKind::Identifier, "Ok".to_string(), Span::new(1, 3)),
                Token::new(TokenKind::Identifier, "x".to_string(), Span::new(4, 5)),
                Token::new(TokenKind::Equal, "=".to_string(), Span::new(6, 7)),
                Token::new(TokenKind::Int, "1".to_string(), Span::new(8, 9)),
                Token::new(TokenKind::Semicolon, String::new(), Span::new(9, 9)),
                Token::new(TokenKind::Eof, String::new(), Span::new(9, 9)),
            ],
        },
        lex_input_type_enum_tokens {
            input: "type ErrWrite enum { Ok; IOError }",
            want: vec![
                Token::new(
                    TokenKind::TypeKeyword,
                    "type".to_string(),
                    Span::new(0, 4),
                ),
                Token::new(
                    TokenKind::Identifier,
                    "ErrWrite".to_string(),
                    Span::new(5, 13),
                ),
                Token::new(
                    TokenKind::EnumKeyword,
                    "enum".to_string(),
                    Span::new(14, 18),
                ),
                Token::new(
                    TokenKind::OpenBrace,
                    "{".to_string(),
                    Span::new(19, 20),
                ),
                Token::new(TokenKind::Identifier, "Ok".to_string(), Span::new(21, 23)),
                Token::new(
                    TokenKind::Semicolon,
                    ";".to_string(),
                    Span::new(23, 24),
                ),
                Token::new(
                    TokenKind::Identifier,
                    "IOError".to_string(),
                    Span::new(25, 32),
                ),
                Token::new(
                    TokenKind::Semicolon,
                    "".to_string(),
                    Span::new(33, 33),
                ),
                Token::new(
                    TokenKind::CloseBrace,
                    "}".to_string(),
                    Span::new(33, 34),
                ),
                Token::new(TokenKind::Semicolon, String::new(), Span::new(34, 34)),
                Token::new(TokenKind::Eof, String::new(), Span::new(34, 34)),
            ],
        },
        lex_input_fn_decl {
            input: "fn write_and_cleanup(path str) WriteAndCleanup { }",
            want: vec![
                Token::new(
                    TokenKind::FnKeyword,
                    "fn".to_string(),
                    Span::new(0, 2),
                ),
                Token::new(
                    TokenKind::Identifier,
                    "write_and_cleanup".to_string(),
                    Span::new(3, 20),
                ),
                Token::new(
                    TokenKind::OpenParen,
                    "(".to_string(),
                    Span::new(20, 21),
                ),
                Token::new(
                    TokenKind::Identifier,
                    "path".to_string(),
                    Span::new(21, 25),
                ),
                Token::new(TokenKind::Identifier, "str".to_string(), Span::new(26, 29)),
                Token::new(
                    TokenKind::CloseParen,
                    ")".to_string(),
                    Span::new(29, 30),
                ),
                Token::new(
                    TokenKind::Identifier,
                    "WriteAndCleanup".to_string(),
                    Span::new(31, 46),
                ),
                Token::new(
                    TokenKind::OpenBrace,
                    "{".to_string(),
                    Span::new(47, 48),
                ),
                Token::new(
                    TokenKind::CloseBrace,
                    "}".to_string(),
                    Span::new(49, 50),
                ),
                Token::new(TokenKind::Semicolon, String::new(), Span::new(50, 50)),
                Token::new(TokenKind::Eof, String::new(), Span::new(50, 50)),
            ],
        },
        lex_input_let_or {
            input: "let .Ok(f) = os::open(path) or { return .IOError }",
            want: vec![
                Token::new(TokenKind::LetKeyword, "let".to_string(), Span::new(0, 3)),
                Token::new(TokenKind::Dot, ".".to_string(), Span::new(4, 5)),
                Token::new(TokenKind::Identifier, "Ok".to_string(), Span::new(5, 7)),
                Token::new(TokenKind::OpenParen, "(".to_string(), Span::new(7, 8)),
                Token::new(TokenKind::Identifier, "f".to_string(), Span::new(8, 9)),
                Token::new(
                    TokenKind::CloseParen,
                    ")".to_string(),
                    Span::new(9, 10),
                ),
                Token::new(
                    TokenKind::Equal,
                    "=".to_string(),
                    Span::new(11, 12),
                ),
                Token::new(TokenKind::Identifier, "os".to_string(), Span::new(13, 15)),
                Token::new(TokenKind::ColonColon, "::".to_string(), Span::new(15, 17)),
                Token::new(
                    TokenKind::Identifier,
                    "open".to_string(),
                    Span::new(17, 21),
                ),
                Token::new(
                    TokenKind::OpenParen,
                    "(".to_string(),
                    Span::new(21, 22),
                ),
                Token::new(
                    TokenKind::Identifier,
                    "path".to_string(),
                    Span::new(22, 26),
                ),
                Token::new(
                    TokenKind::CloseParen,
                    ")".to_string(),
                    Span::new(26, 27),
                ),
                Token::new(
                    TokenKind::OrKeyword,
                    "or".to_string(),
                    Span::new(28, 30),
                ),
                Token::new(
                    TokenKind::OpenBrace,
                    "{".to_string(),
                    Span::new(31, 32),
                ),
                Token::new(
                    TokenKind::ReturnKeyword,
                    "return".to_string(),
                    Span::new(33, 39),
                ),
                Token::new(TokenKind::Dot, ".".to_string(), Span::new(40, 41)),
                Token::new(
                    TokenKind::Identifier,
                    "IOError".to_string(),
                    Span::new(41, 48),
                ),
                Token::new(
                    TokenKind::Semicolon,
                    "".to_string(),
                    Span::new(49, 49),
                ),
                Token::new(
                    TokenKind::CloseBrace,
                    "}".to_string(),
                    Span::new(49, 50),
                ),
                Token::new(TokenKind::Semicolon, String::new(), Span::new(50, 50)),
                Token::new(TokenKind::Eof, String::new(), Span::new(50, 50)),
            ],
        },
        lex_input_pointer {
            input: "let .Ok(p) = maybe_alloc(false) !
print(*p)
free(p)
",
            want: vec![
                Token::new(TokenKind::LetKeyword, "let".to_string(), Span::new(0, 3)),
                Token::new(TokenKind::Dot, ".".to_string(), Span::new(4, 5)),
                Token::new(TokenKind::Identifier, "Ok".to_string(), Span::new(5, 7)),
                Token::new(TokenKind::OpenParen, "(".to_string(), Span::new(7, 8)),
                Token::new(TokenKind::Identifier, "p".to_string(), Span::new(8, 9)),
                Token::new(TokenKind::CloseParen, ")".to_string(), Span::new(9, 10)),
                Token::new(TokenKind::Equal, "=".to_string(), Span::new(11, 12)),
                Token::new(
                    TokenKind::Identifier,
                    "maybe_alloc".to_string(),
                    Span::new(13, 24),
                ),
                Token::new(
                    TokenKind::OpenParen,
                    "(".to_string(),
                    Span::new(24, 25),
                ),
                Token::new(
                    TokenKind::FalseLiteral,
                    "false".to_string(),
                    Span::new(25, 30),
                ),
                Token::new(
                    TokenKind::CloseParen,
                    ")".to_string(),
                    Span::new(30, 31),
                ),
                Token::new(TokenKind::Bang, "!".to_string(), Span::new(32, 33)),
                Token::new(
                    TokenKind::Semicolon,
                    "\n".to_string(),
                    Span::new(33, 34),
                ),
                Token::new(
                    TokenKind::Identifier,
                    "print".to_string(),
                    Span::new(34, 39),
                ),
                Token::new(
                    TokenKind::OpenParen,
                    "(".to_string(),
                    Span::new(39, 40),
                ),
                Token::new(TokenKind::Star, "*".to_string(), Span::new(40, 41)),
                Token::new(TokenKind::Identifier, "p".to_string(), Span::new(41, 42)),
                Token::new(
                    TokenKind::CloseParen,
                    ")".to_string(),
                    Span::new(42, 43),
                ),
                Token::new(
                    TokenKind::Semicolon,
                    "\n".to_string(),
                    Span::new(43, 44),
                ),
                Token::new(
                    TokenKind::Identifier,
                    "free".to_string(),
                    Span::new(44, 48),
                ),
                Token::new(
                    TokenKind::OpenParen,
                    "(".to_string(),
                    Span::new(48, 49),
                ),
                Token::new(TokenKind::Identifier, "p".to_string(), Span::new(49, 50)),
                Token::new(
                    TokenKind::CloseParen,
                    ")".to_string(),
                    Span::new(50, 51),
                ),
                Token::new(
                    TokenKind::Semicolon,
                    "\n".to_string(),
                    Span::new(51, 52),
                ),
                Token::new(TokenKind::Eof, String::new(), Span::new(52, 52)),
            ],
        },
    }

    #[test]
    fn lex_unterminated_string() {
        let src = "\"no end";
        let mut lx = Lexer::new(src);
        match lx.next_token() {
            Err(e) => assert!(
                e.message.contains("Unterminated string"),
                "unexpected error message: {}",
                e.message
            ),
            Ok(_) => panic!("expected lex error for unterminated string"),
        }
    }
}
