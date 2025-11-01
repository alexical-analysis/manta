use std::fmt;
use std::collections::HashSet;

/// Byte span in the source (start..end)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
	Ident,
	Int,
	Float,
	Str,
	Keyword,
	Operator,
	Punct,
	Eof,
}

/// A token produced by the lexer. `lexeme` contains raw text for ids/numbers/strings
/// (strings are returned unescaped), or None for simple punctuation/EOF.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: Option<String>,
    pub span: Span,
}

impl Token {
    /// Create a new token.
	pub fn new(kind: TokenKind, lexeme: Option<String>, span: Span) -> Self {
		Token { kind, lexeme, span }
	}

    /// Create an EOF token at the given span.
	pub fn eof(span: Span) -> Self {
		Token { kind: TokenKind::Eof, lexeme: None, span }
	}

	/// Serialize a single token to a line in the .lex format.
	pub fn to_lex_line(&self) -> String {
		fn escape_lexeme(s: &str) -> String {
			let mut out = String::with_capacity(s.len());
			for ch in s.chars() {
				match ch {
					'\n' => out.push_str("\\n"),
					'\t' => out.push_str("\\t"),
					'\r' => out.push_str("\\r"),
					'\\' => out.push_str("\\\\"),
					'"' => out.push_str("\\\"") ,
					c if (c as u32) < 0x20 => {
						out.push_str(&format!("\\x{:02X}", c as u32));
					}
					c => out.push(c),
				}
			}
			out
		}

		let kind_str = match self.kind {
			TokenKind::Ident => "IDENT",
			TokenKind::Int => "INT",
			TokenKind::Float => "FLOAT",
			TokenKind::Str => "STR",
			TokenKind::Keyword => "KEYWORD",
			TokenKind::Operator => "OPERATOR",
			TokenKind::Punct => "PUNCT",
			TokenKind::Eof => "EOF",
		};

		let lexeme = match &self.lexeme {
			Some(s) => escape_lexeme(s),
			None => String::new(),
		};

		format!("{} \"{}\" {} {}", kind_str, lexeme, self.span.start, self.span.end)
	}
}

/// Serialize a slice of tokens into the .lex textual format.
fn serialize_tokens(tokens: &[Token]) -> String {
	let mut out = String::new();
	for tok in tokens {
		out.push_str(&tok.to_lex_line());
		out.push('\n');
	}
	out
}

/// Write tokens to a file path in UTF-8 .lex format.
fn write_tokens_to_file(path: &std::path::Path, tokens: &[Token]) -> std::io::Result<()> {
	use std::fs::File;
	use std::io::Write;

	let s = serialize_tokens(tokens);
	let mut f = File::create(path)?;
	f.write_all(s.as_bytes())?;
	Ok(())
}

fn read_tokens_from_file(path: &std::path::Path) -> std::io::Result<Vec<Token>> {
    use std::fs::File;
    use std::io::{BufReader, BufRead};

    let f = File::open(path)?;
    let tokens = BufReader::new(f)
        .lines()
        .filter_map(|line_res| line_res.ok())
        .filter_map(|line| {
            let parts: Vec<&str> = line.splitn(4, ' ').collect();
            if parts.len() < 4 {
                return None;
            }
            let kind = match parts[0] {
                "IDENT" => TokenKind::Ident,
                "INT" => TokenKind::Int,
                "FLOAT" => TokenKind::Float,
                "STR" => TokenKind::Str,
                "KEYWORD" => TokenKind::Keyword,
                "OPERATOR" => TokenKind::Operator,
                "PUNCT" => TokenKind::Punct,
                "EOF" => TokenKind::Eof,
                _ => return None,
            };
            let lexeme = if parts[1].is_empty() {
                None
            } else {
                Some(parts[1].to_string())
            };
            let start = parts[2].parse::<usize>().ok()?;
            let end = parts[3].parse::<usize>().ok()?;
            Some(Token::new(kind, lexeme, Span::new(start, end)))
        })
        .collect();

    Ok(tokens)
}

#[derive(Debug)]
pub struct LexError {
	pub message: String,
	pub span: Span,
}

impl fmt::Display for LexError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "LexError at {}..{}: {}", self.span.start, self.span.end, self.message)
	}
}

impl std::error::Error for LexError {}

/// Minimal lexer. Uses a byte cursor but iterates by `char`s for UTF-8 correctness.
#[derive(Clone)]
pub struct Lexer {
	input: String,
	pos: usize,
	len: usize,
}

impl Lexer {
	/// Create a new lexer from source text.
	pub fn new(source: &str) -> Self {
		Lexer {
			input: source.to_string(),
			pos: 0,
			len: source.len(),
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
			return Ok(Token::eof(span));
		}

		// determine by first char
		let ch = self.current_char().unwrap();

		if is_ident_start(ch) {
			return Ok(self.read_ident_or_keyword());
		}

		if ch.is_ascii_digit() {
			return Ok(self.read_number());
		}

		if ch == '"' {
			return self.read_string();
		}

		// operators and punctuation
		return Ok(self.read_operator_or_punct());
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
		(0..n).for_each(|_| { let _ = it.next(); });
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
					if ch == '\n' { break; }
				}
			} else if self.input[self.pos..].starts_with("/*") {
				progressed = true;
				// consume block comment until */
				self.pos += 2;
				loop {
					if self.eof() {
						return Err(LexError { message: "Unterminated block comment".to_string(), span: Span::new(self.pos, self.pos) });
					}
					if self.input[self.pos..].starts_with("*/") {
						self.pos += 2;
						break;
					}
					self.bump();
				}
			}

			if !progressed { break; }
		}
		Ok(())
	}

	fn read_ident_or_keyword(&mut self) -> Token {
		let start = self.pos;
		let lex = self.eat_while(|ch| is_ident_continue(ch));
		let end = self.pos;

		let keywords: HashSet<&'static str> = [
			"let", "fn", "if", "else", "while", "for", "return", "true", "false",
			"match", "enum", "struct", "impl", "const", "pub", "mod",
		]
		.iter()
		.cloned()
		.collect();

		let kind = if keywords.contains(lex.as_str()) {
			TokenKind::Keyword
		} else {
			TokenKind::Ident
		};

		Token::new(kind, Some(lex), Span::new(start, end))
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
		let kind = if seen_dot || seen_exp { TokenKind::Float } else { TokenKind::Int };
		Token::new(kind, Some(s), Span::new(start, end))
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
				return Ok(Token::new(TokenKind::Str, Some(out), Span::new(start, end)));
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
					return Err(LexError { message: "Unfinished escape in string".to_string(), span: Span::new(self.pos, self.pos) });
				}
			}
			out.push(ch);
		}

		Err(LexError { message: "Unterminated string literal".to_string(), span: Span::new(start, self.pos) })
	}

	fn read_operator_or_punct(&mut self) -> Token {
		let start = self.pos;
		let ch = self.bump().unwrap();
		let next = self.current_char();

		// multi-char operators
		if let Some(nc) = next {
			let two = format!("{}{}", ch, nc);
			let two_ops: [&str; 10] = ["==", "!=", "<=", ">=", "&&", "||", ":=", "+=", "-=", "**"];
			if two_ops.contains(&two.as_str()) {
				self.bump();
				return Token::new(TokenKind::Operator, Some(two), Span::new(start, self.pos));
			}
		}

		// single char punctuation vs operator
		let puncts: [char; 10] = ['(', ')', '{', '}', '[', ']', ',', ';', ':', '.'];
		if puncts.contains(&ch) {
			return Token::new(TokenKind::Punct, Some(ch.to_string()), Span::new(start, self.pos));
		}

		// otherwise treat as operator
		Token::new(TokenKind::Operator, Some(ch.to_string()), Span::new(start, self.pos))
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
		LexerIntoIter { lexer: self, done: false }
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

    struct InputCase{
        name: &'static str,
        input: &'static str,
        expected_tokens: Vec<Token>,
    }

    #[test]
    fn lex_inputs() {
		let cases = vec![
			InputCase{
				name: "simple let statement",
				input: "let x = 42",
				expected_tokens: vec![
					Token::new(TokenKind::Keyword, Some("let".to_string()), Span::new(0, 3)),
					Token::new(TokenKind::Ident, Some("x".to_string()), Span::new(4, 5)),
					Token::new(TokenKind::Operator, Some("=".to_string()), Span::new(6,7)),
					Token::new(TokenKind::Int, Some("42".to_string()), Span::new(8, 10)),
					Token::new(TokenKind::Eof, None, Span::new(10,10)),
				],
			},
			InputCase{
				name: "string and escape",
				input: "\"hello\\n\"",
				expected_tokens: vec![
					Token::new(TokenKind::Str, Some("hello\n".to_string()), Span::new(0, 9)),
					Token::new(TokenKind::Eof, None, Span::new(9,9)),
				],
			}
			,
			InputCase{
				name: "comments and whitespace",
				input: "  // top comment\nlet/*block*/ x = 1 // end\n",
				expected_tokens: vec![
					Token::new(TokenKind::Keyword, Some("let".to_string()), Span::new(17,20)),
					Token::new(TokenKind::Ident, Some("x".to_string()), Span::new(30,31)),
					Token::new(TokenKind::Operator, Some("=".to_string()), Span::new(32,33)),
					Token::new(TokenKind::Int, Some("1".to_string()), Span::new(34,35)),
					Token::new(TokenKind::Eof, None, Span::new(43,43)),
				],
			}
			,
			InputCase{
				name: "numbers and floats",
				input: "3.14 1e10 2.5e-3 1_000",
				expected_tokens: vec![
					Token::new(TokenKind::Float, Some("3.14".to_string()), Span::new(0,4)),
					Token::new(TokenKind::Float, Some("1e10".to_string()), Span::new(5,9)),
					Token::new(TokenKind::Float, Some("2.5e-3".to_string()), Span::new(10,16)),
					Token::new(TokenKind::Int, Some("1_000".to_string()), Span::new(17,22)),
					Token::new(TokenKind::Eof, None, Span::new(22,22)),
				],
			}
			,
			InputCase{
				name: "multi char operators",
				input: "a == b && c != d <= e >= f := += **",
				expected_tokens: vec![
					Token::new(TokenKind::Ident, Some("a".to_string()), Span::new(0,1)),
					Token::new(TokenKind::Operator, Some("==".to_string()), Span::new(2,4)),
					Token::new(TokenKind::Ident, Some("b".to_string()), Span::new(5,6)),
					Token::new(TokenKind::Operator, Some("&&".to_string()), Span::new(7,9)),
					Token::new(TokenKind::Ident, Some("c".to_string()), Span::new(10,11)),
					Token::new(TokenKind::Operator, Some("!=".to_string()), Span::new(12,14)),
					Token::new(TokenKind::Ident, Some("d".to_string()), Span::new(15,16)),
					Token::new(TokenKind::Operator, Some("<=".to_string()), Span::new(17,19)),
					Token::new(TokenKind::Ident, Some("e".to_string()), Span::new(20,21)),
					Token::new(TokenKind::Operator, Some(">=".to_string()), Span::new(22,24)),
					Token::new(TokenKind::Ident, Some("f".to_string()), Span::new(25,26)),
					Token::new(TokenKind::Operator, Some(":=".to_string()), Span::new(27,29)),
					Token::new(TokenKind::Operator, Some("+=".to_string()), Span::new(30,32)),
					Token::new(TokenKind::Operator, Some("**".to_string()), Span::new(33,35)),
					Token::new(TokenKind::Eof, None, Span::new(35,35)),
				],
			}
			,
			InputCase{
				name: "variant and assignment",
				input: ".Ok x = 1",
				expected_tokens: vec![
					Token::new(TokenKind::Punct, Some(".".to_string()), Span::new(0,1)),
					Token::new(TokenKind::Ident, Some("Ok".to_string()), Span::new(1,3)),
					Token::new(TokenKind::Ident, Some("x".to_string()), Span::new(4,5)),
					Token::new(TokenKind::Operator, Some("=".to_string()), Span::new(6,7)),
					Token::new(TokenKind::Int, Some("1".to_string()), Span::new(8,9)),
					Token::new(TokenKind::Eof, None, Span::new(9,9)),
				],
			}
			,
			InputCase{
				name: "type enum tokens",
				input: "type ErrWrite enum { Ok IOError }",
				expected_tokens: vec![
					Token::new(TokenKind::Ident, Some("type".to_string()), Span::new(0,4)),
					Token::new(TokenKind::Ident, Some("ErrWrite".to_string()), Span::new(5,13)),
					Token::new(TokenKind::Keyword, Some("enum".to_string()), Span::new(14,18)),
					Token::new(TokenKind::Punct, Some("{".to_string()), Span::new(19,20)),
					Token::new(TokenKind::Ident, Some("Ok".to_string()), Span::new(21,23)),
					Token::new(TokenKind::Ident, Some("IOError".to_string()), Span::new(24,31)),
					Token::new(TokenKind::Punct, Some("}".to_string()), Span::new(32,33)),
					Token::new(TokenKind::Eof, None, Span::new(33,33)),
				],
			}
			,
			InputCase{
				name: "fn signature tokens",
				input: "fn write_and_cleanup(path string) ErrWrite { }",
				expected_tokens: vec![
					Token::new(TokenKind::Keyword, Some("fn".to_string()), Span::new(0,2)),
					Token::new(TokenKind::Ident, Some("write_and_cleanup".to_string()), Span::new(3,20)),
					Token::new(TokenKind::Punct, Some("(".to_string()), Span::new(20,21)),
					Token::new(TokenKind::Ident, Some("path".to_string()), Span::new(21,25)),
					Token::new(TokenKind::Ident, Some("string".to_string()), Span::new(26,32)),
					Token::new(TokenKind::Punct, Some(")".to_string()), Span::new(32,33)),
					Token::new(TokenKind::Ident, Some("ErrWrite".to_string()), Span::new(34,42)),
					Token::new(TokenKind::Punct, Some("{".to_string()), Span::new(43,44)),
					Token::new(TokenKind::Punct, Some("}".to_string()), Span::new(45,46)),
					Token::new(TokenKind::Eof, None, Span::new(46,46)),
				],
			}
			,
			InputCase{
				name: "try catch line tokens",
				input: ".Ok(f) := try open(path) catch { return .IOError }",
				expected_tokens: vec![
					Token::new(TokenKind::Punct, Some(".".to_string()), Span::new(0,1)),
					Token::new(TokenKind::Ident, Some("Ok".to_string()), Span::new(1,3)),
					Token::new(TokenKind::Punct, Some("(".to_string()), Span::new(3,4)),
					Token::new(TokenKind::Ident, Some("f".to_string()), Span::new(4,5)),
					Token::new(TokenKind::Punct, Some(")".to_string()), Span::new(5,6)),
					Token::new(TokenKind::Operator, Some(":=".to_string()), Span::new(7,9)),
					Token::new(TokenKind::Ident, Some("try".to_string()), Span::new(10,13)),
					Token::new(TokenKind::Ident, Some("open".to_string()), Span::new(14,18)),
					Token::new(TokenKind::Punct, Some("(".to_string()), Span::new(18,19)),
					Token::new(TokenKind::Ident, Some("path".to_string()), Span::new(19,23)),
					Token::new(TokenKind::Punct, Some(")".to_string()), Span::new(23,24)),
					Token::new(TokenKind::Ident, Some("catch".to_string()), Span::new(25,30)),
					Token::new(TokenKind::Punct, Some("{".to_string()), Span::new(31,32)),
					Token::new(TokenKind::Keyword, Some("return".to_string()), Span::new(33,39)),
					Token::new(TokenKind::Punct, Some(".".to_string()), Span::new(40,41)),
					Token::new(TokenKind::Ident, Some("IOError".to_string()), Span::new(41,48)),
					Token::new(TokenKind::Punct, Some("}".to_string()), Span::new(49,50)),
					Token::new(TokenKind::Eof, None, Span::new(50,50)),
				],
			}
			,
			InputCase{
				name: "pointer and nil tokens",
				input: "p := maybe_alloc(false)
if p != nil {
    print(*p)
    free(p)
}",
				expected_tokens: vec![
					Token::new(TokenKind::Ident, Some("p".to_string()), Span::new(0,1)),
					Token::new(TokenKind::Operator, Some(":=".to_string()), Span::new(2,4)),
					Token::new(TokenKind::Ident, Some("maybe_alloc".to_string()), Span::new(5,16)),
					Token::new(TokenKind::Punct, Some("(".to_string()), Span::new(16,17)),
					Token::new(TokenKind::Keyword, Some("false".to_string()), Span::new(17,22)),
					Token::new(TokenKind::Punct, Some(")".to_string()), Span::new(22,23)),
					Token::new(TokenKind::Keyword, Some("if".to_string()), Span::new(24,26)),
					Token::new(TokenKind::Ident, Some("p".to_string()), Span::new(27,28)),
					Token::new(TokenKind::Operator, Some("!=".to_string()), Span::new(29,31)),
					Token::new(TokenKind::Ident, Some("nil".to_string()), Span::new(32,35)),
					Token::new(TokenKind::Punct, Some("{".to_string()), Span::new(36,37)),
					Token::new(TokenKind::Ident, Some("print".to_string()), Span::new(42,47)),
					Token::new(TokenKind::Punct, Some("(".to_string()), Span::new(47,48)),
					Token::new(TokenKind::Operator, Some("*".to_string()), Span::new(48,49)),
					Token::new(TokenKind::Ident, Some("p".to_string()), Span::new(49,50)),
					Token::new(TokenKind::Punct, Some(")".to_string()), Span::new(50,51)),
					Token::new(TokenKind::Ident, Some("free".to_string()), Span::new(56,60)),
					Token::new(TokenKind::Punct, Some("(".to_string()), Span::new(60,61)),
					Token::new(TokenKind::Ident, Some("p".to_string()), Span::new(61,62)),
					Token::new(TokenKind::Punct, Some(")".to_string()), Span::new(62,63)),
					Token::new(TokenKind::Punct, Some("}".to_string()), Span::new(64,65)),
					Token::new(TokenKind::Eof, None, Span::new(65,65)),
				],
			}
		];

        for case in cases {
            let toks: Vec<_> = Lexer::new(case.input).into_iter().map(|r| r.unwrap()).collect();
            assert_eq!(toks, case.expected_tokens, "{}\n\tFailed lexing input: {}", case.name, case.input);
        }
    }

    #[test]
    fn lex_unterminated_string() {
        let src = "\"no end";
		let mut lx = Lexer::new(src);
		match lx.next_token() {
			Err(e) => assert!(e.message.contains("Unterminated string"), "unexpected error message: {}", e.message),
			Ok(_) => panic!("expected lex error for unterminated string"),
		}
    }
}