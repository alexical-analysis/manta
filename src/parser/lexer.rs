use crate::str_store::{StrID, StrStore};
use serde::Serialize;
use strum_macros::{Display, EnumString};

/// The type of the Token produced by the lexer.
#[derive(Debug, Display, EnumString, Copy, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum Ty {
    Unknown,
    Identifier,
    Int,
    Float,
    Str,
    MalformedStr, // this is used for unterminated strings
    TrueLiteral,
    FalseLiteral,
    FnKeyword,
    IfKeyword,
    InKeyword,
    AsKeyword,
    ReturnKeyword,
    ElseKeyword,
    WhileKeyword,
    ForKeyword,
    LoopKeyword,
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
    PubKeyword,
    ModKeyword,
    UseKeyword,
    MutKeyword,
    OrKeyword,
    WrapKeyword,
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    RangeExclusive,
    RangeInclusive,
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
    // Technically '..' is not a valid token in Manta but we lex it to provide better diagnostic errors
    // since using '..' for a range is a common pattern in other langugaes with range expressions
    DotDot,
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
    Eof,
}

/// The uniqe identifier of the token in a given FileSet
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct Pos(u32);

impl From<usize> for Pos {
    fn from(v: usize) -> Self {
        Pos(v as u32)
    }
}

/// A token produced by the lexer
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize)]
pub struct Token {
    pub ty: Ty,
    pub pos: Pos,
    pub lexeme: StrID,
}

impl Token {
    pub fn new_open_paren(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::OpenParen,
            pos,
            lexeme: str_store.get_id("("),
        }
    }

    pub fn new_close_paren(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::CloseParen,
            pos,
            lexeme: str_store.get_id(")"),
        }
    }

    pub fn new_open_brace(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::OpenBrace,
            pos,
            lexeme: str_store.get_id("{"),
        }
    }

    pub fn new_close_brace(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::CloseBrace,
            pos,
            lexeme: str_store.get_id("}"),
        }
    }

    pub fn new_range_inclusive(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::RangeInclusive,
            pos,
            lexeme: str_store.get_id("..="),
        }
    }

    pub fn new_range_exclusive(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::RangeExclusive,
            pos,
            lexeme: str_store.get_id("..<"),
        }
    }

    pub fn new_module(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::ColonColon,
            pos,
            lexeme: str_store.get_id("::"),
        }
    }

    pub fn new_plus(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::Plus,
            pos,
            lexeme: str_store.get_id("+"),
        }
    }

    pub fn new_minus(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::Minus,
            pos,
            lexeme: str_store.get_id("-"),
        }
    }

    pub fn new_multiply(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::Star,
            pos,
            lexeme: str_store.get_id("*"),
        }
    }

    pub fn new_divide(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::Slash,
            pos,
            lexeme: str_store.get_id("/"),
        }
    }

    pub fn new_equal(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::Equal,
            pos,
            lexeme: str_store.get_id("="),
        }
    }

    pub fn new_equal_equal(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::EqualEqual,
            pos,
            lexeme: str_store.get_id("=="),
        }
    }

    pub fn new_less_than(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::LessThan,
            pos,
            lexeme: str_store.get_id("<"),
        }
    }

    pub fn new_semicolon(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::Semicolon,
            pos,
            lexeme: str_store.get_id(";"),
        }
    }

    pub fn new_comma(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::Comma,
            pos,
            lexeme: str_store.get_id(","),
        }
    }

    pub fn new_eof(str_store: &mut StrStore, pos: Pos) -> Self {
        Self {
            ty: Ty::Eof,
            pos,
            lexeme: str_store.get_id(""),
        }
    }
}

/// Manta lexer for converting source code into a token stream
pub struct Lexer<'s> {
    source: &'s str,
    base: usize,
    pos: usize,
    is_at_eos: bool,
    next: Token,
}

impl<'s> Lexer<'s> {
    pub fn new(str_store: &mut StrStore, source: &'s str, base: usize) -> Self {
        let mut lexer = Self {
            source,
            base,
            pos: 0,
            is_at_eos: false,
            next: Token::new_eof(str_store, Pos(base as u32)),
        };

        // populate the first token so the lexer is ready to go
        lexer.next(str_store);

        lexer
    }

    /// if the lexer is in a bad spot, this will just eat tokens till we find the next decl
    pub fn recover_until_decl(&mut self, str_store: &mut StrStore) {
        loop {
            let token = self.next(str_store);
            if [
                Ty::FnKeyword,
                Ty::UseKeyword,
                Ty::ModKeyword,
                Ty::TypeKeyword,
            ]
            .contains(&token.ty)
            {
                break;
            }
        }
    }

    /// if the lexer is in a bad spot, this will just eat tokens till we find the next expression
    pub fn recover_until_expr(&mut self, str_store: &mut StrStore) {
        loop {
            let token = self.next(str_store);
            if [Ty::Semicolon, Ty::CloseBrace].contains(&token.ty) {
                self.next(str_store);
                break;
            }
        }
    }

    pub fn peek(&self) -> &Token {
        &self.next
    }

    pub fn next(&mut self, str_store: &mut StrStore) -> Token {
        self.is_at_eos = false;
        let token = self.next;

        let next = self.lex_token(str_store);
        self.next = next;

        if self.can_end_statement(token.ty) {
            self.is_at_eos = true;
        }

        token
    }

    fn set_pos(&self) -> Pos {
        let pos = self.pos + self.base;
        Pos(pos as u32)
    }

    fn lex_token(&mut self, str_store: &mut StrStore) -> Token {
        // skip whitespace and comments
        self.skip();

        let ch = match self.next_char() {
            Some(ch) => ch,
            None => return Token::new_eof(str_store, self.set_pos()),
        };

        if self.insert_semicolon(ch) {
            return Token::new_semicolon(str_store, self.set_pos());
        }

        match ch {
            'a'..='z' => self.lex_ident(str_store, ch),
            'A'..='Z' => self.lex_ident(str_store, ch),
            '0'..='9' => self.lex_number(str_store, ch),
            '.' => self.lex_range(str_store, ch),
            '(' => Token::new_open_paren(str_store, self.bump(ch)),
            ')' => Token::new_close_paren(str_store, self.bump(ch)),
            '{' => Token::new_open_brace(str_store, self.bump(ch)),
            '}' => Token::new_close_brace(str_store, self.bump(ch)),
            '+' => Token::new_plus(str_store, self.bump(ch)),
            '-' => Token::new_minus(str_store, self.bump(ch)),
            '*' => Token::new_multiply(str_store, self.bump(ch)),
            '/' => Token::new_divide(str_store, self.bump(ch)),
            '<' => Token::new_less_than(str_store, self.bump(ch)),
            ';' => Token::new_semicolon(str_store, self.bump(ch)),
            ',' => Token::new_comma(str_store, self.bump(ch)),
            '=' => match self.nth_char(1) {
                Some('=') => {
                    self.bump('=');
                    self.bump('=');
                    Token::new_equal_equal(str_store, self.set_pos())
                }
                _ => Token::new_equal(str_store, self.bump(ch)),
            },
            ':' => match self.nth_char(1) {
                Some(':') => {
                    self.bump(':');
                    self.bump(':');
                    Token::new_module(str_store, self.set_pos())
                }
                _ => self.lex_unknown(str_store, ch),
            },
            _ => self.lex_unknown(str_store, ch),
        }
    }

    fn lex_unknown(&mut self, str_store: &mut StrStore, ch: char) -> Token {
        let start = self.pos;
        self.bump(ch);

        while let Some(ch) = self.next_char() {
            // lex untill we find something that could resonably start a new token
            if ch.is_whitespace() {
                break;
            }

            if matches!(
                ch,
                '(' | ')' | '{' | '}' | '+' | '-' | '*' | '/' | '=' | '<' | ';' | '0'..='9' | 'a'..='z' | 'A'..='Z'
            ) {
                break;
            }

            self.bump(ch);
        }

        let s = &self.source[start..self.pos];
        let lexeme = str_store.get_id(s);

        Token {
            ty: Ty::Unknown,
            pos: Pos::from(start),
            lexeme,
        }
    }

    /// lexes Manta indentifiers which must start with an alpha or underscore character
    fn lex_ident(&mut self, str_store: &mut StrStore, ch: char) -> Token {
        let start = self.pos;
        self.bump(ch);

        while let Some(ch) = self.next_char() {
            if !ch.is_alphanumeric() && ch != '_' {
                break;
            }

            self.bump(ch);
        }

        let s = &self.source[start..self.pos];
        self.ident_to_keyword(Pos::from(start), str_store, s)
    }

    /// matches indentifers into keywords since all Manta keywords are valid identifier names
    fn ident_to_keyword(&self, pos: Pos, str_store: &mut StrStore, s: &str) -> Token {
        let ty = match s {
            "true" => Ty::TrueLiteral,
            "false" => Ty::FalseLiteral,
            "fn" => Ty::FnKeyword,
            "if" => Ty::IfKeyword,
            "in" => Ty::InKeyword,
            "as" => Ty::AsKeyword,
            "return" => Ty::ReturnKeyword,
            "else" => Ty::ElseKeyword,
            "while" => Ty::WhileKeyword,
            "for" => Ty::ForKeyword,
            "loop" => Ty::LoopKeyword,
            "break" => Ty::BreakKeyword,
            "continue" => Ty::ContinueKey,
            "defer" => Ty::DeferKeyword,
            "struct" => Ty::StructKeyword,
            "enum" => Ty::EnumKeyword,
            "switch" => Ty::SwitchKeyword,
            "match" => Ty::MatchKeyword,
            "let" => Ty::LetKeyword,
            "const" => Ty::ConstKeyword,
            "type" => Ty::TypeKeyword,
            "pub" => Ty::PubKeyword,
            "mod" => Ty::ModKeyword,
            "use" => Ty::UseKeyword,
            "mut" => Ty::MutKeyword,
            "or" => Ty::MutKeyword,
            "wrap" => Ty::WrapKeyword,
            _ => Ty::Identifier,
        };

        let lexeme = str_store.get_id(s);
        Token { ty, pos, lexeme }
    }

    fn lex_number(&mut self, str_store: &mut StrStore, ch: char) -> Token {
        let start = self.pos;
        self.bump(ch);

        let mut ty = Ty::Int;

        while let Some(ch) = self.next_char() {
            if ch == '.' && ty == Ty::Int {
                // transition to float and consume the '.'
                ty = Ty::Float;
                self.bump(ch);
                continue;
            }

            if ch == '.' && ty == Ty::Float {
                // second '.', stop and let the lexer handle it as a range or unknown
                break;
            }

            if !ch.is_numeric() && ch != '_' {
                break;
            }

            self.bump(ch);
        }

        let s = &self.source[start..self.pos];
        let lexeme = str_store.get_id(s);

        Token {
            ty,
            pos: Pos::from(start),
            lexeme,
        }
    }

    /// lexes a dot character into either one of the range tokens or into a single dot token or one
    /// of the range expressions (either inclusive or exclusive)
    fn lex_range(&mut self, str_store: &mut StrStore, ch: char) -> Token {
        let start = self.pos;
        self.bump(ch);

        let next = match self.next_char() {
            Some(ch) => ch,
            None => {
                // this is a trailing '.' at the end of the source
                return Token {
                    ty: Ty::Dot,
                    pos: Pos::from(start),
                    lexeme: str_store.get_id("."),
                };
            }
        };

        if next != '.' {
            // this is just a stand alone dot character, not a double dot
            return Token {
                ty: Ty::Dot,
                pos: Pos::from(start),
                lexeme: str_store.get_id("."),
            };
        };

        self.bump(ch);
        let next = match self.next_char() {
            Some(ch) => ch,
            None => {
                // this is a trailing '..' at the end of the source
                return Token {
                    ty: Ty::DotDot,
                    pos: Pos::from(start),
                    lexeme: str_store.get_id(".."),
                };
            }
        };

        return match next {
            '<' => {
                self.bump('<');
                Token {
                    ty: Ty::RangeExclusive,
                    pos: Pos::from(start),
                    lexeme: str_store.get_id("..<"),
                }
            }
            '=' => {
                self.bump('=');
                Token {
                    ty: Ty::RangeInclusive,
                    pos: Pos::from(start),
                    lexeme: str_store.get_id("..="),
                }
            }
            _ => Token {
                ty: Ty::DotDot,
                pos: Pos::from(start),
                lexeme: str_store.get_id(".."),
            },
        };
    }

    fn insert_semicolon(&mut self, ch: char) -> bool {
        if !self.is_at_eos {
            return false;
        }

        match ch {
            '\n' => {
                self.bump(ch);
                return true;
            }
            '}' => return true,
            _ => return false,
        }
    }

    fn skip(&mut self) {
        loop {
            // check if we need to skip a comment first
            if self.source[self.pos..].starts_with("//") {
                self.skip_comment();
            }

            match self.next_char() {
                Some(ch) => {
                    if ch == '\n' && self.is_at_eos {
                        break;
                    }

                    if ch.is_whitespace() {
                        self.bump(ch);
                        continue;
                    }
                }
                None => break,
            }

            break;
        }
    }

    fn skip_comment(&mut self) {
        loop {
            match self.next_char() {
                Some(ch) => {
                    if ch == '\n' {
                        break;
                    }

                    self.bump(ch);
                }
                None => break,
            }
        }
    }

    fn bump(&mut self, ch: char) -> Pos {
        let adv = ch.len_utf8();
        self.pos += adv;
        self.set_pos()
    }

    fn next_char(&self) -> Option<char> {
        self.source[self.pos..].chars().next()
    }

    fn nth_char(&self, n: usize) -> Option<char> {
        self.source[self.pos..].chars().nth(n)
    }

    fn can_end_statement(&mut self, ty: Ty) -> bool {
        matches!(
            ty,
            Ty::Identifier
                | Ty::Int
                | Ty::Float
                | Ty::TrueLiteral
                | Ty::FalseLiteral
                | Ty::BreakKeyword
                | Ty::CloseBrace
                | Ty::CloseParen
        )
    }
}

#[cfg(test)]
mod tests {
    use std::{env, fs};

    use super::*;
    use similar::{ChangeTag, TextDiff};

    /// reads a source file from "test/src" and lexes it into a json string
    fn lex_source(name: &str) -> String {
        let path = format!("tests/src/{}", name);
        let source = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => panic!("failed to read source file to lex {}: {}", &path, e),
        };

        let mut str_store = StrStore::new();
        let mut lexer = Lexer::new(&mut str_store, &source, 0);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next(&mut str_store);
            tokens.push(token);

            if token.ty == Ty::Eof {
                break;
            }
        }

        let tokens = match serde_json::to_string_pretty(&tokens) {
            Ok(s) => s,
            Err(e) => panic!("failed to seralize token stream {}", e),
        };

        let rewrite = env::var("REWRITE").is_ok();
        if rewrite {
            // this isn't super robust but given we have a pretty good idea of the shape of the named file
            let name = name.split(".").next().expect("failed to get file name");

            let path = format!("tests/lexer/{}.json", name);
            println!("rewriting golden file {}", &path);
            fs::write(path, &tokens).expect("failed to write updated golden file");
        }

        tokens
    }

    /// reads a golden file from "test/lexer" and returns it as an owned string
    fn read_golden(name: &str) -> String {
        let path = format!("tests/lexer/{}", name);
        match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => panic!("failed to read golden file {}: {}", &path, e),
        }
    }

    /// prints the diff between got and want if any diffs exists, returns the number of lines that differ
    /// between the two strings.
    fn print_diff(got: String, want: String) -> u32 {
        let diff = TextDiff::from_lines(got, want);

        let mut count = 0;
        let mut str_diff = String::new();
        for change in diff.iter_all_changes() {
            let sign = match change.tag() {
                ChangeTag::Delete => "-",
                ChangeTag::Insert => "+",
                ChangeTag::Equal => " ",
            };

            if change.tag() != ChangeTag::Equal {
                count += 1;
            }

            let line = format!("{}{}", sign, change);
            str_diff.push_str(&line);
        }

        if count > 0 {
            println!("Diffs:\n{}", str_diff);
        }

        return count;
    }

    #[test]
    fn test_lexer_defer_free() {
        let got = lex_source("defer_free.manta");
        let want = read_golden("defer_free.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }

    #[test]
    fn test_lexer_enum_polymorphism() {
        let got = lex_source("enum_polymorphism.manta");
        let want = read_golden("enum_polymorphism.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }

    #[test]
    fn test_lexer_if_else() {
        let got = lex_source("if_else.manta");
        let want = read_golden("if_else.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }

    #[test]
    fn test_lexer_let_or() {
        let got = lex_source("let_or.manta");
        let want = read_golden("let_or.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }

    #[test]
    fn test_lexer_loops() {
        let got = lex_source("loops.manta");
        let want = read_golden("loops.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }

    #[test]
    fn test_lexer_missing_module() {
        let got = lex_source("missing_module.manta");
        let want = read_golden("missing_module.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }

    #[test]
    fn test_lexer_multiple_use_sections() {
        let got = lex_source("multiple_use_sections.manta");
        let want = read_golden("multiple_use_sections.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }

    #[test]
    fn text_lexer_none() {
        let got = lex_source("none.manta");
        let want = read_golden("none.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }

    #[test]
    fn text_lexer_option_match() {
        let got = lex_source("option_match.manta");
        let want = read_golden("option_match.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }

    #[test]
    fn text_lexer_pointers() {
        let got = lex_source("pointers.manta");
        let want = read_golden("pointers.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }

    #[test]
    fn text_lexer_simple_add() {
        let got = lex_source("simple_add.manta");
        let want = read_golden("simple_add.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }

    #[test]
    fn text_lexer_structs() {
        let got = lex_source("structs.manta");
        let want = read_golden("structs.json");

        let diffs = print_diff(got, want);
        if diffs > 0 {
            panic!("token stream does not match what was expected")
        }
    }
}
