# Manta token list (draft)

This file lists the token kinds the lexer will produce for the Manta language. Use this
as the canonical source when implementing the lexer.

Tokens (grouped):

1) Keywords (case-sensitive):
- fn, return, if, else, while, for, break, continue, defer, new, free, try, catch, struct, enum, match, let, const, type, import

2) Primitive type tokens (treated as type-keywords in parser):
- i8, i16, i32, i64, isize, u8, u16, u32, u64, usize, f32, f64, bool, char, str
- *i8, *i16, *i32, *i64, *isize, *u8, *u16, *u32, *u64, *usize, *f32, *f64, *bool, *char, *str

3) Operators and punctuation (single or multi-char tokens):
- { } ( ) [ ] , : ; -> := = => | . * & _

4) Literals:
- INTEGER (decimal, allow underscores, e.g. 1_000)
- FLOAT (decimal float, allow underscores, e.g. 1_000.00)
- STRING (double-quoted)
- CHAR (single-quoted)
- NIL (the literal `nil`) — can be produced as a separate token or as IDENT("nil"); using a separate token simplifies parsing
- TRUE / FALSE (booleans `true` and `false`)

5) Identifiers:
- IDENT (pattern: [A-Za-z_][A-Za-z0-9_]*)

6) Comments and whitespace:
- LINE_COMMENT: // until end-of-line
- BLOCK_COMMENT: /* ... */
- WHITESPACE: spaces, tabs, newlines (skipped)

7) Special tokens for destructuring syntax
- VARIANT_DOT: '.' used before enum variant names in constructors/patterns

Notes and choices:
- `:=` vs `=`: The grammar allows both `:=` (short-decl/infer) and `=` (assignment/explicit). The lexer should emit a distinct token for `:=`.
- `nil` as a dedicated `NIL` token simplifies recognition and ties directly to reference semantics.
- Multi-character tokens like `==` (logical equal check) and `:=` (short assignment) should be recognized before single-char tokens.

Example token stream for `let p *i32 = nil`:
- LET TYPE_KEYWORD('*i32') '=' NIL

Next steps:
- Implement lexer that emits these tokens and write unit tests for tokenizing example inputs.
- Expand tokens if new operators or syntax are added (e.g. pattern guards `if` in match arms).
