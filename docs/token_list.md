# Manta token list (draft)

This file lists the token kinds the lexer will produce for the Manta language. Use this
as the canonical source when implementing the lexer.

Tokens (grouped):

1) Keywords (case-sensitive):
- fn, return, loop, for, break, continue, defer, alloc, free, let, mut, var, const, struct, enum, match, type, use

2) Primitive type tokens (treated as identifiers in the parser but useful to call out):
- i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, bool, rune, str

3) Operators and punctuation (single or multi-char tokens):
- { } ( ) [ ] , : ; = > < > | . * + - / % & _
- := == >= <= || && //

4) Literals:
- INTEGER (decimal, allow underscores, e.g. 1_000)
- FLOAT (decimal float, allow underscores, e.g. 1_000.00)
- STRING (double-quoted)
- RUNE (single-quoted)
- TRUE / FALSE (booleans `true` and `false`)

5) Identifiers:
- IDENT (pattern: [A-Za-z_][A-Za-z0-9_]*)

6) Comments and whitespace:
- LINE_COMMENT: // until end-of-line
- BLOCK_COMMENT: /* ... */
- WHITESPACE: spaces, tabs, newlines (skipped)

Example token stream for `let answer = 42`:
- LET IDENT('answer') EQUAL INTEGER(42)

