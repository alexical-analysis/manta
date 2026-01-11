# Lexer Update Plan

## Overview

This document outlines the plan to update the Manta lexer to support the new grammar features introduced in the updated language specification. The lexer must be updated to recognize new keywords, operators, and tokens that are now part of the language.

## Current State

The existing lexer (`src/parser/lexer.rs`) supports:
- Basic keywords: `fn`, `return`, `try`, `if`, `else`, `while`, `for`, `break`, `continue`, `defer`, `catch`, `struct`, `enum`, `match`, `let`, `const`, `type`, `import`
- Standard operators and punctuation
- Identifiers, integers, floats, strings
- Comments (single-line `//` and block `/* */`)

## Grammar Changes Requiring Lexer Updates

### 1. New Keywords
- `mod` - module declaration
- `use` - module imports (replaces `import`)
- `mut` - mutable variable binding
- `var` - module-level mutable variable
- `or` - error handler in `except` clause
- `wrap` - error wrapper in `except` clause

### 2. Keywords to Remove/Deprecate
- `import` - replaced by `use`
- `try` - replaced by `except` clause
- `catch` - no longer used as standalone (part of old error handling)

### 3. New Operators and Punctuation
- `::` - module path separator (double colon)
- `!` - panic/unwrap operator (already exists but needs context-aware handling)

### 4. Token Adjustments
- Clarify `!` usage in different contexts (unary NOT operator vs. panic operator)
- Ensure `::` is treated as a single token, not two separate colons

## Implementation Plan

### Phase 1: Add New Keywords
**Goal:** Support recognition of `mod`, `use`, `mut`, `var`, `or`, `wrap`

**Tasks:**
1. Add `ModKeyword`, `UseKeyword`, `MutKeyword`, `VarKeyword`, `OrKeyword`, `WrapKeyword` variants to `TokenKind` enum
2. Update keyword recognition in lexer's `is_keyword()` function to map string literals to token kinds
3. Update keyword matching logic in lexer to recognize these new keywords

**Testing:**
- Unit tests for each keyword token recognition
- Verify keywords are not confused with identifiers
- Test in context of real token streams

### Phase 2: Add Module Path Operator (::)
**Goal:** Recognize `::` as a single token for module-qualified names

**Tasks:**
1. Add `ColonColon` variant to `TokenKind` enum
2. Update lexer scanning logic to detect `::` pattern
3. Ensure `:` followed by `=` still produces `ColonEqual`, not `ColonColon` + `Equal`
4. Adjust existing `:` handling to avoid conflicts

**Testing:**
- Unit tests for `::` token recognition
- Test precedence: `:` vs `::` vs `:=`
- Integration tests with module-qualified identifiers

### Phase 3: Update Deprecated Keywords
**Goal:** Phase out `import`, `try`, `catch` keywords

**Tasks:**
1. Keep `import`, `try`, `catch` in lexer for backward compatibility (mark as deprecated in comments)
2. Add warnings in error handling if deprecated keywords are encountered
3. Document migration path in language spec (already done)

**Testing:**
- Ensure deprecated keywords still tokenize correctly
- Prepare for future removal

### Phase 4: Refine Operator Context Handling
**Goal:** Ensure operators are correctly interpreted in different contexts

**Tasks:**
1. Document `!` usage:
   - Unary NOT operator in expressions: `!x`, `!(a && b)`
   - Panic operator in `except` clauses: `let .Ok(x) = expr !`
2. Document `|` vs `||`:
   - Single pipe `|` for bitwise OR
   - Double pipe `||` for logical OR (already supported)
3. Verify existing lexer correctly distinguishes these cases (context handled by parser, not lexer)

**Testing:**
- Verify lexer produces correct tokens regardless of context
- Parser tests will verify contextual correctness

### Phase 5: Update Token List Documentation
**Goal:** Maintain up-to-date token reference

**Tasks:**
1. Update `docs/token_list.md` with new tokens:
   - Add `ModKeyword`, `UseKeyword`, `MutKeyword`, `VarKeyword`, `OrKeyword`, `WrapKeyword`
   - Add `ColonColon`
   - Document deprecated tokens
2. Add examples showing token streams for new language features
3. Document operator precedence as it relates to lexer output

**Testing:**
- Verify documentation matches implementation

## Implementation Details

### Phase 1: Keyword Changes (Detailed)

Current keyword enum structure (approximate):
```rust
pub enum TokenKind {
    FnKeyword,
    ReturnKeyword,
    // ... existing keywords
    LetKeyword,
    ConstKeyword,
    TypeKeyword,
    ImportKeyword,  // to be deprecated
    // ... other tokens
}
```

After Phase 1, add:
```rust
pub enum TokenKind {
    // ... existing
    ModKeyword,      // NEW
    UseKeyword,      // NEW (replaces ImportKeyword)
    MutKeyword,      // NEW
    VarKeyword,      // NEW
    OrKeyword,       // NEW
    WrapKeyword,     // NEW
    ImportKeyword,   // DEPRECATED
    TryKeyword,      // DEPRECATED
    CatchKeyword,    // DEPRECATED
    // ... rest
}
```

Keyword map (in lexer implementation):
```rust
fn is_keyword(ident: &str) -> Option<TokenKind> {
    match ident {
        "fn" => Some(TokenKind::FnKeyword),
        "return" => Some(TokenKind::ReturnKeyword),
        // ... existing mappings
        "mod" => Some(TokenKind::ModKeyword),     // NEW
        "use" => Some(TokenKind::UseKeyword),     // NEW
        "mut" => Some(TokenKind::MutKeyword),     // NEW
        "var" => Some(TokenKind::VarKeyword),     // NEW
        "or" => Some(TokenKind::OrKeyword),       // NEW
        "wrap" => Some(TokenKind::WrapKeyword),   // NEW
        "import" => Some(TokenKind::ImportKeyword), // DEPRECATED
        // ... rest
        _ => None,
    }
}
```

### Phase 2: Module Path Operator (Detailed)

Current colon handling (approximate):
```rust
':' => {
    if self.peek() == Some('=') {
        self.advance();
        TokenKind::ColonEqual
    } else {
        TokenKind::Colon
    }
}
```

Updated to handle `::`:
```rust
':' => {
    if self.peek() == Some('=') {
        self.advance();
        TokenKind::ColonEqual
    } else if self.peek() == Some(':') {  // NEW
        self.advance();
        TokenKind::ColonColon                 // NEW
    } else {
        TokenKind::Colon
    }
}
```

## Testing Strategy

### Unit Tests (in `src/parser/lexer.rs` or test module)
```rust
#[test]
fn test_module_keyword() {
    let tokens = lex("mod main");
    assert_eq!(tokens[0].kind, TokenKind::ModKeyword);
}

#[test]
fn test_use_keyword() {
    let tokens = lex("use (\"os\")");
    assert_eq!(tokens[0].kind, TokenKind::UseKeyword);
}

#[test]
fn test_mut_keyword() {
    let tokens = lex("mut x = 10");
    assert_eq!(tokens[0].kind, TokenKind::MutKeyword);
}

#[test]
fn test_var_keyword() {
    let tokens = lex("var counter = 0");
    assert_eq!(tokens[0].kind, TokenKind::VarKeyword);
}

#[test]
fn test_colon_colon_operator() {
    let tokens = lex("os::open_file");
    assert_eq!(tokens[0].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].kind, TokenKind::ColonColon);
    assert_eq!(tokens[2].kind, TokenKind::Identifier);
}

#[test]
fn test_or_keyword() {
    let tokens = lex("or(e) { }");
    assert_eq!(tokens[0].kind, TokenKind::OrKeyword);
}

#[test]
fn test_wrap_keyword() {
    let tokens = lex("wrap io::Error");
    assert_eq!(tokens[0].kind, TokenKind::WrapKeyword);
}

#[test]
fn test_colon_vs_colon_colon() {
    // Ensure ':' and '::' are distinct
    let tokens1 = lex("x:i32");
    let tokens2 = lex("os::open");
    
    assert_eq!(tokens1[1].kind, TokenKind::Colon);
    assert_eq!(tokens2[1].kind, TokenKind::ColonColon);
}

#[test]
fn test_colon_equal_vs_colon_colon() {
    // Ensure ':=', ':', and '::' are distinct
    let tokens = lex("x := y");
    assert_eq!(tokens[1].kind, TokenKind::ColonEqual);
}
```

### Integration Tests (in `tests/` directory)
1. Create `.manta` files demonstrating new keywords and operators
2. Run lexer on these files and verify token streams
3. Verify tokens flow correctly into parser

Example test files:
- `tests/lexer_module_decl.manta` - module declaration
- `tests/lexer_use_import.manta` - use statement with module paths
- `tests/lexer_mut_var.manta` - mut and var keywords
- `tests/lexer_except_clause.manta` - or and wrap keywords

## Deliverables

### Code Changes
1. Update `src/parser/lexer.rs`:
   - Add new `TokenKind` variants
   - Update keyword recognition
   - Update operator scanning logic
   
2. Update `docs/token_list.md`:
   - Document new tokens
   - Mark deprecated tokens
   - Provide examples

### Tests
1. Unit tests for each new keyword and operator
2. Integration tests with example `.manta` files
3. Regression tests to ensure existing functionality preserved

## Timeline

- **Phase 1:** Add new keywords (1-2 hours)
- **Phase 2:** Add `::` operator (1-2 hours)
- **Phase 3:** Document deprecated keywords (30 min)
- **Phase 4:** Operator context review (30 min)
- **Phase 5:** Update documentation (1 hour)

**Total estimated time:** 4-6 hours

## Risks and Mitigation

### Risk: Breaking existing code
**Mitigation:** Keep deprecated keywords working; add migration guide in docs

### Risk: Conflicts with operator combinations
**Mitigation:** Careful testing of `:`, `:=`, `::` combinations; ensure lexer correctly disambiguates

### Risk: Incomplete keyword coverage
**Mitigation:** Review grammar thoroughly before implementation; cross-check against `docs/grammar.ebnf`

## Next Steps

1. Review this plan with team
2. Start Phase 1 implementation
3. Run existing tests to ensure no regressions
4. Commit changes with clear messages
5. Proceed to parser updates to support new tokens
