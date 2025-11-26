# Manta Parser Implementation Plan

## Overview

This document outlines a comprehensive, phased approach to implementing the Manta parser. The plan prioritizes test-driven development and gradual feature addition, starting with simple expressions and expanding to more complex language constructs.

### Key Principles

1. **Test-Driven Development**: Every phase includes unit tests and integration tests
2. **Gradual Feature Addition**: Simple features first, with each phase building on previous work
3. **Pratt Parser Architecture**: Use a Pratt (precedence climbing) parser for expression parsing, inspired by the `bantam-rust` reference implementation
4. **Integration Testing**: Leverage existing test infrastructure (similar to lexer tests) by parsing `.manta` source files and validating AST output

---

## Architecture Overview

### High-Level Design

```
┌─────────────────────────────────────────────────────────┐
│                   Manta Source File                     │
└──────────────────────────┬──────────────────────────────┘
                           │
                           ▼
                    ┌─────────────┐
                    │   Lexer     │
                    │ (Tokenizer) │
                    └──────┬──────┘
                           │
                           ▼
                     ┌─────────────┐
                     │  TokenStream│
                     └──────┬──────┘
                            │
                            ▼
                   ┌────────────────────┐
                   │      Parser        │
                   │  ┌──────────────┐  │
                   │  │ Pratt Engine │  │
                   │  └──────────────┘  │
                   │  ┌──────────────┐  │
                   │  │ Parselets    │  │
                   │  │ (Prefix/Infix)  │
                   │  └──────────────┘  │
                   └────────┬───────────┘
                            │
                            ▼
                         ┌──────────┐
                         │   AST    │
                         └──────────┘
```

### Core Components

1. **Parser Core** (`src/parser.rs` - main entry point)
   - Manages the overall parsing flow
   - Handles token consumption and lookahead
   - Coordinates between parselets

2. **Pratt Expression Parser** (`src/parser.rs` - method on Parser type)
   - Implements precedence climbing algorithm
   - Manages prefix and infix parselets
   - Handles precedence levels

3. **Parselets** (`src/parser/parselets/`)
   - Prefix parselets: Handle prefix operators, literals, identifiers
   - Infix parselets: Handle binary operators, function calls, field access
   - Each parselet is responsible for parsing one token type

4. **Statement Parser** (`src/parser.rs` - method on Parser type)
   - Parses top-level declarations
   - Parses statement types (let, assign, return, defer, match, etc.)

5. **Type Parser** (`src/parser.rs` - method on Parser type)
   - Parses type specifications
   - Handles type specifiers (basic, pointer, array, slice, named)

---

## Implementation Phases

### Phase 1: Core Infrastructure & Setup [DONE]
**Goal**: Establish basic parser structure and Pratt parser framework

#### Tasks

1. **Set up parser module structure**
   - [x] Already have `src/parser.rs` stub
   - Create `src/parser/` subdirectory structure:
     - `src/parser/parselets.rs` - Parselet traits and registry
     - `src/parser/types.rs` - Type parsing
     - `src/parser/parselets/` - Parselet implementations (one file per parselet)

2. **Define Parselet Traits**
   - Create `PrefixParselet` trait:
     ```rust
     pub trait PrefixParselet {
         fn parse(&self, parser: &mut Parser, token: Token) -> Expr;
     }
     ```
   - Create `InfixParselet` trait:
     ```rust
     pub trait InfixParselet {
         fn parse(&self, parser: &mut Parser, left: Expr, token: Token) -> Expr;
         fn precedence(&self) -> i32;
     }
     ```

3. **Implement Parser Core Structure**
   - Token stream management with lookahead
   - `consume()` and `look_ahead()` methods
   - Parselet registration registry
   - Error handling framework

4. **Create Precedence Levels** (`src/parser/precedence.rs`)
   - Based on Manta language operators
   - Reference levels from bantam-rust:
     - ASSIGNMENT (1)
     - LOGICAL_OR (2)
     - LOGICAL_AND (3)
     - EQUALITY (4)
     - COMPARISON (5)
     - ADDITIVE (6)
     - MULTIPLICATIVE (7)
     - EXPONENTIATION (8)
     - PREFIX/POSTFIX (9)
     - CALL/INDEX/FIELD (10)

#### Testing Strategy

- Unit tests for:
  - Token consumption and lookahead
  - Parselet registration
  - Precedence queries

---

### Phase 2: Simple Literals & Identifiers [DONE]
**Goal**: Parse basic expressions (literals and identifiers)

#### Features to Add

1. **Integer Literals**
   - Parselet: `IntLiteralParselet`
   - AST node: `Expr::IntLiteral(i64)`

2. **Float Literals**
   - Parselet: `FloatLiteralParselet`
   - AST node: `Expr::FloatLiteral(f64)`

3. **String Literals**
   - Parselet: `StringLiteralParselet`
   - AST node: `Expr::StringLiteral(String)`

4. **Boolean & Nil Literals**
   - Parselets: `BoolLiteralParselet`, `NilLiteralParselet`
   - AST nodes: `Expr::BoolLiteral(bool)`, `Expr::NilLiteral`

5. **Identifiers**
   - Parselet: `IdentifierParselet`
   - AST node: `Expr::Identifier(String)`

#### Testing Strategy

- Unit tests for each parselet
- Unit tests using hard coded code snippets for the Parser

#### Acceptance Criteria

- All literal types parse correctly
- Identifiers resolve without errors
- AST output matches expected JSON
- Error handling for malformed literals

---

### Phase 3: Parenthesized Expressions & Grouping [DONE]
**Duration**: ~1 day
**Goal**: Handle expression grouping and nested expressions

#### Features to Add

1. **Grouped Expressions**
   - Parselet: `GroupParselet`
   - Handles: `( expression )`

#### Testing Strategy

- Unit tests for nested parentheses
- Unit tests using hard coded code snippets

---

### Phase 4: Unary Operators [DONE]
**Goal**: Parse unary prefix operators

#### Features to Add

1. **Unary Operators**
   - Operators: `-`, `!`, `*` (dereference), `&` (address-of)
   - Parselets:
     - `UnaryOpParselet` (handles all unary prefix ops)
   - AST node: `Expr::UnaryExpr(UnaryExpr)`

#### Implementation Detail

- Unary operators are **prefix** operations, so they use `PrefixParselet`
- Precedence: Higher than binary operators
- Associativity: Right-associative (e.g., `-!x` should parse as `-(!(x))`)

#### Testing Strategy

- Unit tests for operator parsing
- Unit tests using hard coded code snippets

---

### Phase 5: Binary Arithmetic Operators [DONE]
**Goal**: Parse binary arithmetic expressions with correct precedence

#### Features to Add

1. **Arithmetic Binary Operators**
   - Operators: `+`, `-`, `*`, `/`
   - Parselet:
     - `BinaryOperatorParselet` (single parselet handles all binary arithmetic operations)
   - AST node: `Expr::BinaryExpr(BinaryExpr)`

#### Key Implementation Points

- Single `BinaryOperatorParselet` struct that takes:
  - `operator: BinaryOp` - the specific operator (+, -, *, /)
  - `precedence: Precedence` - precedence level for this operator
- Use operator precedence:
  - Additive (+, -): precedence `Precedence::Addition` (left-associative)
  - Multiplicative (*, /): precedence `Precedence::Multiplication` (left-associative)
- Left-associative: `1 + 2 + 3` → `(1 + 2) + 3`
- Precedence check uses `<=` to ensure correct left-associativity: when the next token has the same or lower precedence, we break from the infix parsing loop

#### Implementation Details

- Register each operator (Plus, Minus, Star, Slash) as an infix token with `BinaryOperatorParselet`
- Each operator gets its appropriate precedence level
- The parser's `parse_expression_precedence` loop continues while `next_precedence > min_precedence` (using `<=` check to break)

#### Testing Strategy

- Unit tests for operator parsing (addition, subtraction, multiplication, division)
- Precedence tests: `1 + 2 * 3` → `(1 + (2 * 3))`
- Left-associativity tests: `1 + 2 + 3` → `((1 + 2) + 3)`
- Complex expression tests to verify precedence chains

---

### Phase 6: Comparison & Logical Operators
**Goal**: Parse comparison and logical operations with correct precedence

#### Features to Add

1. **Comparison Operators**
   - Operators: `==`, `!=`, `<`, `>`, `<=`, `>=`
   - Parselet: `ComparisonOpParselet`
   - Precedence: 5

2. **Logical Operators**
   - Operators: `&&`, `||`
   - Parselets:
     - `LogicalAndParselet` (precedence 3)
     - `LogicalOrParselet` (precedence 2)

3. **Bitwise Operators** (optional for Phase 6, can defer)
   - Operators: `&`, `|`, may include in later phase

#### Testing Strategy

- Unit tests for precedence and short-circuit evaluation (conceptual)
- Integration test: `tests/parser/logic_comparison.manta`
  ```manta
  fn test_logic() {
      let a bool = true && false
      let b bool = true || false
      let c bool = 1 < 2
      let d bool = 3 == 3
      let e bool = true && 1 < 2 || false  // precedence test
  }
  ```

---

### Phase 7: Function Calls
**Goal**: Parse function call expressions

#### Features to Add

1. **Function Call Expression**
   - Syntax: `identifier ( argument_list? )`
   - Parselet: `CallParselet` (infix)
   - AST node: `Expr::Call(FunctionCall)`
   - Precedence: 10 (highest for postfix operations)

#### Implementation Detail

- Calls are **infix** operations (they follow an identifier/expression)
- Arguments separated by commas
- `identifier()` for no arguments

#### Testing Strategy

- Unit tests for argument parsing
- Integration test: `tests/parser/function_calls.manta`
  ```manta
  fn test_calls() {
      let x i32 = add(1, 2)
      let y i32 = add(add(1, 2), 3)
      print()
      print("hello", 42)
  }
  ```

---

### Phase 8: Index & Field Access
**Goal**: Parse indexing and field access operations

#### Features to Add

1. **Index Expression**
   - Syntax: `expression [ expression ]`
   - Parselet: `IndexParselet` (infix)
   - AST node: `Expr::Index { target, index }`
   - Precedence: 10 (same as call)

2. **Field Access Expression**
   - Syntax: `expression . identifier`
   - Parselet: `FieldAccessParselet` (infix)
   - AST node: `Expr::FieldAccess { target, field }`
   - Precedence: 10 (same as call)

#### Testing Strategy

- Unit tests for parsing
- Integration test: `tests/parser/index_field.manta`
  ```manta
  fn test_access() {
      let arr [10]i32 = ...
      let x i32 = arr[0]
      let y i32 = arr[1 + 2]
      
      let obj = ...
      let z i32 = obj.field
  }
  ```

---

### Phase 9: Assignment Expression
**Goal**: Parse assignment operations

#### Features to Add

1. **Assignment Expression**
   - Syntax: `lvalue = expression`
   - Parselet: `AssignmentParselet` (infix)
   - AST node: `Expr::Assignment(Assignment)`
   - Precedence: 1 (lowest)
   - Right-associative: `a = b = c` → `a = (b = c)`

#### Key Points

- Assignment is **right-associative** and **lowest precedence**
- `lvalue` can be:
  - Identifier
  - Dereference: `*ptr`
  - Index: `arr[i]`
  - Field access: `obj.field`

#### Testing Strategy

- Unit tests for lvalue validation
- Integration test: `tests/parser/assignment.manta`
  ```manta
  fn test_assign() {
      let x i32 = 0
      x = 5
      x = 5 + 3
      arr[0] = 10
      obj.field = 42
  }
  ```

---

### Phase 10: New & Free Expressions
**Goal**: Parse memory management operations

#### Features to Add

1. **New Expression** (prefix operator-like)
   - Syntax: `new ( type_spec )`, `new ( [ n ] type_spec )`, `new ( [ ] type_spec , len )`, `new ( [ ] type_spec , len , cap )`
   - Parselet: `NewParselet` (prefix)
   - AST node: `Expr::New(NewExpr)`

2. **Free Expression** (prefix operator)
   - Syntax: `free ( expression )`
   - Parselet: `FreeParselet` (prefix)
   - AST node: `Expr::Free(Box<Expr>)`

#### Testing Strategy

- Unit tests for memory operations
- Integration test: `tests/parser/memory.manta`
  ```manta
  fn test_memory() {
      let p *i32 = new(i32)
      let arr [10]i32 = new([10]i32)
      let slice []i32 = new([]i32, 10)
      free(p)
  }
  ```

---

### Phase 11: Try/Catch Expressions
**Goal**: Parse error handling with try/catch

#### Features to Add

1. **Try/Catch Expression**
   - Syntax: `try expression catch ( identifier )? block`
   - Parselet: `TryParselet` (prefix or statement-level)
   - AST node: `Expr::Try(TryExpr)`

#### Implementation Note

- `try` can appear at expression level but has statement-like semantics
- Optional error binding with `catch (e)`

#### Testing Strategy

- Unit tests for catch block parsing
- Integration test: `tests/parser/try_catch.manta` (reference from `examples/try_catch.manta`)
  ```manta
  fn test_try() {
      .Ok(result) := try some_call() catch { return .Error }
      .Ok(result) := try some_call() catch (e) { print(e); return }
  }
  ```

---

### Phase 12: Statements - Let, Return, Defer
**Goal**: Parse statement-level constructs in blocks

#### Features to Add

1. **Let Statement**
   - Syntax: `let identifier ( type_spec | ( type_spec? '=' expression ) )`
   - Examples:
     - `let x i32`
     - `let x i32 = 5`
     - `let x = 5` (type inference)
   - AST node: `Stmt::Let(LetStmt)`

2. **Return Statement**
   - Syntax: `return expression?`
   - AST node: `Stmt::Return(ReturnStmt)`

3. **Defer Statement**
   - Syntax: `defer block`
   - AST node: `Stmt::Defer(DeferStmt)`

4. **Expression Statement**
   - Any expression used as a statement
   - AST node: `Stmt::Expr(ExprStmt)`

#### Testing Strategy

- Unit tests for statement parsing
- Integration test: `tests/parser/statements.manta`
  ```manta
  fn test_statements() {
      let x i32
      let y i32 = 10
      let z = 20
      return
      return 42
      defer { print("cleanup") }
  }
  ```

---

### Phase 13: Statements - Assignment & If/Else
**Goal**: Parse assignment and conditional statements

#### Features to Add

1. **Assignment Statement**
   - Syntax: `identifier = expression`
   - AST node: `Stmt::Assign(AssignStmt)`

2. **If/Else Statement** (extend Stmt enum)
   - Syntax: `if expression block ( else block )?`
   - AST node: `Stmt::If(IfStmt)` with optional `else_block`

#### Testing Strategy

- Unit tests for conditional parsing
- Integration test: `tests/parser/if_else.manta`
  ```manta
  fn test_if() {
      if true {
          print("yes")
      }
      if false {
          print("no")
      } else {
          print("else")
      }
  }
  ```

---

### Phase 14: Statements - Match
**Goal**: Parse pattern matching constructs

#### Features to Add

1. **Match Statement**
   - Syntax: `match expression { match_arm+ }`
   - `match_arm`: `pattern block`
   - AST node: `Stmt::Match(MatchStmt)`, `MatchArm`, `Pattern`

2. **Pattern Types**
   - Enum variant: `Type.Variant`, `Type.Variant(binding)`
   - Literal: `42`, `"string"`, `true`, `nil`
   - Identifier: `x` (binding)
   - Default: `_`

#### Testing Strategy

- Unit tests for pattern parsing
- Integration test: `tests/parser/match.manta` (reference `examples/option_match.manta`)
  ```manta
  fn test_match(x Maybei32) {
      match x {
          .Some(val) { print(val) }
          .None { print("none") }
      }
  }
  ```

---

### Phase 15: Type Parsing
**Goal**: Parse type specifications

#### Features to Add

1. **Basic Types**
   - Keywords: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `bool`, `str`, `void`
   - AST: `TypeSpec::Int32`, etc.

2. **Pointer Types**
   - Syntax: `* type_spec`
   - AST: `TypeSpec::Pointer(Box<TypeSpec>)`

3. **Array Types**
   - Syntax: `[ INTEGER ] type_spec`
   - AST: `TypeSpec::Array(Box<TypeSpec>, usize)`

4. **Slice Types**
   - Syntax: `[] type_spec`
   - AST: `TypeSpec::Slice(Box<TypeSpec>)`

5. **Named Types**
   - Identifiers referring to user-defined types
   - AST: `TypeSpec::Named(String)`

#### Implementation Detail

- Type parsing is used in:
  - Function parameters
  - Return types
  - Let statements
  - Type declarations
  - Generic contexts (future)

#### Testing Strategy

- Unit tests for type parsing
- Integration test: `tests/parser/types.manta`
  ```manta
  fn test_types(
      a i32,
      b *i32,
      c [10]i32,
      d []i32,
      e Maybei32
  ) *i32 {
      return a
  }
  ```

---

### Phase 16: Top-Level Declarations - Functions
**Goal**: Parse function declarations (the main building blocks)

#### Features to Add

1. **Function Declaration**
   - Syntax: `fn identifier ( param_list? ) return_type? block`
   - `param_list`: `param ( ',' param )*`
   - `param`: `identifier ( type_spec )?`
   - AST node: `TopLevelDecl::Function(FunctionDecl)`

#### Key Implementation Points

- Function body is a `Block` (sequence of statements)
- Each statement in body must be parseable
- Parameters can have optional type annotations

#### Testing Strategy

- Unit tests for parameter and return type parsing
- Integration test: `tests/parser/functions.manta`
  ```manta
  fn add(a, b i32) i32 {
      return a + b
  }

  fn print_hello() {
      print("hello")
  }

  fn no_return(x i32) {
      let y i32 = x + 1
  }
  ```

---

### Phase 17: Top-Level Declarations - Types (Struct/Enum)
**Goal**: Parse type declarations

#### Features to Add

1. **Struct Type Declaration**
   - Syntax: `type identifier 'struct' '{' field_decl* '}'`
   - `field_decl`: `identifier type_spec`
   - AST node: `TopLevelDecl::Type(TypeDecl)` with `TypeKind::Struct`

2. **Enum Type Declaration**
   - Syntax: `type identifier 'enum' '{' enum_variant+ '}'`
   - `enum_variant`: `identifier ( '(' type_spec ')' )?`
   - AST node: `TopLevelDecl::Type(TypeDecl)` with `TypeKind::Enum`

#### Testing Strategy

- Unit tests for struct/enum parsing
- Integration test: `tests/parser/type_decl.manta`
  ```manta
  type Point struct {
      x i32
      y i32
  }

  type Result enum {
      Ok(i32)
      Error
  }
  ```

---

### Phase 18: Top-Level Declarations - Const & Import
**Goal**: Parse const and import declarations

#### Features to Add

1. **Const Declaration**
   - Syntax: `const identifier = expression`
   - AST node: `TopLevelDecl::Const(ConstDecl)`

2. **Import Declaration**
   - Syntax: `import ( STRING | import_block )`
   - `import_block`: `'(' STRING+ ')'`
   - AST node: `TopLevelDecl::Import(ImportDecl)`

#### Testing Strategy

- Unit tests
- Integration test: `tests/parser/top_level.manta`
  ```manta
  import "std"
  import ("io", "math")

  const PI = 3.14159
  const MAX i32 = 100
  ```

---

### Phase 19: Program & Full Integration
**Goal**: Parse complete programs and validate end-to-end

#### Features to Add

1. **Program Parser**
   - Syntax: `{ top_level_decl }`
   - Entry point: `parse_program(tokens) -> Vec<TopLevelDecl>`

2. **Error Handling & Recovery**
   - Proper error messages with span information
   - Parse error types: `ParseError`

#### Testing Strategy

- **Integration tests**: Test complete example files
  - `tests/parser/defer_free.manta` ← reference: `examples/defer_free.manta`
  - `tests/parser/nil_refs.manta` ← reference: `examples/nil_refs.manta`
  - `tests/parser/option_match.manta` ← reference: `examples/option_match.manta`
  - `tests/parser/try_catch.manta` ← reference: `examples/try_catch.manta`

- Each test file should have a corresponding `.json` file with expected AST output

#### Acceptance Criteria

- All example files parse without errors
- AST output matches expected JSON
- Error messages are clear and helpful
- No panics on invalid input (graceful error handling)

---

## Testing Framework

### Unit Testing Strategy

Each phase should include:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_literal() {
        let tokens = vec![Token::IntLiteral(42), Token::Eof];
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expression().unwrap();
        assert_eq!(expr, Expr::IntLiteral(42));
    }

    #[test]
    fn test_parse_binary_precedence() {
        // 1 + 2 * 3 should parse as (1 + (2 * 3))
        let tokens = vec![
            Token::Int(1), Token::Plus, Token::Int(2),
            Token::Star, Token::Int(3), Token::Eof
        ];
        let expr = parser.parse_expression().unwrap();
        // Assert structure
    }
}
```

### Integration Testing Strategy

1. **Test Files**: Place `.manta` source files in `tests/parser/`
2. **Expected Output**: Place `.json` AST files in `tests/parser/`
3. **Test Harness**: Create a test utility that:
   - Lexes `.manta` file
   - Parses into AST
   - Serializes AST to JSON
   - Compares with expected output

Example test harness:

```rust
#[test]
fn test_parse_defer_free() {
    let source = std::fs::read_to_string("tests/parser/defer_free.manta").unwrap();
    let lexer = Lexer::new(&source);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program().unwrap();
    
    let output_json = serde_json::to_string(&program).unwrap();
    let expected_json = std::fs::read_to_string("tests/parser/defer_free.json").unwrap();
    
    assert_eq!(output_json, expected_json);
}
```

### Test File Locations

```
tests/
  parser/
    defer_free.manta
    defer_free.json
    nil_refs.manta
    nil_refs.json
    option_match.manta
    option_match.json
    try_catch.manta
    try_catch.json
    simple_literals.manta
    simple_literals.json
    ... (one pair per phase)
```

---

## Error Handling Strategy

### Error Types

Define `ParseError` enum:

```rust
#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        found: TokenKind,
        span: Span,
    },
    UnexpectedEof {
        expected: String,
    },
    InvalidLValue {
        span: Span,
    },
    InvalidPattern {
        span: Span,
    },
    CustomError(String, Span),
}
```

### Recovery Strategy

- For each phase, identify critical points where recovery is important
- Panic on truly unrecoverable errors; use `Result<T, ParseError>` for expected failures
- Provide helpful error messages with source spans

---

## Implementation Tips & Patterns

### 1. Parselet Registration Pattern

```rust
// In Parser::new or initialization
self.register_prefix(TokenKind::Int, Box::new(IntLiteralParselet));
self.register_prefix(TokenKind::Ident, Box::new(IdentifierParselet));
self.register_infix(TokenKind::Plus, Box::new(AdditiveOpParselet));
```

### 2. Lookahead Pattern

```rust
pub fn look_ahead(&mut self, distance: usize) -> &Token {
    while distance >= self.read.len() {
        self.read.push(self.lexer.next_token().unwrap());
    }
    &self.read[distance]
}

pub fn consume(&mut self) -> Token {
    self.look_ahead(0);
    self.read.remove(0)
}
```

### 3. Match Token Pattern

```rust
pub fn match_token(&mut self, kinds: &[TokenKind]) -> bool {
    for &kind in kinds {
        if self.look_ahead(0).kind == kind {
            self.consume();
            return true;
        }
    }
    false
}
```

### 4. Expression Parsing Loop (Pratt Algorithm)

```rust
pub fn parse_expression(&mut self, min_precedence: i32) -> Result<Expr, ParseError> {
    let token = self.consume();
    let mut left = self.parse_prefix(&token)?;

    while min_precedence < self.current_precedence() {
        let token = self.consume();
        left = self.parse_infix(left, &token)?;
    }

    Ok(left)
}
```

---

## Reference Files

- **Bantam Parser**: `/Users/alexis/Projects/rust/bantam-rust/src/bantam/`
- **Manta Examples**: `/Users/alexis/Projects/rust/manta/examples/`
- **AST Spec**: `/Users/alexis/Projects/rust/manta/docs/ast_spec.md`
- **Language Spec**: `/Users/alexis/Projects/rust/manta/docs/language_spec.md`
- **Grammar**: `/Users/alexis/Projects/rust/manta/docs/grammar.ebnf`

---

## Notes for Implementation

1. **Serialize AST to JSON**: Ensure the AST structures derive `serde::Serialize` and `serde::Deserialize` for test output generation.

2. **Token Stream**: The lexer produces tokens; the parser consumes from a token stream. Consider whether to buffer all tokens upfront or stream them.

3. **Error Recovery**: Early phases can panic on errors. Later phases should implement graceful error recovery.

4. **AST Validation**: Later phases (18+) should consider semantic validation (e.g., ensuring variables are bound before use). This could be a separate semantic analysis phase.

5. **Documentation**: Document each parselet with examples of what it parses.

6. **Performance**: Pratt parser is efficient for expression parsing. For statement parsing, a simple recursive descent approach is sufficient.

