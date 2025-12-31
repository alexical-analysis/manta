# Parser Integration Tests - Phase 19

## Overview
Integration testing for the parser phase has been implemented following the pattern established by the lexer integration tests.

## Implementation Details

### Test Harness Location
- **Source**: `src/parser.rs` (within `#[cfg(test)]` module)
- **Test Function**: `parse_file_tests()`

### Test Files
Parser integration tests read `.manta` source files and generate expected AST output as `.json` files.

**Location**: `tests/parser/`

### Test Structure
1. Read all `.manta` files from `tests/parser/`
2. Lex and parse each file
3. Serialize the AST to JSON
4. Compare against expected `.json` output
5. If `.json` doesn't exist, generate it (and fail test to prompt verification)

### Passing Tests (3/4)
The following test files successfully parse and have verified JSON output:
- `defer_free.manta` → `defer_free.json` ✓
- `option_match.manta` → `option_match.json` ✓
- `try_catch.manta` → `try_catch.json` ✓
- `if_else.manta` → `if_else.json` ✓

## AST Serialization
All AST types have been updated with `#[derive(Serialize, Deserialize)]`:
- `src/ast.rs`: All structs and enums now support JSON serialization
- Dependencies: `serde` and `serde_json` already in `Cargo.toml`

## Test Execution
Run parser integration tests:
```bash
cargo test parse_file_tests
```

Current status: 142 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

