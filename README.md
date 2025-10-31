# Manta — systems language compiler (work-in-progress)

Manta is a small, systems-style programming language and compiler project implemented in Rust.
This README is a living document and reflects the compiler's current goals and how to build and run the repository locally.

## Project goals

- Minimal, explicit systems-language with manual memory management (no GC).
- Familiar surface syntax for Rust/C/Zig/Go users: `fn`, `let`, `defer`, `new`/`free`, `match`, `try`/`catch`.
- Small, practical grammar designed to make implementing a lexer, parser, and simple codegen straightforward.

See `docs/language_spec.md` for the detailed language design notes and rationale.

## Building

This project is a Rust crate. To build the compiler locally you need a Rust toolchain (rustc + cargo). On macOS using Homebrew or rustup is recommended.

Build the project:

```bash
cargo build --release
```

Or for a fast debug build:

```bash
cargo build
```

Currently the compiler front-end is an early prototype; `src/main.rs` contains a placeholder entrypoint.

## Running / examples

There are example Manta programs in the `examples/` folder that demonstrate core features:

- `examples/defer_free.manta` — shows `defer`, `new`, `free`, and error handling via `try`/`catch`.
- `examples/nil_refs.manta` — shows `nil` references, conditional allocation, and manual `free`.
- `examples/option_match.manta` — demonstrates `enum` (sum) types and `match` pattern matching.
- `examples/try_catch.manta` — demonstrates `try`/`catch` for error propagation and `defer` usage.

The compiler currently doesn’t yet implement a full CLI to compile and run Manta programs. When a CLI is available the README will be updated with usage examples such as:

```bash
# Build the compiler
cargo build --release

# Compile a manta source file (example placeholder)
./target/release/mantac examples/try_catch.manta -o out.bin

# (or) run with an interpreter backend:
./target/release/mantac run examples/try_catch.manta
```

Note: the above commands are illustrative; please check `src/main.rs` and the CLI help for the current, accurate flags.

## Language spec references

Primary design documents are in `docs/`. Key files to read:

- `docs/language_spec.md` — high-level language features, examples, and semantics.
- `docs/grammar.ebnf` — starting grammar for the parser (EBNF draft).
- `docs/token_list.md` — the token kinds the lexer should emit.

These documents were used to author the example programs in `examples/` and should be considered the source-of-truth while the language evolves.

## Next steps & roadmap

- Implement lexer and unit tests for tokenization.
- Implement parser using the EBNF grammar and add AST unit tests.
- Design a simple IR and a minimal codegen or interpreter for fast iteration.
- Add a `manta` CLI that can `build`, `run`, and `test` example programs.

## Command-line interface

The main way to interact with Manta is uing the Manta CLI.
The CLI executable is named `manta` and supports the following subcommands and flags:

### Build
`manta build` Compiles the project and produce build artifacts.
The following flags are supported:

- `-o, --out-dir <OUT_DIR>` : optional output directory for build artifacts

### Check
`manta check` Runs checks on the project and report results without producing build artifacts.

### Run
`manta run` Builds and executes the program immediately.

### Fmt
`manta fmt` Formats source files using the official manta formatter.
The following falgs are supported:

- `-w, --write` : write changes to files in-place (otherwise prints diffs)
- positional `FILES...` : files or directories to format

Global flags:

- `-v` / `--verbose` : increase verbosity (stackable; `-vv` for more verbose output)
- `-p, --path <PATH>` : path to the project or file to operate on (defaults to current directory)

Examples (using the built binary in `target/release`):

```bash
# Build the current project (stub)
./target/release/manta build -o out/ 

# Check project in strict mode
./target/release/manta check

# Run the project, passing program arguments
./target/release/manta run

# Format manta source files in-place
./target/release/manta fmt -w src/
```
