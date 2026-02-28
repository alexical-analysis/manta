# Manta MIR (Middle IR) Design

This document describes the design of Manta's Middle IR (MIR). The MIR is a block-based, control-flow oriented intermediate representation that sits between the existing HIR and the LLVM backend. It is intended to be a stable, analyzable, and easily lowerable representation, with first-class support for definite-initialization checking and clear semantics for local variable binding and exception/`or`-handler control flow.

## Overview

MIR is a function-centric, basic-block based IR. Each function is represented as a control-flow graph (CFG) of basic blocks; each block contains a sequence of non-branching instructions and ends with a single terminator which encodes control flow (jump, branch, switch, return, etc.). Values are represented by value identifiers (temporaries) and named locals (storage slots). The MIR is strongly typed at the value level and is designed to make control- and data-flow analysis straightforward.

Key language-driven requirements that informed this design:
- Support uninitialized local variables (all declarations in Manta are uninitialized by default) and enforce a definite-initialization checker that ensures variables are always initialized before use.
- Ensure that match expressions with control-flow handlers (desugared from syntactic forms like `let x = pat or { ... }`) can be safely analyzed—handlers must not allow control to reach code that expects bindings from a failed pattern match.

## Goals

- Provide a block-based IR that is straightforward to build from HIR and to lower to LLVM.
- Make definite-initialization analysis (and related verification) simple and conservative-correct.
- Provide an explicit representation for `pattern-or` binding semantics so that handler divergence can be reliably validated.
- Keep instruction set small but expressive enough to represent common language constructs (calls, destructuring, control flow, loads/stores, returns, etc.).
- Provide clear invariants and a verifier that catches misuse of uninitialized locals and invalid `or` handlers.

## Assumptions and Constraints

- MIR is per-function; there is no interprocedural MIR linking in this design (lowering to LLVM will generate per-function IR and call sites).
- The HIR will supply types for expressions; MIR preserves type information on values.
- Locals (declared with simple declarations in the HIR) are uninitialized by default and map to storage slots; temporaries are single-assignment value identifiers produced by instructions.
- Match expressions with failure handlers (desugared from syntactic forms like `let x = pat or { ... }`) are represented as explicit control-flow edges in the CFG and are subject to reachability/divergence checks described below.

## High-level Structure

- Program
  - Collection of functions (MIRFunction)
- MIRFunction
  - Signature (params, return type)
  - Locals table (LocalId -> Local metadata: type, storage index, declared?)
  - Entry block id
  - Blocks map (BlockId -> BasicBlock)
- BasicBlock
  - id
  - list of Instructions
  - Terminator (see below)
  - Predecessors (computed) and metadata (e.g. block arguments)
- Values
  - ValueId: refers to a temporary/SSA-like value produced by an instruction (typed)
  - LocalId: refers to a declared storage slot associated with a `let` binding

Notes:
- Locals are not implicitly initialized; `let x = undefined` creates a LocalId with no initialized flag.
- Instructions produce ValueIds and may write to Locals with explicit store/initialize instructions.

## Blocks

A BasicBlock is the unit of control flow. Blocks must:
- Contain zero or more non-terminating instructions.
- End with a single Terminator directing control to successor block(s).
- Optionally accept block arguments (explicit incoming ValueIds) if a value must be passed along an edge without a store/load; these are implementation conveniences for lowering and verification.

Terminator kinds (canonical set):
- Return(value?)
- Jump(target)
- Branch(cond_value, true_target, false_target)
- SwitchVariant(value, {variant_id -> target, ...}) -- routes control based on the enum variant of the value
- Unreachable (for proven-unreachable code)

For pattern-or constructs and constructs that must attach a handler, an instruction will specify an explicit failure-success control split (see Instructions below). That instruction adds the handler block to the block's successor set for reachability and initialization analysis.

## Instructions

Instruction categories and representative forms (not exhaustive):

- Basic value producers:
  - const ty, value -> ValueId
  - unary op, binary op
  - load_local(LocalId) -> ValueId
  - store_local(LocalId, ValueId) -- writes into a local slot (and sets initialization bit)

- Call / Invoke forms:
  - call(func, args...) -> ValueId
    - Regular call which returns to the continuation block on return.
  - call_try(func, args..., handler: BlockId) -> ValueId
    - A form that encodes a call whose failure branch jumps to `handler`. Semantics: on the success path, call returns a value used by continuation; on the failure path the handler block is entered. Lowering: modeled as a call followed by a match-check (language semantics determine whether this is a true exception or a value-based Result-check).

- Pattern matching with failure handlers:
  - switch_variant(src: ValueId, {variant_id -> target_block, ...})
    - A terminator that routes control based on the enum variant discriminant of `src`.
    - Each variant_id has an associated target block that is entered when that variant matches.
    - Default/fallback variants can be represented with a catch-all arm or a separate handler block.
  - variant_get_payload(src: ValueId, variant_id, field_index) -> ValueId
    - Extracts the field at `field_index` from variant `variant_id` of the enum value `src`.
    - This is used in the target blocks after a successful variant match to destructure and bind pattern variables.
    - All pattern matching complexity (deciding which fields to extract) is handled during HIR->MIR lowering; MIR represents only explicit field extractions.

- Move/Copy semantics:
  - move(dst_local, src_value)
  - copy(dst_local, src_value)

- Drop/Finalize local:
  - drop_local(LocalId) -- explicit drop request for destructors

- Misc:
  - declare_local(LocalId) -- explicitly declares a local as uninitialized (semantically implicit for all locals, but explicit instruction for clarity)

Two important design decisions:
1. Locals are uninitialized by default. MIR tracks initialization state via dataflow analysis rather than relying on SSA alone. This makes the definite-initialization checker straightforward and conservative-correct.
2. Pattern matching is decomposed into control-flow (`switch_variant` terminator) and data extraction (`variant_get_payload` instructions). This keeps MIR simple and maps directly to LLVM IR, deferring all pattern complexity to the HIR->MIR lowering stage.

## Definite-initialization analysis (flow-checking)

Purpose: ensure that every use of a local is preceded on *all* possible control-flow paths by a prior initialization.

Algorithm (sketch):
- Define a dataflow lattice where the data for each block is the set of Locals that are *definitely initialized* at block entry.
- The transfer function for a block: start with the entry set; process instructions in order; store_local/set_initialized adds the local to the current set; set_uninitialized removes it (only allowed at function entry / declaration).
- The merge operator at joins is set-intersection (a local is definitely initialized at a join only if it is initialized on every incoming path).
- Iteratively compute a fixed point over the CFG until the in-set for every block stabilizes.
- After analysis, ensure every instruction that uses a local has that local in the current definitely-initialized set; otherwise emit a compile-time error.

Pattern-bind-or semantics in the analysis:
- For the success continuation path (fallthrough after the pattern_bind_or instruction), the transfer function will add the newly-bound locals to the set (these are definitely initialized on the success edge only).
- For the failure handler edge, no new locals are added; the handler is verified separately.

## Enforcing match handler termination relative to bindings

Problem: a match handler that returns control to the continuation would make the pattern-bound locals unavailable on some paths (i.e., the pattern would have failed but control still reaches the same continuation expecting the variables to be bound), which would create an unsound use of an uninitialized variable.

Invariant to enforce: For each `switch_variant` terminator at block B that specifies a handler/failure block H, H must not be able to reach continuation blocks that expect bindings from successful variant matches, unless every path from H to those blocks re-establishes (initializes) the same bindings prior to use.

Practical checks to enforce this invariant:
- Compute reachability from H. If a continuation block expecting specific bindings is reachable, then either:
  - Reject the pattern as invalid (simple conservative rule), OR
  - Verify that all paths from H to that block definitely initialize the required locals before the first use (more complex).

Recommended conservative policy (initial implementation): require that H not reach continuation code expecting specific bindings (i.e., handler must be diverging with respect to those continuations). Diverging includes return-from-function, break/continue to an enclosing construct that ensures the continuation is not reached, panic, or otherwise branching to code that can't reach the continuation.

This check is implemented with a simple reachability traversal from H; if it finds continuation code expecting bindings, it errors with guidance that the handler must not fallthrough into those continuations.

## Building MIR from HIR

- For each function, create a MIRFunction with an entry block.
- Map HIR local declarations:
  - A simple `declare x` in the HIR creates a LocalId that is uninitialized by default; no explicit instruction is needed (it is implicit).
  - `assign x = expr` -> lower expr to MIR and emit store_local(x, tmp) to initialize the local.
- Lower expressions to sequences of MIR instructions that produce ValueIds, use temporaries when necessary.
- Lower match expressions (which in the HIR represent desugared syntactic forms like `let x = pat or { ... }`, `let x = pat wrap { ... }`, etc.) into a `switch_variant` terminator with target blocks for each variant and a failure handler block.
  - The HIR has already desugared all `let/or`, `let/wrap`, and `let/!` syntactic forms into distinct declaration and match instructions. During HIR->MIR lowering, the pattern matching logic (which fields to extract, how to destructure) is analyzed, and the result is a `switch_variant` terminator routing to appropriate blocks.
  - In each target block, emit `variant_get_payload` instructions to extract the matched payload fields and `store_local` instructions to bind them into locals.
  - The handler block is verified separately to ensure it doesn't reach code expecting the bindings to exist.
- After constructing basic blocks, compute predecessors and run the definite-initialization analysis and reachability checks for handlers.

## Lowering to LLVM (mapping notes)

- Blocks -> LLVM basic blocks
- ValueIds -> LLVM values (SSA registers)
- Locals -> LLVM alloca stack slots. Reads/writes become load/store instructions; a store_local becomes a `store` to the alloca, and load_local becomes a `load` from the alloca into an LLVM value.
- SwitchVariant terminators -> LLVM `switch` instructions on the variant discriminant, with one case per variant.
- variant_get_payload instructions -> LLVM `extractvalue` or similar operations to extract struct/aggregate fields.
- Terminators map directly to LLVM terminator instructions (br, ret, switch, unreachable, etc.).
- Type information from MIR is preserved when emitting LLVM IR, ensuring type safety throughout lowering.
- Verification must fully complete before lowering (e.g., handler reachability checks, definite-initialization checks), because lowering cannot magically prevent a handler from falling through.
- MIR can be lowered to LLVM IR using Inkwell (Rust bindings) or direct LLVM-C FFI; implementation flexibility allows for choice of backend based on development needs.

## Example (pseudo-MIR)

Function write_demo(f, buf) -> i32

locals:
  n: i32 (uninitialized at declaration)

blocks:

bb0:
  tmp0 = call(write_file, f, buf)
  switch_variant tmp0 { 0 => bb_success, 1 => bb_err }

bb_success:
  n_val = variant_get_payload(tmp0, 0, 0)  // Extract payload from variant 0, field 0
  store_local(n, n_val)  // n is now initialized
  use_n = load_local(n)
  return use_n

bb_err:
  // must not reach bb_success (conservative rule: handler blocks must not reach code expecting bindings)
  return -1


## Verification & invariants

- Each block must end with a single terminator.
- No instruction may use a local that is not definitely initialized at that program point (definite-init check must pass before lowering).
- For each `switch_variant` terminator with a failure handler block, the handler block must not be able to reach code that expects the bindings from a successful variant match (or must re-initialize bindings on all paths) — conservative rejection otherwise.
- Block arguments/phi-like transfers must be explicit and consistent across predecessor edges.

## Open decisions (implementation scope, not speculative features)

- Representing block arguments vs. explicit stores/loads: prefer explicit store/load lowering from HIR, but allow block arguments as lowering convenience if it simplifies code generation.

## Summary

This MIR is a block-based, typed representation designed to make definite-initialization checking and pattern-handler validation explicit and analyzable prior to lowering to LLVM. Key invariants (definite-init and handler non-reachability) are enforced via standard forward dataflow and reachability analyses. The MIR instruction set is intentionally simple, with pattern matching decomposed into explicit `switch_variant` terminators and `variant_get_payload` instructions, making the lowering to LLVM IR straightforward.
