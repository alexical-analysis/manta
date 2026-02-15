# Manta MIR (Middle IR) Design

This document describes the design of Manta's Middle IR (MIR). The MIR is a block-based, control-flow oriented intermediate representation that sits between the existing HIR and the Cranelift backend. It is intended to be a stable, analyzable, and easily lowerable representation, with first-class support for definite-initialization checking and clear semantics for local variable binding and exception/`or`-handler control flow.

## Overview

MIR is a function-centric, basic-block based IR. Each function is represented as a control-flow graph (CFG) of basic blocks; each block contains a sequence of non-branching instructions and ends with a single terminator which encodes control flow (jump, branch, switch, return, etc.). Values are represented by value identifiers (temporaries) and named locals (storage slots). The MIR is strongly typed at the value level and is designed to make control- and data-flow analysis straightforward.

Key language-driven requirements that informed this design:
- Support uninitialized local variables declared with `let IDENT = undefined` and a flow checker that ensures variables are always initialized before use.
- Ensure that `or`-style exception/alternative handlers attached to pattern-let forms (e.g. `let .Ok(n) = call() or { ... }`) do not allow control to continue into the binding's continuation unless the handler never returns to that continuation (i.e., handlers must prevent the use of the bound variable if they fall through).

## Goals

- Provide a block-based IR that is straightforward to build from HIR and to lower to Cranelift.
- Make definite-initialization analysis (and related verification) simple and conservative-correct.
- Provide an explicit representation for `pattern-or` binding semantics so that handler divergence can be reliably validated.
- Keep instruction set small but expressive enough to represent common language constructs (calls, destructuring, control flow, loads/stores, returns, etc.).
- Provide clear invariants and a verifier that catches misuse of uninitialized locals and invalid `or` handlers.

## Assumptions and Constraints

- MIR is per-function; there is no interprocedural MIR linking in this design (lowering to Cranelift will generate per-function IR and call sites).
- The HIR will supply types for expressions; MIR preserves type information on values.
- Locals (declared with `let`) map to storage slots; temporaries are single-assignment value identifiers produced by instructions.
- `or` handlers are expressed explicitly in MIR (as target block ids) and are subject to reachability/divergence checks described below.

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
- Switch(enum_value, {variant -> target})
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

- Pattern/Destructure binding with `or` handler:
  - pattern_bind_or(src: ValueId, pattern, bindings: [(LocalId, ValueId)], handler: BlockId)
    - Tries to match `src` against `pattern`.
    - On success: binds values into the given LocalIds (conceptually `store_local` + mark initialized) and continues execution in the current block after the instruction.
    - On failure: control transfers to `handler` block.
    - Verification rule: `handler` must not be able to reach the continuation point that expects the bindings to exist (see "Validation & Flow Checks").

- Move/Copy semantics:
  - move(dst_local, src_value)
  - copy(dst_local, src_value)

- Drop/Finalize local:
  - drop_local(LocalId) -- explicit drop request for destructors

- Misc:
  - set_uninitialized(LocalId) -- used to represent `let x = undefined` at declaration time
  - set_initialized(LocalId) -- used when a store or binding guarantees initialization

Two important design decisions:
1. Locals have an associated initialization state tracked by MIR analysis rather than relying on SSA alone. This makes representing `let ... = undefined` straightforward.
2. Pattern-bind-or is an instruction-level primitive that encodes the two-path semantics (success: introduce bindings and continue; failure: jump to handler).

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

## Enforcing `or` handler termination relative to bindings

Problem: an `or` handler that returns control to the continuation would make the pattern-bound locals unavailable on some paths (i.e., the pattern would have failed but control still reaches the same continuation expecting the variable to be bound), which would create an unsound use of an uninitialized variable.

Invariant to enforce: For each pattern_bind_or at block B that continues to point C on success and specifies handler H for failure, H must not be able to reach C (there is no path from H to C in the CFG), unless every path from H to C re-establishes (initializes) the same bindings prior to use.

Practical checks to enforce this invariant:
- Compute reachability from H. If C is reachable, then either:
  - Reject the pattern as invalid (simple conservative rule), OR
  - Verify that all paths from H to C definitely initialize the required locals before the first use in C (more complex).

Recommended conservative policy (initial implementation): require that H not reach C (i.e., handler must be diverging with respect to C). Diverging includes return-from-function, break/continue to an enclosing construct that ensures C is not reached, panic, or otherwise branching to code that can't reach C.

This check is implemented with a simple reachability traversal from H; if it finds C, it errors with guidance that the handler must not fallthrough into the continuation.

## Building MIR from HIR

- For each function, create a MIRFunction with an entry block.
- Map HIR local declarations:
  - `let x = undefined` -> create a LocalId and emit set_uninitialized(x) at the entry (or record as uninitialized in the locals table).
  - `let x = expr` -> lower expr to MIR and emit store_local(x, tmp)
- Lower expressions to sequences of MIR instructions that produce ValueIds, use temporaries when necessary.
- Lower pattern-let with `or` as a pattern_bind_or instruction with the handler block id taken from the HIR `or` block.
  - Note: the HIR desugars `let`-with-`or` into a variable declaration followed by a match; the `or` arm is represented as a match/handler successor. MIR therefore performs definite-initialization and handler reachability analysis on the match's control-flow edges rather than relying on distinct `let/or`, `let/wrap`, or `let/panic` MIR primitives. In practice this means initializing a variable in the `or` arm (for example, setting a default value) is acceptable so long as every path that reaches the continuation definitely initializes the local.
- After constructing basic blocks, compute predecessors and run the definite-initialization analysis and reachability checks for handlers.

## Lowering to Cranelift (mapping notes)

- Blocks -> Cranelift blocks
- ValueIds -> Cranelift SSA values
- Locals -> stack slots or frame-slots. Reads/writes become load/store sequences; in simple lowering, store_local becomes a store to the stack slot, and load_local loads into an SSA temporary.
- Pattern-bind-or -> lowered to: evaluate src; test/match the discriminant; conditional branch to success-target (with bound value lowered and stored into locals) or branch to handler-target; the handler-target must not allow control to reach the success-target unless it reinitializes locals.
- Terminators map directly to Cranelift branch/return instructions.
- Verification must fully complete before lowering (e.g., handler reachability checks), because lowering cannot magically prevent a handler from falling through.

## Example (pseudo-MIR)

Function write_demo(f, buf) -> i32

locals:
  n: i32 (declared with `let n = undefined`)

blocks:

bb0:
  tmp0 = call(write_file, f, buf)
  pattern_bind_or(tmp0, pattern=.Ok(n_val), bindings=[(n, n_val)], handler=bb_err)
  // after this instruction n is definitely initialized in bb0 continuation
  use_n = load_local(n)
  return use_n

bb_err:
  // must not reach bb0 (conservative rule: reject if it does)
  return -1


## Verification & invariants

- Each block must end with a single terminator.
- No instruction may use a local that is not definitely initialized at that program point (definite-init check must pass before lowering).
- For each pattern_bind_or, the handler block must not be able to reach the continuation block (or must re-initialize bindings on all paths) — conservative rejection otherwise.
- Block arguments/phi-like transfers must be explicit and consistent across predecessor edges.

## Open decisions (implementation scope, not speculative features)

- Representing block arguments vs. explicit stores/loads: prefer explicit store/load lowering from HIR, but allow block arguments as lowering convenience if it simplifies code generation.

## Summary

This MIR is a block-based, typed representation designed to make definite-initialization checking and `or`-handler validation explicit and analyzable prior to lowering to Cranelift. Key invariants (definite-init and handler non-reachability) are enforced via standard forward dataflow and reachability analyses. The MIR instruction set centers on explicit stores to locals, pattern-bind-or primitives, and canonical terminators to keep lowering straightforward.
