# Implementation Plan: Nominal Named Types

## Problem

`TypeSpec::Named` currently embeds the full expanded type body inline (`type_spec: Box<TypeSpec>`).
When the noder resolves a named type reference, it recursively expands the binding — which infinite-loops
on mutually recursive types like a linked-list node:

```
NodeI32 { data: i32, next: MaybeNodeI32 }
MaybeNodeI32 { Some(*NodeI32), None }
```

Expanding `NodeI32` expands `MaybeNodeI32`, which expands `*NodeI32`, which expands `NodeI32`… forever.

The same structural recursion would hit again in `lower_type_spec` in the blocker once the noder is fixed.

## The Core Idea

Separate *type identity* from *type body*. A `Named` type reference anywhere in the HIR stores only a
`NodeID`. The concrete body (struct fields, enum variants) lives once in `type_map`, keyed by that
`NodeID`. Downstream passes look up the body on demand rather than following an inline pointer. This makes
cycles impossible — a `Named(NodeI32_id)` field inside `MaybeNodeI32`'s body is just an ID, not a
recursive expansion.

---

## Stage 1 — HIR (`src/hir.rs`)

Remove `type_spec` from `NamedType`:

```rust
// Before
pub struct NamedType {
    pub name: NodeID,
    pub type_spec: Box<TypeSpec>,
}

// After
pub struct NamedType {
    pub name: NodeID,
}
```

This is the structural change everything else flows from. It will produce compile errors everywhere
`NamedType` is constructed or matched — those are the exact spots the subsequent stages fix.

---

## Stage 2 — Noder (`src/noder.rs`)

### `node_type_spec` for `Named` (~line 378)

Stop the recursive expansion entirely. Just resolve the `NodeID` and return a thin reference:

```rust
ast::TypeSpec::Named(t) => {
    let scope_pos = module.get_scope_pos(t.id).expect("...");
    let binding = module.find_binding(scope_pos, t.name).expect("...");
    let name_id = self.tree.symbol_map.get(binding.id).expect("...");
    TypeSpec::Named(NamedType { name: *name_id })
}
```

No recursion. The referenced type's body is already (or will be) in `type_map` — this branch only stores
the pointer to it. The `type_spec` produced for the body (struct/enum) is now *shallow*: any named types
appearing in field positions become `Named(id)` references rather than inline expansions.

### `node_decl` for `Decl::Type` (~line 526)

The noder currently stores the expanded body under `decl_id`. The typer later populates
`type_map[ident_id]`. We need to also store the body under `ident_id` in the noder, because that is
what `resolve_type` will look up:

```rust
let decl_id = self.add_root_node(Node::TypeDecl { ident: ident_id });
let type_spec = self.node_type_spec(module, &decl.type_spec);  // shallow — Named fields are just IDs
self.tree.type_map.add(ident_id, type_spec.clone());            // NEW — enables Named lookups
self.tree.type_map.add(decl_id, type_spec);
```

### Hardcoded `NamedType` constructions in unit tests (~lines 2339, 2427)

Remove the `type_spec` field from each inline construction in the noder snapshot test helpers.

---

## Stage 3 — Typer (`src/noder/typer.rs`)

### `Node::TypeDecl` handler (~line 121)

The typer currently wraps the body in `Named(ident, body)` and stores it at `type_map[ident]`. With
the new design, `type_map[ident_id]` already holds the concrete body (added by the noder in Stage 2).
The typer handler simplifies to a no-op:

```rust
Node::TypeDecl { .. } => {
    // type_map[ident_id] already populated by noder; nothing to do here
}
```

### `resolve_type` (~line 824)

Needs `type_map` to look up the body for a `Named(id)`:

```rust
// Before
pub fn resolve_type(ts: &TypeSpec) -> &TypeSpec {
    match ts {
        TypeSpec::Named(t) => resolve_type(&t.type_spec),
        _ => ts,
    }
}

// After
pub fn resolve_type<'a>(ts: &'a TypeSpec, type_map: &'a SideTable<NodeID, TypeSpec>) -> &'a TypeSpec {
    match ts {
        TypeSpec::Named(t) => {
            let body = type_map.get(t.name).expect("missing type body for Named");
            resolve_type(body, type_map)
        }
        _ => ts,
    }
}
```

`resolve_type` only peels `Named` wrappers at the top level — it does not recurse into struct fields or
enum variants — so this lookup does not cycle.

### `resolve_type` call sites (~5 places in typer.rs)

Each already has `node_tree` in scope. Add `&node_tree.type_map` as the second argument.

### Unit tests for `resolve_type` (~lines 1116–1181)

The `named()` helper removes `type_spec`. Each `resolve_type` test now needs a small `SideTable` with
the relevant entry:

```rust
fn named(id: u32) -> TypeSpec {
    TypeSpec::Named(NamedType { name: NodeID::new(id) })
}
// Each resolve_type test builds a small SideTable and passes it in
```

---

## Stage 4 — Blocker (`src/blocker.rs`)

`lower_type_spec` is a free function with ~15 call sites. It needs access to `type_map` for Named
lookups and a cycle guard (`HashSet<NodeID>`) to break pointer-to-recursive-type cycles.

### Convert to a `Blocker` method

Making it a method avoids threading two extra parameters through every call site — the blocker already
holds `self.module` which has `self.module.tree.type_map`:

```rust
// Before: free function
fn lower_type_spec(hir_ts: &hir::TypeSpec) -> TypeSpec { ... }

// After: method on Blocker
fn lower_type_spec(&self, hir_ts: &hir::TypeSpec, in_progress: &mut HashSet<NodeID>) -> TypeSpec { ... }
```

### The `Named` arm — cycle-guarded lookup

```rust
hir::TypeSpec::Named(nt) => {
    if in_progress.contains(&nt.name) {
        // cycle via pointer — opaque is correct under LLVM's opaque pointer model
        TypeSpec::OpaquePtr
    } else {
        in_progress.insert(nt.name);
        let body = self.module.tree.type_map.get(nt.name).expect("missing type body");
        let result = self.lower_type_spec(body, in_progress);
        in_progress.remove(&nt.name);
        result
    }
}
```

### Why `OpaquePtr` is correct here

When the cycle guard fires it means we have already started expanding `NodeI32` and have hit it again
via `*NodeI32` inside `MaybeNodeI32`. With LLVM's opaque pointers, a pointer to a struct is just `ptr`
— no inner type is needed for the pointer value itself. So `Ptr(OpaquePtr)` as the lowered `*NodeI32`
inside the struct layout is accurate.

Tracing the example through with the cycle guard:

```
lower_type_spec(Named(NodeI32_id), in_progress={})
  → insert NodeI32_id; look up → Struct([I32, Named(MaybeNodeI32_id)])
  → lower Struct fields:
      I32 → I32
      Named(MaybeNodeI32_id), in_progress={NodeI32_id}:
        → insert MaybeNodeI32_id; look up → Enum([Some(Ptr(Named(NodeI32_id))), None])
        → lower Enum variants:
            Ptr(Named(NodeI32_id)):
              → lower Named(NodeI32_id) → NodeI32_id IN PROGRESS → OpaquePtr
              → Ptr(OpaquePtr)
            None → Unit
        → Enum { variants: [Ptr(OpaquePtr), Unit] }
        → remove MaybeNodeI32_id
  → Struct([I32, Enum { variants: [Ptr(OpaquePtr), Unit] }])
  → remove NodeI32_id
```

This is finite, correct, and matches what LLVM expects.

### Why this does not break the codegen for variable types

When the blocker lowers a *variable* declared as `*NodeI32`, it calls `lower_type_spec` with an empty
`in_progress`. That expands to the full `Ptr(Struct([I32, Enum([Ptr(OpaquePtr), Unit])]))`. The
`OpaquePtr` only appears *inside* the struct layout (for the recursive back-reference), not in the
variable's own type. So `Deref → Field` projection chains on a top-level `*NodeI32` variable still
work correctly in the codegen.

### Call sites (~15 in blocker.rs)

Top-level call sites become `self.lower_type_spec(ts, &mut HashSet::new())`. Internal recursive calls
thread `in_progress` through.

---

## Stage 5 — Snapshots

The noder snapshot JSONs change because `NamedType` no longer serializes a `type_spec` field. Regenerate:

```bash
rm tests/noder/*.json
cargo test   # generates new snapshots (first run produces failures, writing new files)
cargo test   # passes
```

---

## What This Does NOT Fix (follow-up)

The codegen's `Projection::Deref` handler tracks the current type via `TypeSpec::Ptr(inner)`. If
`inner` is `OpaquePtr` — which only happens for a pointer *inside* a recursive struct field, not for
a top-level variable — subsequent `Field` projections on that pointer would fail. This affects only
deep recursive pointer traversal (e.g., walking a linked list node-by-node in generated code).

The fix would be to have the codegen look up the pointee type from `type_map` on deref rather than
following the embedded inner type. This can be addressed as a follow-up once the pipeline is otherwise
working.

---

## Rough Line Counts

| File | Nature of change | Est. lines |
|---|---|---|
| `src/hir.rs` | Remove 2 lines from struct | ~3 |
| `src/noder.rs` | Simplify Named branch, add `type_map[ident_id]`, fix 2 test constructions | ~20 |
| `src/noder/typer.rs` | Simplify TypeDecl handler, update `resolve_type` sig + 5 call sites + test helpers | ~30 |
| `src/blocker.rs` | Convert `lower_type_spec` to method, add cycle guard, update ~15 call sites | ~50 |
| `tests/noder/*.json` | Regenerated automatically | auto |
