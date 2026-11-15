# Proposal: The `when` Construct

## Problem

Manta's `let` pattern is strong for the golden-path style: bind a value from an expression,
and handle the non-matching case inline with `or`, `wrap`, or `!`. This works well when the
non-matching case is the exceptional path you want to handle and move past.

However, there is a recurring pattern that `let` does not serve well: executing a block of code
only when an expression matches a specific variant, where the non-matching case is simply
"do nothing, continue." Rust solves this with `if let`, but that approach was rejected for
Manta for two reasons:

1. It mixes two keywords (`if` and `let`) for a single construct, creating an irregular pattern
   that is harder to scan quickly in unfamiliar code.
2. It opens the door to `while let`, `else if let`, and other compounding forms that add
   surface area to the language without proportionate value.

The goal is a construct that:
- Binds variables from a pattern, scoped to a block
- Executes the block only on match
- Does not require an else branch
- Reads naturally without prior knowledge of the language
- Stays visually distinct from `let` so the two idioms are easy to tell apart at a glance

---

## Alternatives Considered

### Option A: Non-exhaustive `match` with `_` wildcard

Allow `match` to be used without covering all variants by including a `_` wildcard arm.
This already works today since `_` is supported as a pattern.

```
match result {
    .Ok(v) { fmt_println(v) }
    _ {}
}
```

**Why it was set aside:** The empty `_ {}` arm is pure noise — it exists only to satisfy
exhaustiveness, not to express intent. More importantly, it does not solve the readability
goal. A developer reading this cannot immediately tell whether the missing arms were
intentionally omitted or accidentally forgotten. The value of exhaustive `match` is that
it forces you to think about every variant; relaxing that for convenience undermines the
feature.

---

### Option B: `if expr is .Variant(v)`

Introduce an `is` operator that tests whether an expression matches a pattern. Used as
a boolean expression in an `if` condition it gives you `if let`-style syntax without
the keyword mixing:

```
if result is .Ok(v) {
    fmt_println(v)
}
```

`is` without a binding would also work as a plain boolean:

```
let ok = result is .Ok
```

**Why it was set aside:** Introducing `is` as both a boolean operator and a
pattern-binding form creates an inconsistency. When used standalone (`result is .Ok`)
it returns a bool and binds nothing. When used in an `if` head with a destructuring
pattern (`result is .Ok(v)`) it returns a bool *and* binds `v` into the block scope.
These two forms look similar but behave differently, which is exactly the kind of subtle
distinction that makes code harder to read and teach. It also requires the `if` keyword
to be involved in the scoping rules, which was identified as a core thing to avoid.

---

### Option C: `let` with a `then` branch

Keep everything under the `let` umbrella by adding a `then` keyword as a success-side
counterpart to `or`:

```
// failure side — existing
let .Ok(v) = result or {
    return
}

// success side — proposed
let .Ok(v) = result then {
    fmt_println(v)
}
```

**Why it was set aside:** While the symmetry with `or` is appealing, combining `let` and
`then` in one statement adds a new reading mode to `let`. The `let` construct currently
has a single clear job: bind a value or handle failure. Adding an optional success branch
means `let` now sometimes executes a scoped block and sometimes does not, depending on
whether `then` is present. This makes the `let` pattern harder to follow at a scan, which
is the opposite of what makes it useful today. It also ties two different concerns — binding
into the current scope and conditionally executing a block — to the same keyword.

---

## Recommendation: `when`

Introduce `when` as a dedicated keyword for conditional pattern execution. It replaces the
`if`/`let` mixing entirely and gives the construct its own clear identity:

```
when .Ok(v) = result {
    fmt_println(v)
}
```

### Reasoning

**`let` and `when` each do one job.** `let` binds into the current scope and requires a
failure handler. `when` binds into a block scope and silently does nothing on non-match.
A developer reading Manta code can tell immediately which idiom is in use and what the
control flow implications are without reading the rest of the line.

**It fits the existing mental model.** The pattern syntax is identical to `let`. The
`= expr` placement is identical. The only new thing to learn is the keyword and its
semantics, which are described by the English word "when."

**No `else` needed, by design.** If you need to handle multiple variants or need an
else branch, `match` is the right tool. `when` is intentionally single-purpose. This
also aligns with the broader direction of removing `else` from the language, since `when`
does not depend on or encourage it.

**No keyword mixing.** `when` is a standalone construct. It does not borrow from `if`,
`let`, or any other keyword. This keeps the language surface small and the set of patterns
a developer needs to recognize manageable.

---

## Code Examples

### Basic usage

```
when .Ok(v) = result {
    fmt_println(v)
}
```

### Nested when

Each `when` has its own clearly scoped binding. Nesting is readable:

```
when .Some(inner) = outer {
    when .Ok(v) = inner {
        fmt_println(v)
    }
}
```

### when inside a function with early returns

`when` does not affect control flow outside its block. Execution always continues
after the block regardless of whether the pattern matched:

```
fn handle(result Div) {
    when .Ok(v) = result {
        fmt_println(v)
    }
    // always reached
    fmt_println("done")
}
```

### Contrast with `let` — making the idiom choice clear

Use `let` when you need the binding in the outer scope and must handle failure:

```
fn read(path str) ReadFile {
    let .Ok(f) = os_open(path) wrap .OpenErr
    // f is available here
    return .Ok(f)
}
```

Use `when` when you only want to act on a match and the non-match case is uninteresting:

```
fn log_if_ok(result Div) {
    when .Ok(v) = result {
        fmt_println(v)
        // v only available inside this block
    }
}
```

### when with no inner bindings

If the variant carries no data, the pattern has no binding but `when` still works
as a clean conditional:

```
when .Ready = status {
    start()
}
```

