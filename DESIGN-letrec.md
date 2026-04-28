# DESIGN-letrec.md

This document describes a redesign of the compiler’s handling of
letrec, replacing the current all-or-nothing lowering with a
Waddell-style transformation.

The goal is to minimize unnecessary assignment by partitioning
bindings into simple, lambda, complex, and unreferenced categories,
and generating code that preserves as much direct binding structure as
possible.

Note: Racket `letrec` is the same as Scheme `letrec*`.
So what we need in `compiler.rkt` is an implementation of Racket `letrec` 
aka Scheme `letrec*`.


## 1. Problem

### 1.1 What `letrec` means

A `letrec` binds variables **recursively**:

```racket
(letrec ([x e] ...)
  body)
```

Semantics:

1. Allocate locations for `x`
2. Evaluate `e` in an environment where `x` exist
3. Assign results to `x`
4. Evaluate body

Naive transformation:

```racket
(let ([x undefined] ...)
  (let ([t e] ...)
    (set! x t) ...)
  body)
```

This is correct but problematic.

---

### 1.2 Problems with naive lowering

The naive transformation introduces **unnecessary assignments**:

* Prevents optimizations (inlining, constant propagation)
* Forces variables to be boxed / heap-allocated
* Inhibits closure optimization

The paper explicitly notes that assignments:

> “impede the generation of efficient code” 

---

### 1.3 Desired goal

Transform `letrec` into:

* as many **pure bindings** as possible:

  * `let` for non-recursive
  * `fix` (lambda-only recursion)
* minimal use of `set!`

Key idea:

```text
Separate:
- recursive procedures
- simple values
- complex recursive structures
```

---

## 2. Letrec Transformation (Waddell et al.)

### 2.1 Classification of bindings

Each binding `[x e]` is classified as:

```text
unreferenced : x never used
simple       : x unassigned, e does not refer to letrec-bound vars
lambda       : x unassigned, e is lambda
complex      : everything else
```

---

### 2.2 Transformation

Given:

```racket
(letrec ([x e] ...) body)
```

Transform to:

```racket
(let ([xs es] ...          ; simple
      [xc (void)] ...)     ; complex placeholders
  (fix ([xl el] ...)       ; lambda
    eu ...                 ; unreferenced (effects only)
    (let ([xt ec] ...)
      (set! xc xt) ...)
    body))
```

Key properties:

* `fix` = lambda-only letrec
* simple bindings move **outwards**
* complex bindings use **placeholder + set!**

---

### 2.3 Why this works

* Minimizes assignment
* Enables direct closure wiring
* Preserves semantics of original letrec

---

### 2.4 Requirements

The transformation assumes:

```text
- variables are uniquely named (α-renamed)
- assignment information is known
```

### 2.5 Implementation Notes

Practical implementations of the Waddell transformation (e.g. Vicare)
highlight several important details. First, the transformation relies on
prior analysis (assignment, simplicity, and usage) rather than
recomputing these properties during rewriting. Second, complex RHSs
impose evaluation-order constraints and must be evaluated exactly once,
which requires the introduction of temporaries before assignment. Third,
lambda-like forms (such as case-lambda) must be recognized uniformly so
they can be grouped into fix bindings and support recursive references
without mutation. Finally, even unreferenced bindings must be preserved
when their RHSs may have effects, ensuring that the transformation
remains semantically faithful. Variables are not needed for 
unreferenced bindings; it is enough to run the effect expressions.


---

## 3. New Pipeline

### 3.1 Proposed pipeline

```text
infer-names
=> convert-quotations
=> explicit-begin
=> explicit-case-lambda
=> α-rename

=> uncover-assigned
=> purify-letrec-values     ; Waddell-style

=> assignment-conversion
=> simplify-LFE

=> categorize-applications
=> anormalize
=> closure-conversion
=> flatten-begin
=> generate-code
```

Note: Internally `purify-letrec-values`
can be split in an analysis pass and a rewrite pass.

```
=> uncover-assigned
=> analyze-letrec-values
=> purify-letrec-values
```

where

```
analyze-letrec-values:
  classify each clause as:
  - unreferenced
  - simple
  - lambda
  - complex

purify-letrec-values:
  consume those classifications
  emit let / fix / temporary / set! structure
```

---

### 3.2 Core invariant

```text
Before assignment-conversion:
  - set! still present
  - assignment info available

After assignment-conversion:
  - no set!
  - mutation explicit via boxes
```

---

## 4. New Passes

### 4.1 uncover-assigned

Purpose:

```text
Determine which variables are assigned
```

Output:

```text
assigned? : Var → Bool
```

Implementation:

* scan `(set! x e)`
* mark `x` assigned in its binding scope

---

### 4.2 purify-letrec-values

Implements Waddell transformation.

Steps:

1. Recursively transform RHS and body
2. Collect bindings
3. Partition bindings:

```text
simple / lambda / complex / unreferenced
```

4. Emit nested structure:

```text
let (simple)
let (placeholders)
fix (lambda)
let (temporaries)
set!
body
```

---

### 4.3 assignment-conversion (existing)

Consumes assigned variables:

```text
x → box(x)
(set! x e) → set-box!
```

No semantic change, just representation.

---

## 5. Changes to Existing Passes

### 5.1 lower-letrec-values

Replace with:

```text
purify-letrec-values
```

Major change:

```text
OLD: all-or-nothing lowering
NEW: fine-grained partition
```

---

### 5.2 assignment-conversion

Change:

```text
OLD: discovers assigned variables
NEW: consumes assigned info from uncover-assigned
```

Simplifies implementation.

---

### 5.3 α-rename

No change, but becomes **required earlier**.

---

### 5.4 explicit-case-lambda

Ensure:

```text
case-lambda is treated as lambda-like
```

---

### 5.5 simplify-LFE

Add cleanup after assignment conversion:

```text
remove trivial lets
flatten begins
```

---

## 6. Migration Plan

### Step 1 — Add uncover-assigned

* no behavior change
* record assignment info

---

### Step 2 — Refactor assignment-conversion

* consume assignment info instead of recomputing

---

### Step 3 — Replace lower-letrec-values

Implement Waddell-style partitioning

Keep fallback behavior for safety

That is: Let's have an parameter `current-letrec-strategy` 
that can have two modes `basic` or `wadell`. This allows
users to use `basic` while the new `wadell` strategy is
worked out.


---

### Step 4 — Validate correctness

* recursive lambdas
* mixed bindings
* mutation cases

---

### Step 5 — Enable optimizations

* measure reduction in set!
* check closure conversion improvements

---

## 7. Future Improvements

### 7.1 SCC-based transformation (Fixing Letrec Reloaded)

Replace whole-letrec partitioning with:

```text
dependency graph
=> strongly connected components
=> transform per SCC
```

Benefit:

```text
avoid assignments for nonrecursive complex bindings
```

---

### 7.2 Assimilation of nested bindings

From paper:

```text
(letrec ([x (let (...) ...)]) ...)
=> flatten nested lets
```

Reduces unnecessary complexity.

---

### 7.3 Extend fix beyond lambdas

Paper suggests:

```text
allow data constructors (pairs, vectors)
```

Would enable:

```text
recursive data structures without assignment
```

---

### 7.4 Better “simple” detection

Currently conservative.

Improve:

```text
effect analysis
primitive purity tracking
```

---

### 7.5 letrec* semantics (Racket alignment)

Add:

```text
evaluation-order constraints
```

for internal definitions.

---

## 8. Summary

Design principles:

```text
1. Separate analysis from transformation
2. Delay mutation elimination
3. Preserve as much structure as possible
4. Minimize assignment
5. Enable closure optimization
```

The Waddell transformation is:

```text
simple, effective, and a perfect stepping stone
```

toward the more advanced SCC-based algorithm.

## 9. Addendum

One important clarification concerns the classification of bindings. A
binding is considered simple only if its right-hand side does not refer
to any variables bound by the surrounding `letrec`, whereas `lambda`
bindings are identified syntactically and may freely refer to the entire
recursive group, including themselves and each other. This distinction is
crucial: `lambda` bindings are placed in the fix form precisely because
they support recursive references without requiring assignment, enabling
direct closure wiring. In contrast, non-`lambda` bindings that participate
in recursion must be treated as complex and handled via placeholders and
set!. Additionally, this design deliberately postpones support for
`letrec*`. Since `letrec*` imposes a sequential evaluation order rather
than the simultaneous semantics assumed by the Waddell transformation, it
is excluded from the initial implementation and left as future work once
the core letrec transformation is in place.

---

## References

```text
Waddell, Oscar, Dipanwita Sarkar, and R. Kent Dybvig.
“Fixing Letrec: A Faithful Yet Efficient Implementation of Scheme’s Recursive Binding Construct.”
Higher-Order and Symbolic Computation 18, no. 3–4 (2005): 299–326.
```

```text
Ghuloum, Abdulaziz, and R. Kent Dybvig.
“Fixing Letrec (Reloaded).”
In Proceedings of the Workshop on Scheme and Functional Programming, 2009.
```

```text
Kelsey, Richard, William Clinger, and Jonathan Rees (eds.).
“Revised^5 Report on the Algorithmic Language Scheme.”
ACM SIGPLAN Notices 33, no. 9 (1998): 26–76.
```

```text
Sperber, Michael, R. Kent Dybvig, Matthew Flatt, and Anton van Straaten (eds.).
“Revised^6 Report on the Algorithmic Language Scheme (R6RS).”
2007.
```

