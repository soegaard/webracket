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

## 3. Current Pipeline

### 3.1 Implemented pipeline

The current compiler runs `lower-letrec-values` after `α-rename` and
before assignment conversion:

```text
infer-names
=> convert-quotations
=> explicit-begin
=> explicit-case-lambda
=> α-rename
=> lower-letrec-values
=> assignment-conversion
=> simplify-LFE
=> categorize-applications
=> anormalize
=> closure-conversion
=> flatten-begin
=> generate-code
```

This placement is important:

```text
- unique names are already available
- assignment information is still available
- set! has not yet been eliminated
```

### 3.2 Core invariant

```text
Before assignment-conversion:
  - set! still present
  - assignment info available

After assignment-conversion:
  - no source-level set!
  - mutation explicit via boxes / initialize-boxed!
```

---

## 4. Current Lowering

### 4.1 Strategies

The compiler currently supports two strategies controlled by
`current-letrec-strategy`:

```text
basic
  The older conservative lowering.

waddell
  The newer partitioned lowering.
```

`waddell` is now the default strategy. `basic` remains available as a
debugging and comparison mode.

### 4.2 Classification used by `waddell`

Each clause is classified as one of:

```text
unreferenced
simple-pure
simple-allocates
lambda
complex
```

where:

```text
unreferenced
  binding not used; evaluate RHS only for effects

simple-pure
  unassigned, non-recursive, effect-free RHS

simple-allocates
  unassigned, non-recursive RHS that allocates but is still simple enough
  to recognize structurally

lambda
  unassigned lambda-like RHS, including case-lambda

complex
  everything else
```

The split between `simple-pure` and `simple-allocates` is important.
Allocation is observable in ways that truly pure expressions are not, so
allocating RHSs are lowered more conservatively.

### 4.3 Reconstruction shape

Conceptually, the `waddell` lowering emits:

```text
let-values       ; simple-pure
let-values       ; placeholders for complex / allocating clauses
letrec-values    ; lambda and lambda-like clauses
let-values       ; temporaries for ordered initialization
set!
body
```

The actual implementation is adapted to Racket `letrec`, i.e. Scheme
`letrec*`, so it preserves left-to-right evaluation order for the
non-lambda clauses.

### 4.4 Implemented special cases

The current implementation already handles:

```text
- recursive lambda and case-lambda clauses directly
- exact-arity letrec-values with simple RHSs
- exact-arity (values λ ...) and (values case-lambda ...) clauses
- pure wrappers such as begin / begin0 / if around simple RHSs
- nested let-values assimilation
- nested letrec-values assimilation
- conservative ordered assimilation for allocating nested clauses
```

### 4.5 Current `basic` behavior

The `basic` strategy is still supported, but it has also been hardened:

```text
- assigned recursive lambdas do not stay on the direct recursive path
- recursive case-lambda now works end to end
- multi-lambda letrec-values clauses are normalized before lowering
```

---

## 5. What Was Completed

The migration plan is no longer just a proposal. The following pieces are
implemented:

```text
1. Move letrec lowering after α-rename
2. Add current-letrec-strategy with basic / waddell modes
3. Implement first Waddell-style partitioned lowering
4. Add focused letrec regression suite
5. Add paired compile-time / wat-size measurement tool
6. Support nested assimilation
7. Distinguish pure from allocating simple clauses
8. Support direct recursive case-lambda lowering
9. Make waddell the default
```

The focused regression suite is `test/test-letrec.rkt`. The paired
measurement tool is `tools/measure-letrec-test-basics.rkt`.

---

## 6. Open Follow-Up Work

### 6.1 SCC-based transformation

The current implementation still partitions a whole `letrec` cluster at
once. A later improvement is the SCC-based algorithm from *Fixing Letrec
(Reloaded)*:

```text
dependency graph
=> strongly connected components
=> transform per SCC
```

This could avoid assignments for some currently-complex clauses.

### 6.2 Broader simple detection

Current simple detection is still intentionally conservative.

Possible future improvements:

```text
- richer purity information
- more primitive classifications
- more wrapper-form recognition
```

### 6.3 Direct recursive non-lambda data

The current implementation still reserves the direct recursive path for
lambda-like clauses. Recursive data constructors such as pairs, boxes, or
vectors are still handled conservatively.

### 6.4 Simplification / cleanup

Now that `waddell` is the default, some of the remaining follow-up work is
engineering rather than semantics:

```text
- simplify duplicated basic / waddell support code
- decide how long basic should remain as a supported fallback
- document benchmark results and strategy tradeoffs
```

---

## 7. Notes on Semantics

### 7.1 `letrec` means `letrec*` here

Racket `letrec` has sequential `letrec*`-style RHS evaluation, and the
compiler now respects that fact. Earlier versions of this design note
treated `letrec*` as future work; that is no longer accurate.

### 7.2 Why lambda clauses are special

A binding is considered simple only if its RHS does not refer to any
variables bound by the surrounding `letrec`. By contrast, `lambda` and
`case-lambda` clauses are identified syntactically and may refer to the
entire recursive group, including themselves and each other.

That distinction is what enables direct recursive closure wiring without
introducing assignment for every binding in the group.

---

## 8. Summary

Design principles:

```text
1. Run letrec lowering after α-rename
2. Delay mutation elimination
3. Preserve as much direct binding structure as possible
4. Minimize assignment
5. Respect Racket letrec* evaluation order
6. Keep an escape hatch while the new strategy matures
```

The project is now past the migration stage and into the refinement stage:
the Waddell-style lowering is implemented, tested, benchmarked, and used
by default, while `basic` remains available as a conservative fallback.

---

## 9. Revised SCC Plan

The next major improvement is to add an SCC-based lowering strategy based on
Ghuloum and Dybvig’s “Fixing Letrec (Reloaded).” This should be introduced as
a third strategy, not as an in-place rewrite of the current `waddell` path.

### 9.1 Strategy rollout

Add a third `current-letrec-strategy` mode:

```text
basic
waddell
scc
```

Add a corresponding command-line switch:

```text
--letrec-scc
```

Initially, `scc` may alias `waddell` so the plumbing can be added without
changing behavior.

### 9.2 Why a third strategy

Using a separate `scc` strategy keeps the migration low-risk:

```text
- basic remains the conservative fallback
- waddell remains the current default and known-good optimized path
- scc can evolve experimentally without destabilizing the default
- correctness and benchmark comparisons can be done waddell vs scc
```

### 9.3 Stage 0: plumbing only

Introduce the new strategy and flag:

```text
1. Extend current-letrec-strategy with scc
2. Add --letrec-scc
3. Route scc to the current waddell implementation initially
4. Re-run compiler tests and measurement runs
```

Goal:

```text
Establish the migration switch before adding SCC-specific code.
```

### 9.4 Stage 1: read-only SCC infrastructure

Add the internal machinery without changing lowering:

```text
1. Define a per-binding graph/node representation for one letrec cluster
2. Record, per binding:
   - original position
   - bound ids
   - rhs
   - current clause classification seed
   - referenced letrec-bound ids
   - whether the binding is self-recursive
3. Implement Tarjan SCC computation
4. Build dependency graphs for letrec* semantics
5. Add tests for SCC grouping only
```

Goal:

```text
Prove the graph construction and SCC partitioning independently of codegen.
```

### 9.5 Stage 2: first SCC lowering

Make only the `scc` strategy lower SCC-by-SCC:

```text
1. Keep basic unchanged
2. Keep waddell unchanged
3. For scc:
   - build SCCs for each letrec-values cluster
   - process SCCs in dependency order
   - lower each SCC independently
```

For a singleton SCC:

```text
- drop it if it is unused and pure
- use direct recursive binding if it is an unassigned lambda
- use let if it is not self-recursive
- otherwise use the current placeholder/init path
```

For a multi-binding SCC:

```text
- partition into lambda and non-lambda clauses
- preserve letrec* ordering inside the SCC
- use the existing placeholder + initialization machinery for the complex part
```

Goal:

```text
Recover the main SCC benefit while minimizing semantic churn.
```

### 9.6 Stage 3: order-edge refinement

Refine the graph used by `scc` so it matches the paper/Chez strategy more
closely:

```text
1. Add letrec* order edges for non-freely-movable bindings
2. Preserve left-to-right semantics for effectful or allocating clauses
3. Re-check the order-sensitive letrec test cases
```

Goal:

```text
Make the SCC graph faithful for sequential recursive bindings.
```

### 9.7 Stage 4: assimilation under SCCs

Revisit nested assimilation after SCC lowering is stable:

```text
1. Re-enable or refine nested let-values assimilation within the SCC framework
2. Re-enable or refine nested letrec-values assimilation within the SCC framework
3. Keep conservative handling for allocating and complex clauses unless proven safe
```

Goal:

```text
Recover current structural wins cleanly on top of SCC partitioning.
```

### 9.8 Stage 5: comparison and possible promotion

After `scc` is stable:

```text
1. Compare basic / waddell / scc on test-letrec.rkt
2. Compare basic / waddell / scc on test-basics.rkt
3. Compare real programs such as pict smoke examples
4. Evaluate correctness, compile time, and .wat size
5. Decide whether scc should remain experimental, replace waddell, or become the new default
```

Goal:

```text
Promote scc only after it is clearly better or clearly simpler at equal quality.
```

### 9.9 Recommended migration order

Recommended order of work:

```text
Stage 0  add strategy + flag
Stage 1  add graph + Tarjan + SCC tests
Stage 2  lower SCCs under scc only
Stage 3  refine letrec* order edges
Stage 4  revisit assimilation inside SCC lowering
Stage 5  benchmark and decide on promotion
```

This keeps the current compiler stable while the SCC path matures, and it
preserves both `basic` and `waddell` as fallback/debugging strategies during
the transition.

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
