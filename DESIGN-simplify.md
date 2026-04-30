Here is a clean **`DESIGN-simplify.md`** documenting the transformations,
organized from simple to advanced and tailored to your `simplify-LFE` pass.

---

````markdown
# DESIGN: simplify-LFE

## Overview

`simplify-LFE : LFE -> LFE` is a local optimization pass inspired by
Chez Scheme’s `cp0`. It performs small, semantics-preserving rewrites
on LFE expressions to:

- reduce syntactic noise
- eliminate dead code
- fold constants
- simplify control flow
- prepare code for later lowering passes

This pass is intentionally conservative:
- no global analysis
- no aggressive inlining
- no code duplication unless safe

---

## Design Principles

1. **Local reasoning only**
   Each transformation depends only on the current expression and its
   immediate subexpressions.

2. **Preserve evaluation order**
   Side effects must not be reordered or duplicated.

3. **Respect multiple values**
   Transformations must not change arity behavior.

4. **Be conservative about purity**
   Only obviously pure expressions are dropped or duplicated.

---

## Expression Classes

We treat expressions in three broad categories:

- **Pure**: constants, lambdas
- **Simple**: variables, quotes
- **Effectful**: applications, mutation, control operators

Only pure/simple expressions are eligible for elimination or duplication.

---

## Transformations

### 1. Constant Propagation

Inline known constant bindings.

```racket
(let-values ([(x) '5])
  x)
=> '5
````

(Initial version may skip this; requires environment tracking.)

---

### 2. Constant Folding

Evaluate primitive operations when all arguments are known constants.

```racket
(app + '1 '2 '3)
=> '6

(app not '#f)
=> '#t

(app eq? '1 '1)
=> '#t
```

Only applies to:

* safe primitives
* known literal arguments

---

### 3. Conditional Simplification

Simplify `if` when the condition is known.

```racket
(if '#f e1 e2)
=> e2

(if '10 e1 e2)   ; anything not #f is truthy
=> e1
```

---

### 4. Sequence Simplification (`begin`)

Flatten and remove useless expressions.

```racket
(begin e)
=> e

(begin (pure) body)
=> body

(begin (begin a b) c)
=> (begin a b c)
```

Pure expressions in non-final position are removed.

---

### 5. Empty Binding Elimination

Remove vacuous `let-values`.

```racket
(let-values () e)
=> e
```

---

### 6. Trivial Let Simplification

Inline trivial bindings.

```racket
(let-values ([(x) e])
  x)
=> e
```

(Only when safe: single value, no duplication concerns.)

---

### 7. Lambda Application (Beta Reduction)

Inline direct lambda applications.

```racket
(app (λ (x) (+ x 1)) '5)
=> (+ '5 1)
=> '6
```

Constraints:

* arguments must be safe to substitute
* no duplication of effectful expressions

---

### 8. Primitive Specialization

Recognize operations on known data.

```racket
(app car '(1 2 3))
=> '1
```

This requires recognizing literal data structures.

---

### 9. Dead Code Elimination

Remove unused pure expressions.

```racket
(begin
  (+ '1 '2)
  e)
=> e
```

Effectful expressions are preserved.

---

### 10. Begin0 Preservation

Do not simplify away `begin0`:

```racket
(begin0 e0 e1 ...)
```

Must preserve:

* value of `e0`
* effects of `e1 ...`

---

### 11. No Reordering of Effects

This is **not allowed**:

```racket
(+ (print 1) (print 2))
```

Must remain unchanged.

---

### 12. Conservative Letrec Handling

Recursive bindings are preserved:

```racket
(letrec-values ...)
```

No inlining or restructuring in the initial version.

---

## Non-Goals (Initial Version)

The following cp0 features are explicitly **not implemented**:

* operand system
* scoring / size limits
* polyvariant specialization
* global inlining
* loop unrolling
* advanced copy propagation
* mutation-aware optimization

---

## Future Extensions

Once the base pass is stable:

### A. Copy Propagation Environment

Track bindings:

```racket
(let-values ([(x) '5])
  (+ x x))
=> '10
```

---

### B. Purity Analysis

Extend `pure?` to include:

* selected primitives
* structural constructors

---

### C. Controlled Inlining

Inline small functions when:

* single use
* small body
* arguments are simple

---

### D. Structure Awareness

Recognize constructed data:

```racket
(let-values ([(p) (cons '1 '2)])
  (car p))
=> '1
```

---

## Example Pipeline

Input:

```racket
(app
  (λ (x)
    (if (pair? x)
        (car x)
        '0))
  '(10 20))
```

Steps:

1. Inline lambda
2. Fold predicate
3. Simplify `if`
4. Fold `car`

Result:

```racket
'10
```

---

## Summary

`simplify-LFE` provides:

* predictable, local rewrites
* reduced IR size
* improved constant exposure
* better input for later passes

It is intentionally simple but forms the foundation for more
cp0-like optimizations later.

```

---

If you want, I can next produce a **phase 2 design** (copy propagation +
safe inlining) that still fits cleanly into your compiler without turning
into full cp0 complexity.
```

---

## Larceny Twobit Rules Used

The local transformations listed in
[LARCENY-twobit-local-transformations.md](./LARCENY-twobit-local-transformations.md)
were a useful source of inspiration for `simplify-LFE`.

The Larceny document is arranged in three columns:

- original form
- rewritten form
- side conditions

The following Twobit-style rules are now implemented, either directly
or in a slightly adapted form that better fits the current `LFE`.

### Sequencing Rules

- `(begin ... (begin ---) ...) => (begin ... --- ...)`
  Nested `begin` forms are flattened.

- `(begin ... 'K ... E) => (begin ... ... E)`
  Pure non-final quoted constants are dropped from `begin`.

- `(begin ... (lambda ---) ... E) => (begin ... ... E)`
  Pure lambda expressions are dropped from non-final `begin` position.

- `(set! I (begin ... E)) => (begin ... (set! I E))`
  `set!` is normalized to keep sequencing explicit.

- `(if (begin ... B0) E1 E2) => (begin ... (if B0 E1 E2))`
  `begin` is hoisted out of `if` test position.

### Conditional Rules

- `(if '#f E1 E2) => E2`

- `(if 'K E1 E2) => E1` when `K != #f`

- `(if (if B0 '#f '#f) E1 E2) => (begin B0 E2)`

- `(if (if B0 '#f 'K) E1 E2) => (if B0 E2 E1)` when `K != #f`

- `(if (if B0 'K '#f) E1 E2) => (if B0 E1 E2)` when `K != #f`

- `(if (if B0 'K1 'K2) E1 E2) => (begin B0 E1)` when `K1` and `K2` are truthy constants

- `(if (if B0 (if B1 #t #f) B2) E1 E2) => (if (if B0 B1 B2) E1 E2)`

- `(if (if B0 B1 (if B2 #t #f)) E1 E2) => (if (if B0 B1 B2) E1 E2)`

- `(if (if X X B0) E1 E2) => (if (if X #t B0) E1 E2)` when `X` is a variable

- `(if (if X B0 X) E1 E2) => (if (if X B0 #f) E1 E2)` when `X` is a variable

- `(if ((lambda (X) (if X X B2)) B0) E1 E2)
   => (if ((lambda (X) (if X #t (if B2 #t #f))) B0) E1 E2)`

- `(if (not E0) E1 E2) => (if E0 E2 E1)`

### Lambda/Application Rules

- `((lambda () E)) => E`
  This was generalized to permit multi-expression bodies, which are
  rebuilt as a `begin`.

- `((lambda (I1 ... Ik . Irest) ---) E1 ... Ek Ek+1 ...)
   => ((lambda (I1 ... Ik Irest) ---) E1 ... Ek (LIST Ek+1 ...))`

- A conservative unary-only normalization inspired by Twobit’s
  lambda/application simplifications:

  ```racket
  ((lambda (x) body) e)
  =>
  (let-values ([(x) e]) body)
  ```

  This is not written in the Twobit list exactly this way, but it
  serves the same goal of reducing application overhead while keeping
  value-count behavior explicit.

## Chez-Style Partial Folding Used

After the Twobit-style control-flow and sequencing rewrites were in
place, `simplify-LFE` also gained a small Chez-inspired partial-folding
layer.

This layer is deliberately separate from ordinary constant folding:

- `foldable`
  means an application may be evaluated immediately when all arguments
  are known constants.

- `partial-foldable`
  means an application may be simplified algebraically even when only
  some arguments are constants.

- `multi`
  marks the exceptional primitives that may produce multiple values.
  Primitive applications are otherwise treated as single-valued by
  default when used by the local value-count predicates.

The current partial folders are intentionally small and local. They are
implemented directly in `simplify-LFE` rather than through a more
general `define-inline`-style registration framework like Chez uses in
`cp0`.

### Arithmetic Families

The following arithmetic rewrites are currently implemented.

- `(+ ... 0 ...) => (+ ... ...)`
  Zero arguments are dropped from `+`.

- `(* ... 1 ...) => (* ... ...)`
  One arguments are dropped from `*`.

- `(- X 0) => X`

- `(/ X 1) => X`

- `(+ X) => X`

- `(* X) => X`

- `(+ C1 ... X ... C2 ...) => (+ C X ...)`
  Exact-integer constants in `+` are combined conservatively.

- `(* C1 ... X ... C2 ...) => (* C X ...)`
  Exact-integer constants in `*` are combined conservatively.

- `(* E0 ... 0 ... En) => (begin E0 ... En 0)`
  This is only used when the surviving expressions are obviously
  numeric and obviously single-valued, so the primitive’s type checks
  are not silently erased.

### Bitwise Families

The following bitwise rewrites are currently implemented.

- `(bitwise-ior ... 0 ...) => (bitwise-ior ... ...)`

- `(bitwise-xor ... 0 ...) => (bitwise-xor ... ...)`

- `(bitwise-and ... -1 ...) => (bitwise-and ... ...)`

- `(bitwise-ior X) => X`

- `(bitwise-xor X) => X`

- `(bitwise-and X) => X`

- `(bitwise-ior C1 ... X ... C2 ...) => (bitwise-ior C X ...)`

- `(bitwise-xor C1 ... X ... C2 ...) => (bitwise-xor C X ...)`

- `(bitwise-and C1 ... X ... C2 ...) => (bitwise-and C X ...)`

- `(bitwise-and E0 ... 0 ... En) => (begin E0 ... En 0)`
  As with `*`, this only applies when the surviving expressions are
  obviously exact-integer-valued and obviously single-valued.

### Sequence/List Families

The following small sequence/list rewrites are currently implemented.

- `(list) => '()`

- `(append) => '()`

- `(append X) => X`

- `(values X) => X`

- `(member X '()) => (begin X '() #f)`

- `(memq X '()) => (begin X '() #f)`

- `(memv X '()) => (begin X '() #f)`

- `(assoc X '()) => (begin X '() #f)`

- `(assq X '()) => (begin X '() #f)`

- `(assv X '()) => (begin X '() #f)`

- `(memf P '()) => (begin P '() #f)`

- `(assf P '()) => (begin P '() #f)`

- `(findf P '()) => (begin P '() #f)`

- `(remove X '()) => (begin X '() '())`

- `(remq X '()) => (begin X '() '())`

- `(remv X '()) => (begin X '() '())`

- `(remf P '()) => (begin P '() '())`

- `(filter P '()) => (begin P '() '())`

- `(andmap P '()) => (begin P '() #t)`

- `(ormap P '()) => (begin P '() #f)`

### Guardrails

The partial-folding layer is intentionally constrained by a few rules.

- Primitive type checks must not be erased.
  For example, `(+ 'x 0)` must not simplify to `'x`, because the
  original program signals an error.

- Type-preserving rewrites therefore rely on small local predicates
  such as:
  - `obviously-number-valued-expression?`
  - `obviously-exact-integer-valued-expression?`
  - `obviously-single-valued-expression?`

- `obviously-single-valued-expression?` is intentionally shallow.
  It is useful as a cheap local guard, but it is not meant to grow into
  a general value-count analysis.

- Sequencing is always preserved explicitly.
  When a primitive result is known from an annihilator such as `0`,
  remaining operands are still evaluated through `begin`.

## Future Optimizations For a Later Pass

Some Twobit-style optimizations were intentionally not added to
`simplify-LFE`, because they want information that is better computed
in a later pass with richer binding/use analysis.

### Rules That Need Body-Use Information

- `((lambda (... IGNORED ...) ---) ... E ...)
   => (begin E ((lambda (... ...) ---) ... ...))`

  This requires knowing that a formal is not used in the body.
  That is not available from purely local pattern matching.

- Beta substitutions of quoted constants into lambda bodies:

  ```racket
  ((lambda (... I ...) E1) ... 'K ...)
  =>
  ((lambda (... ...) E2) ... ...)
  ```

  where `E2 = E1['K/I]`

  This needs occurrence information and careful duplication reasoning.

- Beta substitutions of variables that are known not to be assigned:

  ```racket
  ((lambda (... I ...) E1) ... I2 ...)
  =>
  ((lambda (... ...) E2) ... ...)
  ```

  where `E2 = E1[I2/I]`

  This wants assignment/use information that belongs in a later pass.

- Lambda-floating style rewrites where a bound lambda is lifted into a
  local definition.

  These also rely on binding/assignment knowledge rather than simple
  local shape.

### Rules That Need More Delicate Evaluation Analysis

- `(E0 ... (begin --- E) ...) => (begin --- (E0 ... E ...))`

  This is attractive, but moving `begin` outward through application
  position deserves more careful evaluation-order reasoning than we
  currently want in `simplify-LFE`.

### Rules That Need Richer Value and Numeric Analysis

- More aggressive partial folds that rely on proving type properties of
  arbitrary expressions.

  The current partial folders use shallow local predicates such as
  `obviously-number-valued-expression?` and
  `obviously-exact-integer-valued-expression?`. This is enough for the
  current identity, subset-folding, and annihilator rules, but a later
  pass would be a better place for deeper type/value reasoning.

- Richer multi-value reasoning.

  The current pass uses `multi` metadata and a cheap local
  single-valued heuristic. If future optimizations want stronger
  guarantees about result arity, they should use a later pass with
  explicit value-count information instead of growing the local
  heuristic much further.
