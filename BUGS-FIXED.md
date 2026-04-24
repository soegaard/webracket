# BUGS FIXED

## `datum->correlated` wraps an already-correlated datum

Status: fixed

Minimal repro:

```racket
(define c (datum->correlated 'inner))
(define d (datum->correlated c))

(js-log (eq? c d))
(js-log (correlated-e d))
```

Real Racket:

```text
#t
inner
```

Previous WebRacket behavior:

```text
false
#(struct correlated #f #f #f #f #f inner #hash())
```

`datum->correlated` is implemented in terms of Racket's `datum->syntax`,
which returns an already-correlated datum unchanged. The runtime now returns
the input immediately in that case, ignoring supplied srcloc/property
arguments just like Racket.

## Correlated syntax edge cases

Status: fixed

Minimal repros:

```racket
(define inner (datum->correlated 'x))
(define converted (correlated->datum (box inner)))
(correlated? (unbox converted))
```

```racket
(define c
  (correlated-property
   (correlated-property (datum->correlated 'seed) 'tag 'value)
   123
   'number))
(correlated-property-symbol-keys c)
```

```racket
(define c (correlated-property (datum->correlated 'seed) 'tag #f))
(list (correlated-property c 'tag)
      (correlated-property-symbol-keys c))
```

Real Racket:

```text
#t
(tag)
(#f (tag))
```

Previous WebRacket behavior:

```text
false
(tag 123)
(false null)
```

The runtime now matches Racket's correlated-object behavior for these cases:
`correlated->datum` preserves boxes like vectors, `correlated-property-symbol-keys`
filters to symbol keys, and `correlated-property` stores `#f` as a real property
value instead of treating it as a deletion sentinel.

## Correlated syntax property keys and srcloc structs

Status: fixed

Minimal repros:

```racket
(define k1 (string-copy "k"))
(define k2 (string-copy "k"))
(define c0 (datum->correlated 'seed))
(define c1 (correlated-property c0 k1 'v1))
(define c2 (correlated-property c1 k2 'v2))
(list (equal? k1 k2)
      (eq? k1 k2)
      (correlated-property c2 k1)
      (correlated-property c2 k2))
```

```racket
(define c (datum->correlated 'x (srcloc 'src 1 0 1 1)))
(list (correlated-source c)
      (correlated-line c)
      (correlated-column c)
      (correlated-position c)
      (correlated-span c))
```

Real Racket:

```text
(#t #f v1 v2)
(src 1 0 1 1)
```

Previous WebRacket behavior:

```text
(#t #f #f v2)
datum->correlated: contract violation
```

The runtime now treats correlated-property keys as identity-sensitive when
replacing an existing property, so equal but distinct non-symbol keys can
coexist. `datum->correlated` also accepts `srcloc?` values and copies their
fields through the same srcloc validation path used for vectors and lists.

## Top-level `set!` after definition is rejected

Status: fixed

Minimal repro:

```racket
(define x 0)
(set! x 1)
x
```

Real Racket:

```text
1
```

Previous WebRacket behavior:

```text
parse: set!: assignment to unbound identifier
```

This was a regression from rejecting unbound `set!` targets during `parse`.
The fix moved assignment-target validation to `alpha-rename`, where top-level
bindings introduced by earlier forms are available.

## Top-level `set!` before definition is accepted

Status: fixed

Minimal repro:

```racket
(set! x 1)
(define x 0)
x
```

Real Racket:

```text
set!: assignment disallowed;
 cannot set variable before its definition
```

Previous WebRacket behavior:

```text
0
```

The `set!` target resolved in `alpha-rename` because `initial-rho` was seeded
with all top-level bindings collected from the whole program, not just
definitions that had appeared earlier in source order.

The fix tracks top-level definitions seen so far in source order. Immediate
top-level assignments must target an already-seen binding, while delayed
assignments inside lambdas can still refer to later top-level definitions.

## Top-level reference before definition is not checked

Status: fixed

Minimal repro:

```racket
x
(define x 0)
```

Real Racket:

```text
x: undefined;
 cannot reference an identifier before its definition
```

Previous WebRacket behavior:

```text
#<void>
```

The compiler classified top-level references by symbol name against all
top-level definitions, regardless of source order. Code generation then read
the boxed top-level global directly, without checking whether it still
contained `$undefined`.

Related repro:

```racket
(f)
(define (f) 42)
```

The same root cause appeared inside top-level definition RHSs:

```racket
(define x x)
x
```

and:

```racket
(define x y)
(define y 1)
x
```

It also appeared in top-level assignment RHSs:

```racket
(define x 0)
(set! x y)
(define y 2)
x
```

The fix records top-level bindings that are referenced before their definition
has been evaluated. Reads of those bindings check for `$undefined` at runtime,
including reads in effect position; ordinary top-level reads stay on the fast
path.

## `letrec-values` placeholder reads produce `0`

Status: fixed

Minimal repro:

```racket
(letrec-values ([(x) x]) x)
```

Real Racket:

```text
x: undefined;
 cannot use before initialization
```

Previous WebRacket behavior:

```text
0
```

Another repro:

```racket
(letrec-values ([(x) y] [(y) 1]) x)
```

Real Racket reports that `y` is undefined before initialization. Previous
WebRacket behavior returned `0`.

Multi-value bindings showed the same problem:

```racket
(letrec-values ([(x y) (values x 1)]) x)
```

Assignment before initialization was also accepted:

```racket
(letrec-values ([(x) (begin (set! x 1) 2)]) x)
```

A call through another binding before initialization also read the placeholder:

```racket
(letrec-values ([(x) (f)] [(f) (lambda () x)]) x)
```

Internal definitions exposed the same rewrite bug:

```racket
(define (f)
  (define x x)
  x)
(f)
```

and:

```racket
(define (f)
  (define x y)
  (define y 1)
  x)
(f)
```

Assignment-before-initialization through an internal definition was also
accepted:

```racket
(define (f)
  (define x (begin (set! x 1) 2))
  x)
(f)
```

Even RHSs that ultimately produced lambdas could read or call another
uninitialized binding too early:

```racket
(letrec-values ([(f) (begin (g) (lambda () 1))]
                [(g) (lambda () 2)])
  (f))
```

and:

```racket
(letrec-values ([(f) (begin g (lambda () 1))]
                [(g) (lambda () 2)])
  (f))
```

The fix uses `unsafe-undefined` for generated `letrec-values` placeholders,
guards `unboxed` and user `set-boxed!` so the placeholder cannot leak from safe
code, and uses a separate internal `initialize-boxed!` primitive for the
compiler's own initialization writes. Mixed lambda/complex `letrec-values`
forms now keep source-order placeholder initialization instead of initializing
later lambda clauses before earlier complex RHSs run.

## Top-level variable references are reported as constant

Status: fixed

Minimal repro:

```racket
(define x 1)
(variable-reference-constant? (#%variable-reference x))
```

Real Racket:

```text
#f
```

Previous WebRacket behavior:

```text
#t
```

Lexical local variable references are constant in Racket, but top-level
variable references are not. WebRacket classified `#%variable-reference` for a
top-level binding like a non-top lexical reference before code generation, so
`$VariableReference` was constructed with the `constant?` flag set.

The fix refines `VariableReferenceId` during `alpha-rename`: a `non-top`
variable reference that resolves to a collected top-level binding is rewritten
to `top`, while local lexical variable references remain `non-top`.

## Invalid instance variable modes are accepted

Status: fixed

Minimal repro:

```racket
(define i (make-instance 'i #f 'bogus 'x 1))
(js-log (instance-variable-value i 'x))
```

and:

```racket
(define i (make-instance 'i))
(instance-set-variable-value! i 'x 1 'bogus)
(js-log (instance-variable-value i 'x))
```

Real Racket:

```text
contract violation
  expected: (or/c #f 'constant 'consistent)
```

Previous WebRacket behavior:

```text
1
```

The fix validates the optional mode in both `make-instance` and
`instance-set-variable-value!`, while leaving full constant/consistent
enforcement to the separate instance-variable metadata bug.

## Duplicate linklet exports are accepted

Status: fixed

Minimal repro:

```racket
(define l
  (make-compiled-linklet
   'l
   '()
   '(x x)
   (lambda (self)
     (instance-set-variable-value! self 'x 1))))

(js-log (linklet-export-variables l))
```

Real Racket rejects the corresponding linklet shape:

```text
invalid parameter list ...
```

Previous WebRacket behavior:

```text
(x x)
```

The fix validates `make-compiled-linklet` exports with a temporary `hasheq`
of seen symbols and rejects duplicate exported names before constructing the
compiled linklet.

## Byte-string literals are mutable

Status: fixed

Minimal repro:

```racket
(define b #"a")
(bytes-set! b 0 98)
b
```

Real Racket:

```text
bytes-set!: contract violation
  expected: (and/c bytes? (not/c immutable?))
```

Previous WebRacket behavior:

```text
#"b"
```

Current WebRacket behavior:

```text
RuntimeError: unreachable
at raise-expected-mutable-bytes
at bytes-set!
```

The runtime now represents quoted byte strings as immutable `$Bytes`; the
basic suite also checks that `#"ab"` satisfies `immutable-bytes?` and not
`mutable-bytes?`.

## Closure-conversion free-variable set type bug in `topmodule`

Status: fixed

Running some `-r` programs triggered an internal compiler/runtime contract
error:

```text
id-set-ids: contract violation
 expected: id-set?
 given: '()
```

In `determine-free-variables`, `topmodule` used `'()` instead of `empty-set`:

```racket
(ModuleLevelForm m '())
(values T '())
```

Downstream code expects `id-set`.

The fix updated `compiler.rkt` to use `empty-set` in both places.
