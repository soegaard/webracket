# BUGS-5-5

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
The fix moved assignment-target validation to `α-rename`, where top-level
bindings introduced by earlier forms are available.

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

Current WebRacket behavior:

```text
#<void>
```

The compiler classifies top-level references by symbol name against all
top-level definitions, regardless of source order. See `top-variable?` in
`compiler.rkt`. Code generation then reads the boxed top-level global directly,
without checking whether it still contains `$undefined`.

Related repro:

```racket
(f)
(define (f) 42)
```

Current WebRacket behavior is a later runtime application error involving
`#<undefined>`, not Racket's earlier "cannot reference an identifier before its
definition" error.

The same root cause appears inside top-level definition RHSs:

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

Real Racket reports an undefined-before-definition error. Current WebRacket
prints `#<undefined>`.

It also appears in top-level assignment RHSs:

```racket
(define x 0)
(set! x y)
(define y 2)
x
```

Real Racket reports that `y` is undefined before definition. Current WebRacket
stores `#<undefined>` in `x`.

The fix records top-level bindings that are referenced before their definition
has been evaluated. Reads of those bindings check for `$undefined` at runtime,
including reads in effect position; ordinary top-level reads stay on the fast
path.

## `letrec-values` placeholder reads produce `0`

Status: open

Minimal repro:

```racket
(letrec-values ([(x) x]) x)
```

Real Racket:

```text
x: undefined;
 cannot use before initialization
```

Current WebRacket behavior:

```text
0
```

Another repro:

```racket
(letrec-values ([(x) y] [(y) 1]) x)
```

Real Racket reports that `y` is undefined before initialization. Current
WebRacket returns `0`.

Multi-value bindings show the same problem:

```racket
(letrec-values ([(x y) (values x 1)]) x)
```

Real Racket reports that `x` is undefined before initialization. Current
WebRacket returns `0`.

The parser rewrites complex `letrec-values` clauses so that non-lambda bindings
are first introduced with literal `0` placeholders:

```racket
(let-values ([(x) 0] ...)
  ...)
```

See the `letrec-values` rewrite in `compiler.rkt`. Premature references
therefore read the placeholder as a real value instead of triggering
Racket's uninitialized-binding error.
