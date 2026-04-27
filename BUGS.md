# BUGS

## Quoted hash literals are mutable

Status: open, blocked on immutable hash-table support.

Minimal repro:

```racket
(define h '#hash((a . 1)))
(hash-set! h 'a 2)
(hash-ref h 'a)
```

Real Racket:

```text
hash-set!: contract violation
  expected: (and/c hash? (not/c immutable?))
```

Current WebRacket behavior:

```text
2
```

`datum->construction-expr` lowers quoted hash datums through `make-hash`,
`make-hasheq`, or `make-hasheqv`. Those constructors produce mutable hash
tables, but quoted hash literals are immutable in Racket.

WebRacket currently supports mutable hashes only, so this should wait until
immutable hash tables are implemented, or until the runtime has at least an
immutable flag/check path for hash values.

## `begin0` drops multiple values from its first expression

Status: open.

Minimal repro: `tmp/begin0-multiple-values-repro.rkt`

```racket
(define-values (a b)
  (begin0
    (values 1 2)
    (void)))

(js-log (list a b))
```

Real Racket:

```text
(1 2)
```

Current WebRacket behavior:

```text
#<void>
```

The code generator handles `begin0` in value position by storing the first
expression into a single local, then running the remaining expressions for
effect. That loses the full value bundle when the first expression returns
multiple values.

## `case-lambda` calls return the wrong value

Status: open.

Minimal repro: `tmp/case-lambda-repro.rkt`

```racket
(define f
  (case-lambda
    [(x) x]
    [(x y) (+ x y)]))

(js-log (list (f 1) (f 1 2)))
```

Real Racket:

```text
(1 3)
```

Current WebRacket behavior:

```text
#<void>
```

The closure-conversion/code-generation path has explicit support for
`case-closure`, but this simple dispatch test does not preserve the selected
arm's result.
