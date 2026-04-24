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
