# BUGS

## LINKLETS

### Uninitialized linklet exports are treated as missing

Status: open.

Minimal repro:

```racket
(define l
  (make-compiled-linklet
   'l
   '()
   '(x)
   (lambda (self)
     (void))))

(define i (instantiate-linklet l '()))
(js-log (instance-variable-names i))
(js-log (instance-variable-value i 'x 'fallback))
```

Real Racket linklets expose exported variables even when they have no
definition; the variable is effectively uninitialized, and referencing it
raises a variable error instead of using the fallback.

Current WebRacket behavior:

```text
null
Symbol(fallback)
```

WebRacket's `make-compiled-linklet` helper records the export list, but
`instantiate-linklet` does not populate missing exports in a fresh instance
with an uninitialized binding. This makes exported-but-unset variables
indistinguishable from variables that were never exported.

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
