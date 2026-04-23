# BUGS

## LINKLETS

### Instance variable modes are ignored

Status: open.

Minimal repro:

```racket
(define i (make-instance 'i #f 'constant 'x 1))
(instance-set-variable-value! i 'x 2)
(js-log (instance-variable-value i 'x))
```

Real Racket:

```text
x: cannot modify a constant
```

Current WebRacket behavior:

```text
2
```

`make-instance` and `instance-set-variable-value!` accept a mode argument,
where `'constant` and `'consistent` should make a variable constant. Later
`instance-set-variable-value!` and `instance-unset-variable!` calls must reject
changes to an existing constant variable.

The runtime currently stores instance variables as `Symbol -> Box`, so the
mode is parsed but not represented. Fixing this likely needs an instance
variable record with both the value box and the mode/constant flag.

### Invalid instance variable modes are accepted

Status: open.

Minimal repro:

```racket
(define i (make-instance 'i #f 'bogus 'x 1))
(js-log (instance-variable-value i 'x))
```

Real Racket:

```text
make-instance: contract violation
  expected: (or/c #f 'constant 'consistent)
  given: 'bogus
```

Current WebRacket behavior:

```text
1
```

The same problem exists for `instance-set-variable-value!`:

```racket
(define i (make-instance 'i))
(instance-set-variable-value! i 'x 1 'bogus)
(js-log (instance-variable-value i 'x))
```

Current WebRacket behavior:

```text
1
```

Because `make-instance` parses optional arguments as `[data #f] [mode #f]`
before variable bindings, `(make-instance 'i 'x 1)` must also reject `1` as an
invalid mode. WebRacket currently accepts it as an instance with data `'x` and
no variables.

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

### Linklet imports are not checked against imported instances

Status: open.

Minimal repro:

```racket
(define l
  (make-compiled-linklet
   'l
   '((x))
   '()
   (lambda (self imported)
     (void))))

(define imported (make-instance 'imported))
(define result (instantiate-linklet l (list imported)))
(js-log (instance? result))
```

Real Racket checks that every imported variable is exported by the
corresponding import instance during `instantiate-linklet`, even if the body
does not read the variable.

Current WebRacket behavior:

```text
true
```

`instantiate-linklet` currently validates only that `import-instances` is a
list of instances and that the number of import instances matches the number
of import sets. It does not validate the symbol names listed in the compiled
linklet's `importss` field against the corresponding instance.

### Instantiated linklet exports are mutable through the instance API

Status: open.

Minimal repro:

```racket
(define l
  (make-compiled-linklet
   'l
   '()
   '(x)
   (lambda (self)
     (instance-set-variable-value! self 'x 1))))

(define i (instantiate-linklet l '()))
(instance-set-variable-value! i 'x 2)
(js-log (instance-variable-value i 'x))
```

Real Racket rejects mutating a variable exported by an instantiated linklet:

```text
x: cannot modify a constant
```

Current WebRacket behavior:

```text
2
```

This is related to the missing instance-variable mode representation. Real
linklet instantiation makes exported bindings constant from the instance API,
but the WebRacket helper linklet body currently writes ordinary mutable
instance variables.

### Duplicate linklet exports are accepted

Status: open.

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

Current WebRacket behavior:

```text
(x x)
```

`make-compiled-linklet` validates that exports are symbols, but it does not
check for duplicate exported names. Since exports define the externally
accessible instance variables, exact duplicates should be rejected.

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
