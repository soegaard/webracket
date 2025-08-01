# webracket
The WebRacket language is a subset of Racket that compiles to WebAssembly.

## s-exp->fasl

`s-exp->fasl` serializes a value to FASL bytes. If an output port is provided via
`out`, the bytes are written to the port. When `out` is `#f` the bytes are
returned instead. Passing any value other than an output port or `#f` results in
an error. Internally `$s-exp->fasl` delegates to `$fasl:s-exp->fasl` so the same
serializer is used in both cases.

## fasl->s-exp

`fasl->s-exp` decodes a byte string produced by `s-exp->fasl` and reconstructs
the original value.  The WebAssembly runtime includes its own decoder
implementation, while the reference implementation falls back to Racket's
`racket/fasl` library. The Node runtime helper `fasl_to_js_value` can
likewise decode these bytes directly into JavaScript values. Symbols
are decoded as `Symbol.for` instances and pairs are returned as objects
`{tag: 'pair', car, cdr}`.
