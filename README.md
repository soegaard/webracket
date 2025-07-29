# webracket
The WebRacket language is a subset of Racket that compiles to WebAssembly.

## s-exp->fasl

`s-exp->fasl` serializes a value to FASL bytes. If an output port is provided via
`out`, the bytes are written to the port. When `out` is `#f` the bytes are
returned instead. Passing any value other than an output port or `#f` results in
an error. Internally `$s-exp->fasl` delegates to `$fasl:s-exp->fasl` so the same
serializer is used in both cases.
