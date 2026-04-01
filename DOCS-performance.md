# Reference: performance.ffi

This document describes the browser Performance bindings exported by
`ffi/performance.ffi` in WebRacket.

Use this API when you need a browser timestamp for measuring elapsed
time or coordinating animation work.

Assumption in examples: the program is compiled with `--ffi dom`.

## Highlights

- `performance-now` returns a monotonic high-resolution timestamp
