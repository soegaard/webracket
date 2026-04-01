# Reference: element.ffi

This document describes the browser Element bindings exported by
`ffi/element.ffi` in WebRacket.

It covers the generic DOM element helpers that are useful for building
checked browser-side wrappers.

Assumption in examples: the program is compiled with `--ffi dom`.

## Highlights

- `append-child!`, `set-attribute!`, and `get-attribute`
- selector helpers such as `query-selector` and `query-selector-all`
- geometry and scrolling helpers such as `get-bounding-client-rect` and `scroll-to!`
