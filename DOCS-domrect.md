# Reference: domrect.ffi

This document describes the browser DOMRect bindings exported by
`ffi/domrect.ffi` in WebRacket.

Use this family when you need the geometry of an element or canvas
relative to the viewport.

Assumption in examples: the program is compiled with `--ffi dom`.

## Highlights

- `dom-rect-left`, `dom-rect-top`, `dom-rect-width`, and `dom-rect-height`
