# Reference: canvas.ffi

This document describes the browser Canvas bindings exported by
`ffi/canvas.ffi` in WebRacket.

The split page combines the `HTMLCanvasElement` surface with the 2D
rendering context helpers.

Assumption in examples: the program is compiled with `--ffi dom`.

## Highlights

- `canvas-get-context`, `canvas-width`, and `canvas-height`
- `canvas-to-data-url` and `canvas-to-blob`
- `canvas-2d-fill-rect`, `canvas-2d-stroke-rect`, and the path helpers
