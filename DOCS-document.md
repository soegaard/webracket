# Reference: document.ffi

This document describes the browser Document bindings exported by
`ffi/document.ffi` in WebRacket.

Use it for the current document, element lookup, selector queries, and
document-level helpers.

Assumption in examples: the program is compiled with `--ffi dom`.

## Highlights

- `document`, `document-head`, and `document-body`
- `document-create-element`, `document-create-text-node`, and selector queries
- `document-get-element-by-id` for simple node lookup
