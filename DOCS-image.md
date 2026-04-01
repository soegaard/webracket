# Reference: image.ffi

This document describes the browser HTMLImageElement bindings exported
by `ffi/image.ffi` in WebRacket.

The split page covers image construction and the common image properties
used by browser code.

Assumption in examples: the program is compiled with `--ffi dom`.

## Highlights

- `image-new` for constructing an image element
- source and metadata helpers such as `image-src`, `image-alt`, and `image-current-src`
- loading state helpers such as `image-loading` and `image-complete?`
