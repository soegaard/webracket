# Reference: window.ffi

This document describes the browser Window bindings exported by
`ffi/window.ffi` in WebRacket.

The split page focuses on the pieces most commonly used from the
checked `window` wrapper:

- dialogs and browser chrome
- timers and animation callbacks
- scrolling, resizing, and navigation
- access to the current document and selection

Assumption in examples: the program is compiled with `--ffi dom`.

## Highlights

- `window` and `window-document` for browser entry points
- `window-open`, `window-confirm`, and `window-prompt` for navigation and dialogs
- `window-set-timeout`, `window-request-animation-frame`, and related timer helpers
- `window-scroll-to`, `window-scroll-by`, and `window-structured-clone`
