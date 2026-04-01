# Reference: event.ffi

This document describes the browser Event bindings exported by
`ffi/event.ffi` in WebRacket.

The split page covers the core event hierarchy and the most common
accessors used when handling DOM events.

Assumption in examples: the program is compiled with `--ffi dom`.

## Highlights

- event predicates such as `event?`, `mouse-event?`, and `touch-event?`
- event accessors such as `event-type` and `event-target`
- propagation helpers such as `prevent-default!` and `stop-propagation!`
