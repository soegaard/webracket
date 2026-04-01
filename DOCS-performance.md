# Reference: performance.ffi

This document describes the browser Performance bindings exported by
`ffi/performance.ffi` in WebRacket.

Use this API when you need browser timing information, performance
marks and measures, or the current page's recorded performance
entries.

Assumption in examples: the program is compiled with `--ffi dom`.

## Highlights

- `performance-now` returns a monotonic high-resolution timestamp
- `performance-time-origin` returns the start timestamp for the page
- `performance-event-counts`, `performance-interaction-count`,
  `performance-navigation`, `performance-timing`, and
  `performance-memory` expose the browser's timing metadata
- `performance-event-counts` returns a `performance-event-count-map`
  wrapper instead of a raw browser object
  - `performance-event-count-map-get` and
    `performance-event-count-map-has?` let you inspect individual event
    types
- `performance-clear-marks`, `performance-clear-measures`, and
  `performance-clear-resource-timings` clear browser buffers
- `performance-get-entries`, `performance-get-entries-by-name`, and
  `performance-get-entries-by-type` query the recorded entries
- `performance-mark` and `performance-measure` create named entries
- `performance-measure-user-agent-specific-memory` estimates memory
  usage in browsers that support it
- `performance-set-resource-timing-buffer-size` adjusts the resource
  timing buffer
- `performance-to-json` returns a JSON-style representation of the
  object
