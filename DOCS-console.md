# Reference: `console.ffi`

## Chapter 1 — Introduction

This document describes the browser Console bindings exported by `ffi/console.ffi` in WebRacket.

The surface mirrors the browser `console` object and includes both widely supported methods and the additional deprecated/non-standard methods documented by MDN.

Assumption in examples: the program is compiled with `--ffi console`.

## Chapter 2 — Conventions

### 2.1 Type Legend

| Type | Meaning |
|---|---|
| `(value)` | Packed console arguments converted through the FFI value bridge. |
| `()` | No arguments (input) or no result / void (output). |

### 2.2 Argument Packing

- Variadic console methods take a single packed argument list at the raw layer.
- The checked `console-*` wrapper is responsible for building that argument list.
- Label-like arguments are normalized to strings by the wrapper.

### 2.3 Example Patterns

- Logging a mixed message:
  - `(console-log "hello" 42)`
  - `(js-console-log (vector "hello" 42))`
- Using a symbol label:
  - `(console-count 'requests)`
  - `(console-time-log 'render "phase" "layout")`
- Grouping output:
  - `(console-group "load")`
  - `(console-log "starting")`
  - `(console-group-end)`

### 2.4 Raw Call Shapes

- Pack variadic arguments into one vector:
  - `(js-console-log (vector "hello" 42))`
  - `(js-console-time-log (vector "render" "layout" 12))`
- Pass one packed value for optional-label methods:
  - `(js-console-count (vector "requests"))`
  - `(js-console-time-stamp (vector "frame"))`

## Chapter 3 — Logging

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| `js-console-log` | `(value)` | `()` | `(js-console-log (vector "hello" 1))` | write a log message. |
| `js-console-info` | `(value)` | `()` | `(js-console-info (vector "hello"))` | write an informational message. |
| `js-console-warn` | `(value)` | `()` | `(js-console-warn (vector "careful"))` | write a warning message. |
| `js-console-error` | `(value)` | `()` | `(js-console-error (vector "boom"))` | write an error message. |
| `js-console-debug` | `(value)` | `()` | `(js-console-debug (vector "details"))` | write a debug message. |
| `js-console-assert` | `(value)` | `()` | `(js-console-assert (vector #f "bad"))` | report a failed assertion. |

## Chapter 4 — Inspection

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| `js-console-clear` | `()` | `()` | `(js-console-clear)` | clear the console. |
| `js-console-dir` | `(value)` | `()` | `(js-console-dir (vector obj))` | inspect a value. |
| `js-console-dirxml` | `(value)` | `()` | `(js-console-dirxml (vector node))` | inspect XML/HTML output. |
| `js-console-table` | `(value)` | `()` | `(js-console-table (vector data columns))` | display tabular data. |

## Chapter 5 — Groups

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| `js-console-group` | `(value)` | `()` | `(js-console-group (vector "group"))` | start a group. |
| `js-console-group-collapsed` | `(value)` | `()` | `(js-console-group-collapsed (vector "group"))` | start a collapsed group. |
| `js-console-group-end` | `()` | `()` | `(js-console-group-end)` | end the current group. |

## Chapter 6 — Counters and Timers

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| `js-console-count` | `(value)` | `()` | `(js-console-count (vector "hits"))` | increment a counter. |
| `js-console-count-reset` | `(value)` | `()` | `(js-console-count-reset (vector "hits"))` | reset a counter. |
| `js-console-time` | `(value)` | `()` | `(js-console-time (vector "render"))` | start a timer. |
| `js-console-time-end` | `(value)` | `()` | `(js-console-time-end (vector "render"))` | end a timer. |
| `js-console-time-log` | `(value)` | `()` | `(js-console-time-log (vector "render" "extra"))` | log a timer checkpoint. |
| `js-console-time-stamp` | `(value)` | `()` | `(js-console-time-stamp (vector "render"))` | add a timeline marker. |

## Chapter 7 — Stack and Profiling

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| `js-console-trace` | `(value)` | `()` | `(js-console-trace (vector "trace"))` | write a stack trace. |
| `js-console-profile` | `(value)` | `()` | `(js-console-profile (vector "profile"))` | start a profiling session. |
| `js-console-profile-end` | `(value)` | `()` | `(js-console-profile-end (vector "profile"))` | end a profiling session. |
| `js-console-exception` | `(value)` | `()` | `(js-console-exception (vector "deprecated"))` | deprecated alias for error output. |

## Chapter 8 — Coverage Checklist

- This document covers the raw Console bindings in `ffi/console.ffi`.
- The wrapper library provides the checked `console-*` API.
