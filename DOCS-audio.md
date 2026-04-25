# Reference: `audio.ffi`

## Chapter 1 - Introduction

This document describes the browser Web Audio bindings exported by `ffi/audio.ffi` in WebRacket.

The surface is organized around the core Web Audio building blocks:
- `AudioContext`
- `AudioNode`
- `AudioParam`
- `AudioBuffer`
- common node types such as gain, oscillator, analyser, filter, and buffer source nodes

Assumption in examples: the program is compiled with `--ffi audio`.

## Chapter 2 - Conventions

### 2.1 Type Legend

| Type | Meaning |
|---|---|
| `(extern)` | External JavaScript object/reference used for input. |
| `(extern/raw)` | Raw JS value/object reference with no null/undefined mapping. |
| `(value)` | Value that crosses the normal JS-to-WebRacket bridge. |
| `(string)` | JavaScript string mapped to a Racket string. |
| `(u32)` | Unsigned 32-bit integer. |
| `(f64)` | Double-precision floating-point number. |
| `()` | No result / void. |

### 2.2 Optional Argument Conventions

- When a browser method has optional trailing arguments, the raw binding accepts `(void)` for omitted values.
- Promise-returning browser methods are exposed as `(extern/raw)` so the wrapper can decide how to handle them later.

## Chapter 3 - Audio Context

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| `js-audio-context-new` | `()` | `(extern/raw)` | `(js-audio-context-new)` | create a new audio context. |
| `js-audio-context-close` | `(extern)` | `(extern/raw)` | `(js-audio-context-close ctx)` | close a context. |
| `js-audio-context-resume` | `(extern)` | `(extern/raw)` | `(js-audio-context-resume ctx)` | resume a suspended context. |
| `js-audio-context-suspend` | `(extern)` | `(extern/raw)` | `(js-audio-context-suspend ctx)` | suspend a running context. |
| `js-audio-context-state` | `(extern)` | `(string)` | `(js-audio-context-state ctx)` | inspect the current state. |
| `js-audio-context-current-time` | `(extern)` | `(f64)` | `(js-audio-context-current-time ctx)` | inspect the audio clock. |

## Chapter 4 - Nodes and Params

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| `js-audio-node-connect` | `(extern value value value)` | `(extern/raw)` | `(js-audio-node-connect node dest (void) (void))` | connect nodes or params. |
| `js-audio-node-disconnect` | `(extern value value value)` | `()` | `(js-audio-node-disconnect node (void) (void) (void))` | remove graph connections. |
| `js-audio-param-value` | `(extern)` | `(f64)` | `(js-audio-param-value param)` | read a parameter value. |
| `js-audio-param-set-value!` | `(extern value)` | `()` | `(js-audio-param-set-value! param 0.5)` | set a parameter immediately. |

## Chapter 5 - Buffer and Playback

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| `js-audio-context-create-buffer` | `(extern value value value)` | `(extern/raw)` | `(js-audio-context-create-buffer ctx 2 44100 48000)` | create an audio buffer. |
| `js-audio-buffer-length` | `(extern)` | `(u32)` | `(js-audio-buffer-length buffer)` | read buffer length. |
| `js-audio-buffer-get-channel-data` | `(extern value)` | `(extern/raw)` | `(js-audio-buffer-get-channel-data buffer 0)` | read channel samples. |
| `js-audio-oscillator-node-start!` | `(extern value)` | `()` | `(js-audio-oscillator-node-start! osc (void))` | start oscillator playback. |
| `js-audio-audio-buffer-source-node-start!` | `(extern value value value)` | `()` | `(js-audio-audio-buffer-source-node-start! src (void) (void) (void))` | start a buffer source. |

## Chapter 6 - Coverage Checklist

- This document covers the raw Audio bindings in `ffi/audio.ffi`.
- It is intentionally compact and focuses on the first browser-backed Web Audio surface.
