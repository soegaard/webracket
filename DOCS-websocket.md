# Reference: `websocket.ffi`

## Chapter 1 — Introduction

This document describes the browser WebSocket bindings exported by `ffi/websocket.ffi` in WebRacket.

This FFI exposes the core WebSocket surface:
- construct a socket
- send data
- close the connection
- read the read-only state/properties that browsers expose on `WebSocket`

Assumption in examples: the program is compiled with `--ffi websocket`.

All function names are linked to MDN pages for the corresponding Web API member.

### Table of Contents

- [Chapter 1 — Introduction](#chapter-1--introduction)
- [Chapter 2 — Conventions](#chapter-2--conventions)
- [2.1 Type Legend](#21-type-legend)
- [2.2 Optional Argument Conventions](#22-optional-argument-conventions)
- [2.3 Common Setup Helpers](#23-common-setup-helpers)
- [2.4 Environment and Availability Notes](#24-environment-and-availability-notes)
- [Chapter 3 — Construction](#chapter-3--construction)
- [Chapter 4 — Properties](#chapter-4--properties)
- [Chapter 5 — Methods](#chapter-5--methods)
- [Chapter 6 — Mini Workflows](#chapter-6--mini-workflows)
- [Build, Send, and Inspect State](#build-send-and-inspect-state)
- [Mocked Constructor for Tests](#mocked-constructor-for-tests)
- [Chapter 7 — Coverage Checklist](#chapter-7--coverage-checklist)

## Chapter 2 — Conventions

### 2.1 Type Legend

| Type | Meaning |
|---|---|
| `(extern)` | External JavaScript object/reference (typically used for input parameters). |
| `(extern/raw)` | Raw JavaScript return value/object reference (no `null`/`undefined` mapping). |
| `(value)` | WebRacket value converted through the FFI value bridge. |
| `(string)` | JavaScript string mapped to WebRacket string. |
| `(u32)` | Unsigned 32-bit integer. Used for `readyState` and `bufferedAmount`. |
| `()` | No arguments (input) or no value / void (output). |

### 2.2 Optional Argument Conventions

- `js-websocket-new` takes a URL and an optional `protocols` argument.
- Pass `(void)` to use the browser default of no subprotocols.
- `js-websocket-close` follows the browser API shape where `code` and `reason` are optional and positional.
- Pass `(void)` for trailing optional arguments you want to omit.

### 2.3 Common Setup Helpers

```racket
(define ws (js-websocket-new "wss://example.invalid/socket" (vector "chat")))
(define mock-ws (js-eval "globalThis.WebSocket"))
(define send-data "hello")
```

### 2.4 Environment and Availability Notes

- These bindings are browser-oriented and assume a `WebSocket` implementation is available.
- In browsers that do not expose `WebSocket`, construction will fail.
- In tests, it is often easier to install a mock constructor on `globalThis.WebSocket` before calling `js-websocket-new`.

## Chapter 3 — Construction

WebSocket API: [WebSocket](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-websocket-new`](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/WebSocket) | `(string value)` | `(extern/raw)` | `(js-websocket-new "wss://example.invalid/socket" (vector "chat"))` | create a new WebSocket connection. |

## Chapter 4 — Properties

WebSocket API: [WebSocket](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-websocket-url`](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/url) | `(extern)` | `(string)` | `(js-websocket-url ws)` | read the connection URL. |
| [`js-websocket-ready-state`](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/readyState) | `(extern)` | `(u32)` | `(js-websocket-ready-state ws)` | read the current connection state. |
| [`js-websocket-buffered-amount`](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/bufferedAmount) | `(extern)` | `(u32)` | `(js-websocket-buffered-amount ws)` | read the number of queued bytes waiting to be transmitted. |
| [`js-websocket-protocol`](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/protocol) | `(extern)` | `(string)` | `(js-websocket-protocol ws)` | read the negotiated subprotocol, if any. |
| [`js-websocket-extensions`](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/extensions) | `(extern)` | `(string)` | `(js-websocket-extensions ws)` | read the negotiated extensions string. |

## Chapter 5 — Methods

WebSocket API: [WebSocket](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket)

| Function | Input types | Output type | Example | Use when |
|---|---|---|---|---|
| [`js-websocket-send`](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send) | `(extern value)` | `()` | `(js-websocket-send ws "hello")` | send a message through the socket. |
| [`js-websocket-close`](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close) | `(extern value value)` | `()` | `(js-websocket-close ws 1000 "done")` | close the connection, optionally with a code and reason. |

## Chapter 6 — Mini Workflows

### Build, Send, and Inspect State

```racket
(define ws (js-websocket-new "wss://example.invalid/socket" (vector "chat")))
(js-websocket-send ws "hello")
(js-websocket-close ws 1000 "done")
(list (js-websocket-url ws)
      (js-websocket-ready-state ws)
      (js-websocket-buffered-amount ws)
      (js-websocket-protocol ws)
      (js-websocket-extensions ws))
```

### Mocked Constructor for Tests

```racket
(js-eval
 "globalThis.WebSocket = class WebSocket {
    constructor(url, protocols) {
      this.url = url;
      this.protocols = protocols;
      this.readyState = 1;
      this.bufferedAmount = 7;
      this.protocol = 'chat';
      this.extensions = 'permessage-deflate';
    }
    send(data) { this.sent = data; }
    close(code, reason) { this.closed = [code, reason]; }
  };")
```

## Chapter 7 — Coverage Checklist

- This document covers **8** functions from `ffi/websocket.ffi`.
- Total documented functions: **8**
- `construction`: 1 function
- `properties`: 5 functions
- `methods`: 2 functions
