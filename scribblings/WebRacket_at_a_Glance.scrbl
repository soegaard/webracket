#lang scribble/manual

@title{WebRacket at a Glance}

This is a high-level overview of WebRacket.

@section{What is WebRacket?}

WebRacket is a @bold{Racket-to-WebAssembly compiler} designed to run practical
Racket programs directly in the browser.

It compiles a substantial subset of Racket to
@hyperlink["https://webassembly.org/"]{WebAssembly}, which you can think of as
portable machine code for the browser.

@hyperlink["https://webassembly.org/"]{WebAssembly} runs in a virtual machine
that is separate from the JavaScript engine. Because of this separation, a
foreign-function interface (FFI) is required when WebRacket code needs browser
features such as DOM operations, timers, and graphics APIs.

In practice, WebRacket:

@itemlist[
  @item{Targets modern browsers such as Chrome, Firefox, and Safari.}
  @item{Uses a custom runtime designed for WebAssembly.}
  @item{Keeps the boundary to JavaScript explicit via the FFI.}
]

@section{What WebRacket is not}

WebRacket is not a drop-in replacement for Racket on the Racket VM.
Some features are missing or behave differently because of browser
and WebAssembly constraints.

WebRacket aims for a useful and predictable subset, with differences reduced
over time.

@section{Execution Model}

WebRacket programs execute as single-threaded WebAssembly modules embedded in
a browser environment. This model drives how computation, memory management,
and host interaction are structured.

@itemlist[
  @item{@bold{Single-threaded execution}: no parallel threads or shared-memory concurrency.}
  @item{@bold{Custom runtime}: implements core Racket value representations and primitive operations.}
  @item{@bold{WebAssembly GC types}: uses Wasm GC for heap-allocated values.}
]

@section{Language Coverage}

WebRacket focuses on practical web programs. It supports many core forms and
data structures. Some areas are still a work in progress; see
@hyperlink["Implemented_Primitives.html"]{Implemented Primitives} for the
current primitive list.

Supported areas include:

@itemlist[
  @item{Fixnums and flonums.}
  @item{Strings and byte strings.}
  @item{Symbols and keywords.}
  @item{Pairs, vectors, boxes, and mutable hash tables.}
  @item{The standard expander, including @racket[for] and @racket[match].}
  @item{Tail calls, multiple values, and upward exceptions.}
  @item{Structures with super-structures and properties.}
  @item{Large parts of @tt{racket/base}.}
]

Current limitations include:

@itemlist[
  @item{Single-threaded execution only.}
  @item{Module support is still in progress.}
  @item{No support for continuations and continuation marks.}
  @item{Some numeric and control features are not yet implemented.}
  @item{Keyword-argument behavior is evolving;
        see the Libraries chapter for @racket[define/key].}
]

@section{JavaScript FFI}

WebAssembly programs can't directly call JavaScript functions.
A foreign-function interface (FFI) is needed as a bridge between the two languages.

Only very simple values, such as integers and floating-point numbers, can be
passed directly when a JavaScript function is called from WebAssembly.
To pass other values, WebAssembly needs to encode the values as bytes
and store them in a shared byte array. On the JavaScript side, the bytes
are decoded before the function is invoked. This process is called FASL
encoding and decoding.

The process is similar when JavaScript calls a WebAssembly function.

WebRacket attempts to automate this process as much as possible,
but you still need to be aware that not all values can cross the bridge.

For common browser APIs, WebRacket has efficient static bindings.
This covers the DOM, Canvas, and libraries such as MathJax,
Xterm.js, and JSXGraph.

If you need other browser APIs, there is a dynamic FFI available.
