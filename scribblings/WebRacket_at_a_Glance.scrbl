#lang scribble/manual

@title{WebRacket at a Glance}

A high-level overview of what WebRacket is, what it targets, and how the main
pieces fit together.

@section{What WebRacket Is}

WebRacket is a @bold{Racket-to-WebAssembly compiler} designed to run practical
Racket programs directly in modern web browsers.

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

@section{What WebRacket Is Not}

WebRacket is not a drop-in replacement for Racket on the Racket VM.
Some features are missing or differ because of browser and WebAssembly
constraints.

WebRacket aims for a useful and predictable subset, with differences reduced
over time.

@section{Execution Model}

WebRacket programs execute as single-threaded WebAssembly modules embedded in
a browser environment. This model drives how computation, memory management,
and host interaction are structured.

@itemlist[
  @item{@bold{Single-threaded execution}: no parallel threads or shared-memory
        concurrency.}
  @item{@bold{Custom runtime}: implements core Racket value representations and
        primitive operations.}
  @item{@bold{WebAssembly GC types}: uses Wasm GC for heap-allocated values
        where available.}
]

@section{Language Coverage}

WebRacket focuses on practical web programs. It supports many core forms and
data structures, while some areas are still in progress.

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
  @item{Keyword-argument behavior is evolving; see the Libraries chapter for
        current @racket[define/key] and @racket[call/key] support.}
]

@section{JavaScript FFI}

WebRacket integrates with the browser through an explicit JavaScript FFI.
This is how programs use the DOM, Canvas, and libraries such as MathJax,
Xterm.js, and JSXGraph.

The FFI keeps conversions and effects explicit so boundary behavior stays
predictable and debuggable.

@section{Very Brief Compiler Overview}

WebRacket uses a direct-style compiler that translates expanded Racket programs
to WebAssembly through a sequence of code transformations. This structure
