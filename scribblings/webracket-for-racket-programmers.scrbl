#lang scribble/manual

@title{WebRacket for Racket Programmers}

This chapter describes WebRacket from the perspective of a programmer
familiar with Racket.

@section{Overview}

@margin-note{It is also possible to run WebRacket programs in Node.js.}
WebRacket lets you write programs in Racket and run them in the browser.
It uses Racket as a frontend language, but executes programs on a custom
runtime compiled to WebAssembly.

@itemlist[
  @item{Programs are written in Racket syntax.}
  @item{They are expanded using the Racket expander.}
  @item{They are compiled to WebAssembly and run in the browser.}
]

The main differences from Racket arise from the runtime environment and
the constraints of WebAssembly and the browser.

@section{The Compilation Pipeline}

A WebRacket program is processed in several stages:

@itemlist[
  @item{The source file is read into syntax objects.}
  @item{The program is expanded using the Racket expander.}
  @item{The expanded program is compiled to WebAssembly text (WAT).}
  @item{The WAT is assembled into a WebAssembly module (@tt{.wasm}).}
  @item{A small host file (HTML or JavaScript) is generated.}
]

The use of the Racket expander means that macros and special
forms work like they do in Racket.


@section{What is the same}

Many core aspects of Racket are preserved:

@itemlist[
  @item{The Racket expander is used, so macros work.}
  @item{Core forms such as @racket[lambda], @racket[if], @racket[let],
        @racket[match], and @racket[for] are supported.}
  @item{Tail calls and multiple values are supported.}
  @item{Data structures such as pairs, vectors, strings, symbols,
        and mutable hash tables are available.}
  @item{Structures work. THis includes super-structures and structure properties. }  
]

In practice, many programs written using @tt{racket/base} can be adapted
to WebRacket with relatively small changes.

Since the Racket expander is used at compilation time, anything
compile time related (phase 1 and greater) is using normal Racket.
This means you can use @racket[syntax-parse] for macros.


@section{What is different}

WebRacket programs do not run on Racket’s usual runtime system.
Instead, they are compiled to WebAssembly and executed on a custom
runtime inside the browser.


This leads to several differences:

@itemlist[
  @item{@bold{No modules (yet):}
        Programs are written as top-level code. Module support is
        under development.}

  @item{@bold{No continuations:}
        Features such as @racket[call/cc] and continuation marks
        are not available.}

  @item{@bold{Single-threaded execution:}
        There are no threads or places. Programs run in a single
        browser thread.}

  @item{@bold{Numeric limitations:}
        Numbers are currently limited to fixnums and flonums.}

  @item{@bold{Keyword arguments:}
        The forms @racket[define] and @racket[lambda] does not support keyword arguments.
        For programs that need keyword arguments, look at the @racket[(include-lib define)] library.}

  @item{@bold{Different I/O model:}
        @margin-note{Programs can print to the JavaScript console.}
        There is no terminal. Programs do not print to a console by default;
        instead, they typically produce visible results in the browser.}
  ]

@section{Programs Without Modules}

WebRacket programs are currently written as a single top-level program.
There is no @racket[module] form, and no separate compilation units.

Instead, the entire program is treated as one sequence of top-level
definitions and expressions.

@subsection{One big top-level}

A WebRacket source file is read and wrapped in a single @racket[begin]
before compilation. For example:

@racketblock[
(define (f x) (+ x 1))
(define (g x) (f (* x 2)))

(g 10)
]

is treated as one top-level program, where all definitions are visible
to each other.

This means:

@itemlist[
  @item{Definitions are processed in order.}
  @item{All bindings live in the same global scope.}
  @item{There is no module-level encapsulation.}
]

For simple programs, this is often sufficient. For larger programs,
it requires a different organization style than in Racket.

@subsection{Using libraries with @racket[include-lib]}

To structure programs and reuse code, WebRacket provides the
@racket[include-lib] form.

@racketblock[
(include-lib web-easy)
(include-lib define)
]

This form inserts the contents of a library into the current program
at compile time.

Conceptually, it is similar to copying the contents of the library into
your file before compilation.

@subsection{Standard libraries}

WebRacket provides a collection of libraries that can be included
using @racket[include-lib]. These libraries provide functionality
that would normally be imported via modules in Racket.

Examples include:

@itemlist[
  @item{@racket[(include-lib web-easy)] for building user interfaces}
  @item{@racket[(include-lib define)] for extended @racket[define]
        and @racket[lambda] forms}
  @item{Libraries for DOM access, events, and browser APIs}
]

Because there is no module system, libraries are designed to work in a
single shared namespace.

@subsection{Program organization}

Without modules, programs are typically organized by:

@itemlist[
  @item{Splitting code into multiple files and including them with
        @racket[include-lib] or similar mechanisms}
  @item{Grouping related definitions together}
  @item{Using naming conventions to avoid conflicts}
]

Although this is different from standard Racket practice, it is often
sufficient for the kinds of interactive programs WebRacket targets.

@subsection{Summary}

In WebRacket:

@itemlist[
  @item{Programs are single top-level units.}
  @item{There is no module system (yet).}
  @item{Libraries are included using @racket[include-lib].}
]

This model keeps the compilation process simple, while still allowing
code reuse through libraries.


@section{The Runtime Model}

WebRacket uses a custom runtime implemented in WebAssembly.

@itemlist[
  @item{Values are represented using WebAssembly GC types.}
  @item{Heap-allocated values such as pairs, strings, and vectors
        are implemented as WebAssembly structures.}
  @item{Closures are explicit runtime objects.}
]


@section{Interacting with the Browser}

WebRacket programs run inside a browser environment.

Interaction with the browser is explicit and happens through:

@itemlist[
  @item{Libraries such as @tt{web-easy} for building user interfaces.}
  @item{Bindings to DOM and Web APIs.}
  @item{The JavaScript foreign-function interface (FFI).}
]

Because WebAssembly is separate from JavaScript, all interaction
between WebAssembly goes through a JavaScript ↔ WebAssembly value
conversion bridge.

@section{Program Structure}

WebRacket programs are typically structured differently from
traditional Racket programs.

Instead of computing a value and exiting, a program usually:

@itemlist[
  @item{constructs a user interface}
  @item{inserts it into the page}
  @item{responds to events over time}
]

Programs are therefore:

@itemlist[
  @item{interactive}
  @item{event-driven}
  @item{long-running}
]

@section{Summary}

For a Racket programmer, WebRacket can be understood as:

@itemlist[
  @item{Racket syntax and macros}
  @item{compiled to WebAssembly}
  @item{running on a custom runtime in the browser}
  @item{with explicit access to browser APIs}
]
