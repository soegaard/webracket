#lang scribble/manual

@(require "browser-api-docs.rkt"
          (for-label (lib "scribblings/console-labels.rkt" "webracket")))

@section{Console}
@declare-exporting[(lib "scribblings/console-labels.rkt" "webracket")]

The browser @racket[console] object exposes the standard developer console
API available in web pages and web workers.

This page documents the raw @racket[js-console-*] bindings from
@tt{ffi/console.ffi}. The checked @racket[console-*] wrappers are
documented separately in the @racketid[console] library chapter.

@subsection{Low-Level FFI Surface: Console}

These bindings follow the browser API directly. They are useful when you
want to work with the JavaScript object model as-is, or when you are
building a higher-level abstraction on top of the raw interop layer.

Variadic console methods receive a packed argument list as a single
value, typically a vector.

Methods with an optional label or timer name also use a packed single
value when the label is present.

@subsubsection{Examples}

@racketblock[
(code:comment "A single raw call that logs two values.")
(js-console-log (vector "hello" 42))

(code:comment "A warning message in raw form.")
(js-console-warn (vector "something looks odd"))

(code:comment "A raw counter name is packed into a one-element vector.")
(js-console-count (vector "requests"))

(code:comment "A raw timer name is packed into a one-element vector.")
(js-console-time (vector "render"))

(code:comment "A collapsed group in raw form.")
(js-console-group-collapsed (vector "details"))

(code:comment "A timer checkpoint with extra data.")
(js-console-time-log (vector "render" "phase" "layout"))

(code:comment "A timeline marker name is packed into a one-element vector.")
(js-console-time-stamp (vector "frame"))
]

@(render-ffi-docs "ffi/console.ffi" "console.ffi")
