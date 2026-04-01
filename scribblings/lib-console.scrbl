#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt")

@title{Library: @racketid[console]}

@(how-to-require include-lib console (lib "libs/console.rkt"))
@(compile-option-bar "Compile option: " "--ffi console")

The browser console is a developer tool built into every browser. It
shows messages, warnings, errors, and debug output from a web page.

The @racket[console] library provides the checked @racket[console-*]
wrappers on top of the raw @racket[js-console-*] bindings from
@tt{ffi/console.ffi}.

Use it when you want to write messages, build collapsible groups,
inspect values, or track timers and counters from WebRacket code.

@section{Console Quick Start}

The browser console is a built-in developer panel that shows messages,
warnings, errors, and debug output from a web page. Open the browser
Developer Tools and select the @tt{Console} tab.

@racketblock[
(include-lib console)

(code:comment "Print a message to the console.")
(console-log "hello" 42)

(code:comment "Show a warning message.")
(console-warn "something looks odd")

(code:comment "Start or increment a counter named requests.")
(console-count 'requests)

(code:comment "Start a timer named render.")
(console-time 'render)

(code:comment "This whole block can be pasted into a WebRacket page.")
]

@section{Examples}

@racketblock[
(code:comment "Labels may be symbols or strings.")
(code:comment "Start a counter using a symbol label.")
(console-count 'requests)
(code:comment "Reset the same counter using a string label.")
(console-count-reset "requests")

(code:comment "Group related output together.")
(code:comment "Open a group titled load.")
(console-group "load")
(code:comment "Write a message inside the group.")
(console-log "starting")
(code:comment "Close the current group.")
(console-group-end)

(code:comment "Inspect tabular data and timer checkpoints.")
(code:comment "Show a two-column table.")
(console-table (vector 1 2 3) (vector "id"))
(code:comment "Record a timer checkpoint with extra context.")
(console-time-log 'render "phase" "layout")
]

@section{Console API Reference}

@subsection{Core Logging}

@defproc[(console-log [v any/c] ...) void?]{
Writes a log message to the browser console.

@racketblock[
(console-log "hello" 42)
(console-log "user" 'alice)
]
}

@defproc[(console-info [v any/c] ...) void?]{
Writes an informational message to the browser console.

@racketblock[
(code:comment "Show an informational message.")
(console-info "server ready")
]
}

@defproc[(console-warn [v any/c] ...) void?]{
Writes a warning message to the browser console.
}

@defproc[(console-error [v any/c] ...) void?]{
Writes an error message to the browser console.

@racketblock[
(code:comment "Show an error message.")
(console-error "connection failed")
]
}

@defproc[(console-debug [v any/c] ...) void?]{
Writes a debug message to the browser console.
}

@defproc[(console-assert [condition any/c] [v any/c] ...) void?]{
Writes an assertion failure message when @racket[condition] is false.

@racketblock[
(code:comment "Write nothing because the condition is true.")
(console-assert (< 1 2) "math still works")
(code:comment "Write an assertion failure message because the condition is false.")
(console-assert #f "something went wrong")
]
}

@subsection{Inspection}

@defproc[(console-clear) void?]{
Clears the console output.
}

@defproc[(console-dir [v any/c] [options any/c #f]) void?]{
Inspects a value in the console.

@racketblock[
(define opts (js-eval "({depth: 2})"))
(console-dir "widget" opts)
]
}

@defproc[(console-dirxml [v any/c] ...) void?]{
Inspects XML/HTML output in the console.

@racketblock[
(code:comment "Inspect an XML or HTML value in the console.")
(console-dirxml (vector 'p "hello"))
]
}

@defproc[(console-table [data any/c] [columns any/c #f]) void?]{
Displays tabular data in the console.

@racketblock[
(code:comment "Show a one-column table with an explicit column label.")
(console-table (vector 1 2 3) (vector "id"))
(code:comment "Show a two-column table from row data.")
(console-table (vector (vector "a" 1) (vector "b" 2)) (vector "name" "count"))
]
}

@subsection{Groups}

@defproc[(console-group [v any/c] ...) void?]{
Starts a new console group.
}

@defproc[(console-group-collapsed [v any/c] ...) void?]{
Starts a collapsed console group.

@racketblock[
(code:comment "Start a collapsed group for related output.")
(console-group-collapsed "details")
]
}

@defproc[(console-group-end) void?]{
Ends the current console group.
}

@subsection{Counters and Timers}

@defproc[(console-count [label (or/c #f string? symbol?) #f]) void?]{
Increments and displays a counter.

If @racket[label] is provided, symbols and strings are accepted and the
wrapper converts symbols to strings before calling the browser API.
}

@defproc[(console-count-reset [label (or/c #f string? symbol?) #f]) void?]{
Resets a counter.
}

@defproc[(console-time [label (or/c #f string? symbol?) #f]) void?]{
Starts a timer.

If @racket[label] is provided, symbols and strings are accepted and the
wrapper converts symbols to strings before calling the browser API.
}

@defproc[(console-time-end [label (or/c #f string? symbol?) #f]) void?]{
Ends a timer.

If @racket[label] is provided, symbols and strings are accepted and the
wrapper converts symbols to strings before calling the browser API.
}

@defproc[(console-time-log [label (or/c #f string? symbol?) #f] [v any/c] ...) void?]{
Logs the current value of a timer and any additional data.

@racketblock[
(console-time-log 'render "phase" "layout")
]
}

@defproc[(console-time-stamp [label (or/c #f string? symbol?) #f]) void?]{
Adds a timeline marker.

If @racket[label] is provided, symbols and strings are accepted and the
wrapper converts symbols to strings before calling the browser API.

@racketblock[
(code:comment "Add a timeline marker named startup.")
(console-time-stamp 'startup)
]
}

@subsection{Tracing and Profiling}

@defproc[(console-trace [v any/c] ...) void?]{
Writes a stack trace to the console.
}

@defproc[(console-profile [label (or/c #f string? symbol?) #f]) void?]{
Starts a browser profiler session.

If @racket[label] is provided, symbols and strings are accepted and the
wrapper converts symbols to strings before calling the browser API.
}

@defproc[(console-profile-end [label (or/c #f string? symbol?) #f]) void?]{
Ends a browser profiler session.

If @racket[label] is provided, symbols and strings are accepted and the
wrapper converts symbols to strings before calling the browser API.
}

@defproc[(console-exception [v any/c] ...) void?]{
Deprecated alias for @racket[console-error]. Prefer @racket[console-error]
for new code.

@racketblock[
(console-exception "deprecated")
]
}
