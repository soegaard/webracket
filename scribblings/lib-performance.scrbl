#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[performance]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib performance (lib "libs/performance.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[performance] library is a checked wrapper around the
browser's high-resolution timing API.

Use @racket[performance] when you want to measure how long something
took or compare two points in time on the page.

Typical uses include:

@itemlist[
  @item{timing a bit of Racket code}
  @item{measuring animation or rendering work}
  @item{recording a start and end time for a browser action}
]

The @racket[performance] library exposes a checked high-resolution
timestamp helper for the browser Performance API.

@section{Performance Quick Start}

Start by calling the timestamp helper before and after the work you
want to measure.

@racketblock[
(code:comment "Include the checked performance wrapper library.")
(include-lib performance)

(code:comment "Record the starting timestamp.")
(define start
  (performance-now))

(code:comment "Record the ending timestamp after the work you care about.")
(define end
  (performance-now))

(code:comment "Subtract to get a duration in milliseconds.")
(- end start)
]

The quick start shows the basic pattern: capture two timestamps and
subtract them.

@section{Performance Example}

This example shows how to time a small chunk of work. In a real
program, you would wrap the work you care about between the two timing
calls.

@racketblock[
(code:comment "Include the performance wrapper library.")
(include-lib performance)

(code:comment "Start the timer.")
(define start
  (performance-now))

(code:comment "Do a little bit of work.")
(define total
  (for/sum ([n (in-range 1000)])
    n))

(code:comment "Stop the timer and compute the elapsed time.")
(define elapsed
  (- (performance-now) start))

(void total elapsed)
]

When you only need one helper, @racket[performance-now] is the main
entry point.

@defproc[(performance-now) real?]{
@(mdn-bar "Performance: now() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Performance/now")
Returns a monotonic browser timestamp suitable for measuring elapsed
time.
}
