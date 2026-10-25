#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-js-promise-labels.rkt" "webracket-doc"))
          "webracket-scribble-utils.rkt")

@title{Library: @racketmodname[js/promise]}
@declare-exporting[(lib "scribblings/lib-js-promise-labels.rkt" "webracket-doc")]

@defmodule[js/promise]

The @racketmodname[js/promise] module wraps JavaScript Promise values and
provides a small set of helper functions for attaching fulfillment,
rejection, and cleanup handlers from WebRacket code.

Use @racketmodname[js/promise] when you want to:

@itemlist[
  @item{recognize wrapped JavaScript Promise-like values}
  @item{attach @tt{then}, @tt{catch}, and @tt{finally} handlers without reflective string method calls}
  @item{work with Promise-returning APIs such as Clipboard, Fetch, and media methods}
]

@section{Quick Start}

@racketblock[
(require js/promise)

(define ready
  (js-promise-resolved "done"))

(js-promise-then
 ready
 (lambda (value)
   (js-log value)
   (void)))
]

The most common pattern is:

@itemlist[
  @item{get a @racket[js-promise?] value back from a browser API}
  @item{attach a success handler with @racket[js-promise-then]}
  @item{optionally attach error cleanup with @racket[js-promise-catch] or @racket[js-promise-finally]}
]

@section{Callback lifetime}

Each handler attachment creates one native callback bridge. The JavaScript
Promise retains that bridge until the corresponding handler settles; the
library has no permanent global callback cache. When a handler performs
side effects only, return @racket[(void)] so the browser callback does not
need to marshal an unintended Racket result.

@defstruct[js-promise ([raw external/raw])]{
Wraps a JavaScript Promise-like value.
}

@defproc[(js-promise-thenable? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a wrapped Promise-like value.
}

@defproc[(js-promise-wrap [raw external/raw]) js-promise?]{
Wraps a Promise-like browser value.
}

@defproc[(js-promise-unwrap [promise js-promise?]) external/raw]{
Returns the wrapped raw Promise-like value.
}

@defproc[(js-promise-map-fulfilled [promise js-promise?]
                                   [transform procedure?])
         js-promise?]{
Returns a wrapper that applies @racket[transform] to a fulfilled value before a
Racket procedure supplied to @racket[js-promise-then] receives it. The raw
browser Promise and its rejection behavior are unchanged. JavaScript handlers
passed as @racket[external?] values continue to receive the raw value.
}

@defproc[(js-promise-resolved [value any/c]) js-promise?]{
Creates an already-resolved JavaScript Promise.
}

@defproc[(js-promise-rejected [value any/c]) js-promise?]{
Creates an already-rejected JavaScript Promise.
}

@defproc[(js-promise-then [promise js-promise?]
                          [on-fulfilled (or/c procedure? external?)]
                          [on-rejected (or/c #f procedure? external?) #f])
         js-promise?]{
Attaches fulfillment and optional rejection handlers and returns the chained
JavaScript Promise. The optional rejection handler defaults to @racket[#f].
}

@defproc[(js-promise-catch [promise js-promise?]
                           [on-rejected (or/c procedure? external?)])
         js-promise?]{
Attaches a rejection handler and returns the chained JavaScript Promise.
}

@defproc[(js-promise-finally [promise js-promise?]
                             [on-finally (or/c procedure? external?)])
         js-promise?]{
Attaches a cleanup handler and returns the chained JavaScript Promise.
}
