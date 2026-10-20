#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-resize-observer-labels.rkt" "webracket-doc")
                     browser/document
                     browser/element)
          "webracket-scribble-utils.rkt")

@title{Library: @racketid[resize-observer]}
@declare-exporting[(lib "scribblings/lib-resize-observer-labels.rkt" "webracket-doc")]

@defmodule[resize-observer]

The @racketmodname[resize-observer] module wraps the browser
@tt{ResizeObserver} API. It is useful when code should react to size
changes in an element without polling layout continuously.

@(mdn-bar "Resize Observer API"
          "https://developer.mozilla.org/en-US/docs/Web/API/Resize_Observer_API")

@section{Quick Start}

@racketblock[
(require browser/document
         browser/dom
         resize-observer)

(define target
  (document-query-selector "#panel"))

(define observer
  (make-resize-observer
   (lambda (entries _observer)
     (define first-entry (vector-ref entries 0))
     (js-log "panel resized"
             (resize-observer-entry-content-rect first-entry)))))

(resize-observer-observe! observer target)
]

@section{API}

@defproc[(resize-observer-supported?) boolean?]{
Returns @racket[#t] when the browser exposes @tt{ResizeObserver}.
}

@defproc[(resize-observer [raw external/raw]) resize-observer?]{
Wraps a raw browser @tt{ResizeObserver} value.
}

@defproc[(resize-observer? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a wrapped observer.
}

@defproc[(resize-observer-raw [value resize-observer?]) external/raw]{
Returns the wrapped raw browser observer.
}

@defproc[(make-resize-observer-options [box (or/c #f string? symbol?)]) resize-observer-options?]{
Creates options for @tt{ResizeObserver.observe()}.
}

@defproc[(make-resize-observer [callback (or/c procedure? external?)]) resize-observer?]{
Creates a wrapped browser observer. The callback receives a vector of
@racket[resize-observer-entry?] values and the wrapped observer. The native
observer owns the callback bridge while it remains connected; this library
does not keep a permanent callback cache, and @racket[resize-observer-disconnect!]
ends observation.
}

@defproc[(resize-observer-observe! [observer resize-observer?]
                                   [target (or/c dom-element? external?)]
                                   [options (or/c #f resize-observer-options?) #f])
         void?]{
Starts observing @racket[target].
}

@defproc[(resize-observer-unobserve! [observer resize-observer?]
                                     [target (or/c dom-element? external?)])
         void?]{
Stops observing @racket[target].
}

@defproc[(resize-observer-disconnect! [observer resize-observer?]) void?]{
Disconnects the observer from all targets.
}

@defstruct[resize-observer-entry ([raw external/raw])]{
Wraps a browser @tt{ResizeObserverEntry} value.
}

@defproc[(resize-observer-entry-target [entry resize-observer-entry?]) dom-element?]{
Returns the observed target element.
}

@defproc[(resize-observer-entry-content-rect [entry resize-observer-entry?]) dom-rect?]{
Returns the current content rectangle for the entry.
}
