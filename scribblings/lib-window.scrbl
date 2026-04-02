#lang scribble/manual

@(require scribble/manual
          (for-label (only-in racket/base struct))
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[window]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib window (lib "libs/window.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[window] library is the easiest way to talk to the browser
from WebRacket when you need the page itself, not just the current DOM
tree.

If you are new to browser programming, think of the Window as the
browser tab or page container. It gives you access to the current
document, browser dialogs, timers, scrolling, resizing, and a few other
page-level controls.

Use @racket[window] when you want to:

@itemlist[
  @item{find the current document}
  @item{show an alert, confirm, or prompt dialog}
  @item{schedule code to run later}
  @item{scroll or resize the page}
]

The @racket[window] library provides checked wrappers for the browser
Window object, timers, dialogs, scrolling, and navigation helpers.
The current Window object, document, and location are wrapped in small
checked structs so the API stays Rackety instead of exposing raw browser
objects at the top level.

@section{Window Values}

@defstruct[window ([raw external/raw])]{
Wraps a browser Window object.
}

@defproc[(window-raw [win window?]) external/raw]{
Returns the wrapped browser Window object.
}

@defproc[(Window) window?]{
@(mdn-bar "Window"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window")
Returns the current browser Window object wrapped in a checked struct.
}

@defproc[(window-self) window?]{
@(mdn-bar "Window: self property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/self")
Returns the current browser Window object via @racket[self], wrapped in
the checked struct.
}

@defstruct[window-document-info ([raw external/raw])]{
Wraps the current document object returned by @racket[window-document].
}

@defproc[(window-document-info-raw [doc window-document-info?])
         external/raw]{
Returns the wrapped document object.
}

@defstruct[window-location-info ([raw external/raw])]{
Wraps the current location object returned by @racket[window-location].
}

@defproc[(window-location-info-raw [loc window-location-info?])
         external/raw]{
Returns the wrapped location object.
}

@section{Window Quick Start}

Start by including the library, getting the current document, and
calling a simple page-level helper.

@racketblock[
(code:comment "Include the checked Window wrapper library.")
(include-lib window)

(code:comment "Capture the current Window object as a checked wrapper.")
(define win
  (Window))

(code:comment "Get the current document from the browser window.")
(define doc
  (window-document))

(code:comment "Show a simple confirmation dialog.")
(define ok?
  (window-confirm "Continue with this action?"))

(code:comment "If the user agrees, scroll the page a little.")
(when ok?
  (window-scroll-to 0 240))
]

The quick start shows the three most common ideas:
include the wrapper, capture the current Window object, and call a
browser helper that affects the page. After this block, @racket[win] is
the wrapped current Window object and @racket[doc] is the current
document.

@section{Window Example}

This example shows how to use the current document and schedule work
to happen a little later. The timer is useful when you want to defer a
page update without blocking the current event handler.

@racketblock[
(code:comment "Include the wrapper library at the top level.")
(include-lib window)

(code:comment "Capture the current browser Window object.")
(define win
  (Window))

(code:comment "Get the current document so we can inspect the page.")
(define doc
  (window-document))

(code:comment "Keep the wrapped document value around for later DOM work.")

(code:comment "Run a small update after a short delay.")
(window-set-timeout
  (lambda ()
    (displayln "Hello from the browser window")))
]

If you are only reading the page and not changing it, the most useful
window entry points are usually @racket[Window], @racket[window-document],
@racket[window-confirm], @racket[window-set-timeout], and
@racket[window-scroll-to].

For optional arguments, @racket[#f] means omitted. If you need a
literal @racket[#f] value, pass a thunk such as @racket[(lambda () #f)].

@section{Window API}

@defproc[(window-document) window-document-info?]{
@(mdn-bar "Window: document property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/document")
Returns the current document object wrapped in a checked struct.
}

@defproc[(window-location) window-location-info?]{
@(mdn-bar "Window: location property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/location")
Returns the current location object wrapped in a checked struct.
}

@defproc[(window-set-name! [name string?]) void?]{
@(mdn-bar "Window: name property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/name")
Sets the window name.
}

@defproc[(window-open [url string?]
                      [target any/c #f]
                      [features any/c #f]
                      [replace any/c #f])
         (or/c #f external?)]{
@(mdn-bar "Window: open() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/open")
Opens a new browsing context.
}

@defproc[(window-confirm [message string?]) boolean?]{
@(mdn-bar "Window: confirm() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/confirm")
Shows a confirmation dialog and converts the browser result to a boolean.
}

@defproc[(window-scroll-to [x real?] [y real?] [options any/c #f]) void?]{
@(mdn-bar "Window: scrollTo() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollTo")
Scrolls the window to an absolute position.
}

@defproc[(window-set-timeout [callback external?]) exact-nonnegative-integer?]{
@(mdn-bar "Window: setTimeout() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/setTimeout")
Schedules a one-shot timer callback.
}
