#lang scribble/manual

@(require scribble/manual
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

@section{Window Quick Start}

Start by including the library, getting the current document, and
calling a simple page-level helper.

@racketblock[
(code:comment "Include the checked Window wrapper library.")
(include-lib window)

(code:comment "Get the current document from the browser window.")
(define doc
  (window-document))

(code:comment "Show a simple confirmation dialog.")
(define ok?
  (window-confirm "Continue with this action?"))

(code:comment "If the user agrees, scroll the page a little.")
(when ok?
  (window-scroll-to 0 240 (void)))
]

The quick start shows the three most common ideas:
include the wrapper, reach the current document, and call a browser
helper that affects the page.

@section{Window Example}

This example shows how to use the current document and schedule work
to happen a little later. The timer is useful when you want to defer a
page update without blocking the current event handler.

@racketblock[
(code:comment "Include the wrapper library at the top level.")
(include-lib window)

(code:comment "Get the current document so we can inspect the page.")
(define doc
  (window-document))

(code:comment "Keep the document value around for later DOM work.")
(void doc)

(code:comment "Run a small update after a short delay.")
(window-set-timeout
  (lambda ()
    (displayln "Hello from the browser window")))
]

If you are only reading the page and not changing it, the most useful
window entry points are usually @racket[window-document],
@racket[window-confirm], @racket[window-set-timeout], and
@racket[window-scroll-to].

@section{Window API}

@defproc[(window) external/raw]{
@(mdn-bar "Window: window property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/window")
Returns the current Window object.
}

@defproc[(window-document) external/raw]{
@(mdn-bar "Window: document property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/document")
Returns the current document object.
}

@defproc[(window-set-name! [name string?]) void?]{
@(mdn-bar "Window: name property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/name")
Sets the window name.
}

@defproc[(window-open [url string?]
                      [target any/c (void)]
                      [features any/c (void)]
                      [replace any/c (void)])
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

@defproc[(window-scroll-to [x real?] [y real?] [options any/c (void)]) void?]{
@(mdn-bar "Window: scrollTo() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollTo")
Scrolls the window to an absolute position.
}

@defproc[(window-set-timeout [callback external?]) exact-nonnegative-integer?]{
@(mdn-bar "Window: setTimeout() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/setTimeout")
Schedules a one-shot timer callback.
}
