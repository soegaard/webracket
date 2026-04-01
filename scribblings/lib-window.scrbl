#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[window]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib window (lib "libs/window.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[window] library provides checked wrappers for the browser
Window object, timers, dialogs, scrolling, and navigation helpers.

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
