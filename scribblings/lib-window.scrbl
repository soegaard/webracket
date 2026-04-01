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
Returns the current Window object.
}

@defproc[(window-document) external/raw]{
Returns the current document object.
}

@defproc[(window-set-name! [name string?]) void?]{
Sets the window name.
}

@defproc[(window-open [url string?]
                      [target any/c (void)]
                      [features any/c (void)]
                      [replace any/c (void)])
         (or/c #f external?)]{
Opens a new browsing context.
}

@defproc[(window-confirm [message string?]) boolean?]{
Shows a confirmation dialog and converts the browser result to a boolean.
}

@defproc[(window-scroll-to [x real?] [y real?] [options any/c (void)]) void?]{
Scrolls the window to an absolute position.
}

@defproc[(window-set-timeout [callback external?]) exact-nonnegative-integer?]{
Schedules a one-shot timer callback.
}
