#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[event]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib event (lib "libs/event.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[event] library wraps DOM event predicates and common event
accessors.

@defproc[(event? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a DOM Event value.
}

@defproc[(mouse-event? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a DOM MouseEvent value.
}

@defproc[(event-type [evt any/c]) string?]{
Returns the browser event type string.
}

@defproc[(prevent-default! [evt any/c]) void?]{
Prevents the default action for an event.
}
