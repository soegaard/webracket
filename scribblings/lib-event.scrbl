#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-event-labels.rkt" "webracket")))

@title{Library: @racketid[event]}
@declare-exporting[(lib "scribblings/lib-event-labels.rkt" "webracket")]

@(how-to-require include-lib event (lib "libs/event.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[event] library is the checked wrapper for browser events.

Events are the messages the browser sends when something happens:
clicks, key presses, form submissions, pointer movement, and many
other changes in the page.

Use @racket[event] when you want to:

@itemlist[
  @item{check whether a value is a browser event}
  @item{read the event type string}
  @item{stop the browser's default action}
  @item{branch on event kind inside a handler}
]

The @racket[event] library wraps DOM event predicates and common event
accessors.

@section{Event Quick Start}

Start by including the library and writing a tiny handler that inspects
the event before acting on it.

@racketblock[
(code:comment "Include the checked event wrapper library.")
(include-lib event)

(code:comment "Define a small handler that reads the event type.")
(define (handle-event evt)
  (define kind
    (event-type evt))
  kind)

(code:comment "The handler can be used from element or window event hooks.")
(void handle-event)
]

This quick start shows the basic pattern: include the library, accept
an event value, and inspect it with a checked helper.

@section{Event Example}

This example shows the usual browser event workflow: recognize the
event kind, stop the default action when needed, and continue with your
own logic.

@racketblock[
(code:comment "Include the wrapper library.")
(include-lib event)

(code:comment "Handle a browser event in a small, readable function.")
(define (handle-click evt)
  (code:comment "Read the browser event type.")
  (define kind
    (event-type evt))

  (code:comment "Stop the browser from doing its default action.")
  (prevent-default! evt)

  (code:comment "Return the event type so the caller can log it.")
  kind)

(code:comment "A caller can use the predicate to distinguish mouse events.")
(define (handle-any-event evt)
  (when (mouse-event? evt)
    (handle-click evt)))

(void handle-click handle-any-event)
]

When you are writing event code, the most useful entry points are
usually @racket[event?], @racket[mouse-event?], @racket[event-type],
and @racket[prevent-default!].

@defproc[(event? [x any/c]) boolean?]{
@(mdn-bar "Event"
          "https://developer.mozilla.org/en-US/docs/Web/API/Event")
Returns @racket[#t] when @racket[x] is a DOM Event value.
}

@defproc[(mouse-event? [x any/c]) boolean?]{
@(mdn-bar "MouseEvent"
          "https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent")
Returns @racket[#t] when @racket[x] is a DOM MouseEvent value.
}

@defproc[(keyboard-event? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a DOM KeyboardEvent value.
}

@defproc[(pointer-event? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a DOM PointerEvent value.
}

@defproc[(focus-event? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a DOM FocusEvent value.
}

@defproc[(input-event? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a DOM InputEvent value.
}

@defproc[(submit-event? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a DOM SubmitEvent value.
}

@defproc[(touch-event? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a DOM TouchEvent value.
}

@defproc[(wheel-event? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a DOM WheelEvent value.
}

@defproc[(event-type [evt any/c]) string?]{
@(mdn-bar "Event: type property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Event/type")
Returns the browser event type string.
}

@defproc[(prevent-default! [evt any/c]) void?]{
@(mdn-bar "Event: preventDefault() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault")
Prevents the default action for an event.
}
