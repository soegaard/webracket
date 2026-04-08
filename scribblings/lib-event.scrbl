#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-event-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt"
          )

@title{Library: @racketid[event]}
@declare-exporting[(lib "scribblings/lib-event-labels.rkt" "webracket")]

@(how-to-require include-lib event (lib "libs/event.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[event] library wraps browser events in checked values.

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

The @racket[event] library provides predicates and accessors for common
browser events.

The @racket[message-event?] and @racket[close-event?] helpers cover
browser @racketid[MessageEvent] and @racketid[CloseEvent] values, which
are especially useful for WebSocket handlers.

@section{Event Quick Start}

Start by including the library and writing a tiny handler that inspects
the event before acting on it.

@racketblock[
(code:comment "Include the checked event wrapper library.")
(include-lib event)

(code:comment "Define a small handler that reads the event type.")
(define (handle-event evt)
  (define kind (event-type evt))
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
  (define kind (event-type evt))

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

@defproc[(message-event? [x any/c]) boolean?]{
@(mdn-bar "MessageEvent"
          "https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent")
Returns @racket[#t] when @racket[x] is a DOM MessageEvent value.
}

@defproc[(message-event-data [evt any/c]) any/c]{
@(mdn-bar "MessageEvent: data property"
          "https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/data")
Returns the message payload carried by @racket[evt].
}

@defproc[(message-event-origin [evt any/c]) string?]{
@(mdn-bar "MessageEvent: origin property"
          "https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/origin")
Returns the origin string associated with @racket[evt].
}

@defproc[(message-event-last-event-id [evt any/c]) string?]{
@(mdn-bar "MessageEvent: lastEventId property"
          "https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/lastEventId")
Returns the event id string associated with @racket[evt].
}

@defproc[(message-event-source [evt any/c]) any/c]{
@(mdn-bar "MessageEvent: source property"
          "https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/source")
Returns the source object associated with @racket[evt].
}

@defproc[(message-event-ports [evt any/c]) any/c]{
@(mdn-bar "MessageEvent: ports property"
          "https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/ports")
Returns the transferred ports associated with @racket[evt].
}

@defproc[(close-event? [x any/c]) boolean?]{
@(mdn-bar "CloseEvent"
          "https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent")
Returns @racket[#t] when @racket[x] is a DOM CloseEvent value.
}

@defproc[(close-event-was-clean [evt any/c]) boolean?]{
@(mdn-bar "CloseEvent: wasClean property"
          "https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent/wasClean")
Returns whether @racket[evt] closed cleanly.
}

@defproc[(close-event-code [evt any/c]) exact-nonnegative-integer?]{
@(mdn-bar "CloseEvent: code property"
          "https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent/code")
Returns the numeric close code for @racket[evt].
}

@defproc[(close-event-reason [evt any/c]) string?]{
@(mdn-bar "CloseEvent: reason property"
          "https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent/reason")
Returns the close reason string for @racket[evt].
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

@section{Event Accessors}

@defproc[(event-target [evt any/c]) any/c]{
Returns the event target object.
}

@defproc[(event-current-target [evt any/c]) any/c]{
Returns the current event target object.
}

@defproc[(stop-propagation! [evt any/c]) void?]{
@(mdn-bar "Event: stopPropagation() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation")
Stops the event from continuing to bubble.
}

@defproc[(stop-immediate-propagation! [evt any/c]) void?]{
@(mdn-bar "Event: stopImmediatePropagation() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Event/stopImmediatePropagation")
Stops the event from reaching later handlers on the same target.
}

@defproc[(mouse-event-offset-x [evt any/c]) real?]{
Returns the horizontal offset for a mouse event.
}

@defproc[(mouse-event-offset-y [evt any/c]) real?]{
Returns the vertical offset for a mouse event.
}

@defproc[(mouse-event-client-x [evt any/c]) real?]{
Returns the mouse pointer's client X coordinate.
}

@defproc[(mouse-event-client-y [evt any/c]) real?]{
Returns the mouse pointer's client Y coordinate.
}

@defproc[(keyboard-event-key [evt any/c]) string?]{
Returns the keyboard key string for a keyboard event.
}

@defproc[(keyboard-event-code [evt any/c]) string?]{
Returns the keyboard code string for a keyboard event.
}

@defproc[(touch-event-touches [evt any/c]) touch-list?]{
Returns the active touch points for a touch event.
}

@defproc[(touch-event-target-touches [evt any/c]) touch-list?]{
Returns the touches currently targeting the event target.
}

@defproc[(touch-event-changed-touches [evt any/c]) touch-list?]{
Returns the touches that changed for the event.
}

@defproc[(touch-list? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a browser TouchList value.
}

@defproc[(touch-list-length [lst touch-list?]) exact-nonnegative-integer?]{
Returns the number of touches in the list.
}

@defproc[(touch-list-ref [lst touch-list?] [index exact-nonnegative-integer?])
         (or/c #f touch?)]{
Returns the touch at @racket[index], or @racket[#f] when the index is out
of range.
}

@defproc[(touch? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a browser Touch value.
}

@defproc[(touch-identifier [touch touch?]) exact-integer?]{
Returns the touch identifier.
}

@defproc[(touch-client-x [touch touch?]) real?]{
Returns the touch client X coordinate.
}

@defproc[(touch-client-y [touch touch?]) real?]{
Returns the touch client Y coordinate.
}

@defproc[(touch-page-x [touch touch?]) real?]{
Returns the touch page X coordinate.
}

@defproc[(touch-page-y [touch touch?]) real?]{
Returns the touch page Y coordinate.
}

@defproc[(touch-screen-x [touch touch?]) real?]{
Returns the touch screen X coordinate.
}

@defproc[(touch-screen-y [touch touch?]) real?]{
Returns the touch screen Y coordinate.
}
