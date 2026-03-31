#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt")

@title{Library: @racketid[websocket]}

@(how-to-require include-lib websocket (lib "libs/websocket.rkt"))
@(compile-option-bar "Compile option: " "--ffi websocket")

WebSockets are a browser technology for keeping a connection open
between a web page and a server.

With ordinary HTTP, the browser sends a request and waits for a single
response. With WebSockets, the connection stays open after the initial
handshake. That means the browser and server can both send messages at
any time.

This is useful whenever the page needs live, bidirectional updates:

@itemlist[
  @item{chat and messaging}
  @item{live dashboards and notifications}
  @item{collaborative editing}
  @item{multiplayer games and remote controls}
]

The @racket[websocket] library is the checked, high-level API for WebSocket
programs in WebRacket. It is implemented on top of the lower-level
@tt{ffi/websocket.ffi} bindings, but application code should normally use
the @racket[websocket-*] functions documented on this page.

@section{WebSocket Quick Start}

To use WebSockets, create a connection, install the handlers you need,
and send messages when you are ready.

@racketblock[
(include-lib websocket)

(define ws
  (websocket-new "wss://echo-websocket.fly.dev/"))

(websocket-onopen! ws
  (lambda (_evt)
    (displayln "connected")))

(websocket-onmessage! ws
  (lambda (_evt)
    (displayln "message received")))

(websocket-send ws "hello")
]

When you are done with a connection, call @racket[websocket-close].
If your page is served over @tt{https}, use @tt{wss://} rather than
@tt{ws://}.


@section{WebSocket API Reference}

@subsection{WebSocket Validation}

@defproc[(websocket? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a WebSocket value.

The value is the opaque browser object produced by @racket[websocket-new].
}

@defproc[(check-websocket [who symbol?] [x any/c]) void?]{
Checks that @racket[x] is a WebSocket value and raises a contract error
if not.
}

@subsection{WebSocket Construction and State}

@defproc[(websocket-new [url string?]
                        [protocol (or/c string? symbol?)] ...)
         websocket?]{
@(mdn-bar "WebSocket() constructor"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/WebSocket")

Creates a new WebSocket connection to @racket[url].

Provide zero or more protocol names after @racket[url] to select the
subprotocols the client is willing to speak.

Each protocol name may be a string or a symbol. Symbols are converted
to strings before the low-level WebSocket binding is called.

Common protocol names include @racket["chat"], @racket["json"],
@racket["metrics"], and @racket["sync-v2"], but the server and client
must agree on the actual names.
}

@defproc[(websocket-url [ws websocket?]) string?]{
@(mdn-bar "WebSocket: url property"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/url")

Returns the connection URL associated with @racket[ws].
}

@defproc[(websocket-ready-state [ws websocket?]) symbol?]{
@(mdn-bar "WebSocket: readyState property"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/readyState")

Returns the browser @tt{readyState} value for @racket[ws] as a symbol.

The returned symbols are @racket['connecting], @racket['open],
@racket['closing], and @racket['closed].
}

@defproc[(websocket-ready-state-number [ws websocket?]) exact-nonnegative-integer?]{
@(mdn-bar "WebSocket: readyState property"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/readyState")

Returns the browser @tt{readyState} number for @racket[ws].

The usual WebSocket state numbers are:
@itemlist[
  @item{@tt{0}: connecting}
  @item{@tt{1}: open}
  @item{@tt{2}: closing}
  @item{@tt{3}: closed}
]
}


@defproc[(websocket-buffered-amount [ws websocket?]) exact-nonnegative-integer?]{
@(mdn-bar "WebSocket: bufferedAmount property"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/bufferedAmount")

Returns the number of queued bytes waiting to be transmitted.
}

@defproc[(websocket-protocol [ws websocket?]) string?]{
@(mdn-bar "WebSocket: protocol property"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/protocol")

Returns the negotiated subprotocol string, or an empty string if no
subprotocol was selected.
}

@defproc[(websocket-extensions [ws websocket?]) string?]{
@(mdn-bar "WebSocket: extensions property"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/extensions")

Returns the negotiated WebSocket extension string, or an empty string
if no extensions were negotiated.
}

@subsection{WebSocket Sending and Closing}

@defproc[(websocket-send [ws websocket?]
                         [data (or/c string? bytes? external?)])
         void?]{
@(mdn-bar "WebSocket: send() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send")

Sends @racket[data] through the WebSocket connection.

The message body can be a string, bytes, or an external browser value
such as @tt{ArrayBuffer}, @tt{Uint8Array}, or @tt{Blob}.
}

@defproc[(websocket-close [ws websocket?]
                          [code exact-integer? 1000]
                          [reason (or/c #f string?) #f])
         void?]{
@(mdn-bar "WebSocket: close() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close")

Closes the WebSocket connection.

If @racket[code] is omitted, the wrapper uses the browser default close
code @racket[1000].
If @racket[reason] is omitted or @racket[#f], no reason string is sent.
}

@subsection{WebSocket Event Handlers}

WebSocket event handlers are ordinary callback procedures that the
browser calls when something happens on the socket. Each handler is
stored in a property such as @tt{onopen} or @tt{onmessage}. Setting the
handler to @racket[#f] clears it, which means the browser stops calling
that callback for future events.

@defproc[(websocket-onopen! [ws websocket?]
                            [handler (or/c #f procedure? external?)])
         void?]{
@(mdn-bar "WebSocket: open event"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/open_event")

Installs @racket[handler] as the @tt{onopen} event handler, or clears
the handler when @racket[handler] is @racket[#f].
}

@defproc[(websocket-onmessage! [ws websocket?]
                               [handler (or/c #f procedure? external?)])
         void?]{
@(mdn-bar "WebSocket: message event"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/message_event")

Installs @racket[handler] as the @tt{onmessage} event handler, or clears
the handler when @racket[handler] is @racket[#f].
}

@defproc[(websocket-onclose! [ws websocket?]
                             [handler (or/c #f procedure? external?)])
         void?]{
@(mdn-bar "WebSocket: close event"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close_event")

Installs @racket[handler] as the @tt{onclose} event handler, or clears
the handler when @racket[handler] is @racket[#f].
}

@defproc[(websocket-onerror! [ws websocket?]
                             [handler (or/c #f procedure? external?)])
         void?]{
@(mdn-bar "WebSocket: error event"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/error_event")

Installs @racket[handler] as the @tt{onerror} event handler, or clears
the handler when @racket[handler] is @racket[#f].
}

@subsection{WebSocket Event Listeners}

@defproc[(websocket-add-event-listener! [ws websocket?]
                                        [event-name (or/c string? symbol?)]
                                        [listener (or/c procedure? external?)]
                                        [options (or/c boolean? external?)] ...)
         external?]{
@(mdn-bar "EventTarget: addEventListener() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener")

Registers @racket[listener] for the named browser event and returns the
installed callback token.

Any additional positional values after @racket[listener] are forwarded
to the browser @tt{addEventListener} call unchanged.

The most common option form is a capture flag such as @racket[#t] or
@racket[#f]. You can also pass a JavaScript options object that uses
fields such as @tt{capture}, @tt{once}, @tt{passive}, and @tt{signal}.
}

@defproc[(websocket-remove-event-listener! [ws websocket?]
                                           [event-name (or/c string? symbol?)]
                                           [listener (or/c procedure? external?)]
                                           [options (or/c boolean? external?)] ...)
         void?]{
@(mdn-bar "EventTarget: removeEventListener() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/removeEventListener")

Removes a previously registered listener for the named browser event.

The listener can be either the original procedure or the token returned
by @racket[websocket-add-event-listener!].
Any additional positional values after @racket[listener] are forwarded
to the browser @tt{removeEventListener} call unchanged.

Use the same option values here that you used when adding the listener,
for example a capture flag or the same JavaScript options object.
}
