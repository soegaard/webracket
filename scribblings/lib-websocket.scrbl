#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-websocket-labels.rkt" "webracket")))

@title{Library: @racketid[websocket]}
@declare-exporting[(lib "libs/websocket.rkt" "webracket")]

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

String-like WebSocket arguments accept either strings or symbols, and
the wrapper converts symbols to their string names. Optional arguments
use @racket[#f] to mean that the argument is omitted.

The @racket[websocket-new] constructor returns a wrapped
@racket[websocket] value. Use @racket[websocket-raw] only when you need
to inspect the underlying browser object directly.

@section{WebSocket Quick Start}

To use WebSockets, create a connection, install the handlers you need,
and send messages when you are ready.

@racketblock[
(code:comment "Include the WebSocket library.")
(include-lib websocket)

(code:comment "Open a WebSocket connection.")
(define ws
  (websocket-new "wss://echo-websocket.fly.dev/"))

(code:comment "Run code when the connection opens.")
(websocket-onopen! ws
  (lambda (_evt)
    (displayln "connected")))

(code:comment "Run code when a message arrives.")
(websocket-onmessage! ws
  (lambda (_evt)
    (displayln "message received")))

(code:comment "Send a message over the connection.")
(websocket-send ws "hello")
]

When you are done with a connection, call @racket[websocket-close].
If your page is served over @tt{https}, use @tt{wss://} rather than
@tt{ws://}.


@section{WebSocket API Reference}

@subsection{WebSocket Validation}

@defstruct[websocket ([raw external/raw])]{
@racket[websocket-new] returns a wrapped browser WebSocket object.
The raw browser object is stored in @racket[raw].
}

@defproc[(websocket-raw [ws websocket?]) external/raw]{
Returns the underlying browser WebSocket object.
}

@defproc[(websocket? [x any/c]) boolean?]{
Returns @racket[#t] when @racket[x] is a wrapped WebSocket value.

The wrapped value is produced by @racket[websocket-new].
}

@defproc[(check-websocket [who symbol?] [x any/c]) void?]{
Checks that @racket[x] is a WebSocket value and raises a contract error
if not.
}

@subsection{WebSocket Construction and State}

@defproc[(websocket-new [url (or/c string? symbol?)]
                        [protocol (or/c string? symbol?)] ...)
         websocket?]{
@(mdn-bar "WebSocket() constructor"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/WebSocket")

Creates a new WebSocket connection to @racket[url].

The raw @racket[url] argument may be a string or a symbol. Symbols are
converted to strings before the browser constructor is called.

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
                         [data (or/c string? symbol? bytes? external?)])
         void?]{
@(mdn-bar "WebSocket: send() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send")

Sends @racket[data] through the WebSocket connection.

The raw @racket[data] argument can be a browser @racketid[ArrayBuffer],
@racketid[Uint8Array], or @racketid[Blob] value, or a Racket
@racket[bytes] value. If @racket[data] is a symbol, the wrapper converts
it to its string name first.
}

@defproc[(websocket-close [ws websocket?]
                          [code exact-integer? 1000]
                          [reason (or/c #f string? symbol?) #f])
         void?]{
@(mdn-bar "WebSocket: close() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close")

Closes the WebSocket connection.

If @racket[code] is omitted, the wrapper uses the browser default close
code @racket[1000].
If @racket[reason] is omitted or @racket[#f], no reason string is sent.
If @racket[reason] is a symbol, the wrapper converts it to its string
name before closing the socket.
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

The raw @racket[handler] argument, when supplied as an external, should
be a browser event-handler function that receives a browser
@racketid[Event] value.

Installs @racket[handler] as the @tt{onopen} event handler, or clears
the handler when @racket[handler] is @racket[#f].
}

@defproc[(websocket-onmessage! [ws websocket?]
                               [handler (or/c #f procedure? external?)])
         void?]{
@(mdn-bar "WebSocket: message event"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/message_event")

The raw @racket[handler] argument, when supplied as an external, should
be a browser event-handler function that receives a browser
@racketid[MessageEvent] value.

Installs @racket[handler] as the @tt{onmessage} event handler, or clears
the handler when @racket[handler] is @racket[#f].
}

@defproc[(websocket-onclose! [ws websocket?]
                             [handler (or/c #f procedure? external?)])
         void?]{
@(mdn-bar "WebSocket: close event"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close_event")

The raw @racket[handler] argument, when supplied as an external, should
be a browser event-handler function that receives a browser
@racketid[CloseEvent] value.

Installs @racket[handler] as the @tt{onclose} event handler, or clears
the handler when @racket[handler] is @racket[#f].
}

@defproc[(websocket-onerror! [ws websocket?]
                             [handler (or/c #f procedure? external?)])
         void?]{
@(mdn-bar "WebSocket: error event"
          "https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/error_event")

The raw @racket[handler] argument, when supplied as an external, should
be a browser event-handler function that receives a browser
@racketid[Event] value.

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

The raw @racket[listener] argument, when supplied as an external, should
be a browser callback function. The optional @racket[options] values can
be booleans or a browser @racketid[AddEventListenerOptions] object.

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

The raw @racket[listener] argument, when supplied as an external, should
be a browser callback function. The optional @racket[options] values can
be booleans or a browser @racketid[AddEventListenerOptions] object.

Removes a previously registered listener for the named browser event.

The listener can be either the original procedure or the token returned
by @racket[websocket-add-event-listener!].
Any additional positional values after @racket[listener] are forwarded
to the browser @tt{removeEventListener} call unchanged.

Use the same option values here that you used when adding the listener,
for example a capture flag or the same JavaScript options object.
}
