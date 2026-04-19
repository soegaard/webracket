#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-fetch-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt"
          )

@title{Library: @racketid[fetch]}
@declare-exporting[(lib "scribblings/lib-fetch-labels.rkt" "webracket")]

@(how-to-require include-lib fetch (lib "libs/fetch.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The Fetch API is the browser's modern networking interface. It lets a
page create @racketid[Request] values, start network operations, and
inspect @racketid[Response] and @racketid[Headers] data without
dropping to lower-level XHR code.

In WebRacket, the @racket[fetch] library provides checked wrappers for
the browser Fetch API. The library keeps the common request,
response, and headers objects readable from Racket code while still
handing the browser raw objects where needed.

@(mdn-bar "Fetch API"
          "https://developer.mozilla.org/en-US/docs/Web/API/fetch")

Use @racket[fetch] when you want to:

@itemlist[
  @item{start a browser fetch request}
  @item{wrap and inspect @racketid[Request], @racketid[Response], and @racketid[Headers] values}
  @item{read status, URL, and header information from a response}
  @item{construct requests or headers from Racket values}
]

The library exposes checked wrapper structs for request, response, and
headers values. Use the @racket[fetch-request-raw],
@racket[fetch-response-raw], and @racket[fetch-headers-raw] accessors
only when you need to hand the browser object to a lower-level helper.

@(mdn-bar "Fetch API"
          "https://developer.mozilla.org/en-US/docs/Web/API/fetch")

String-like arguments accept either strings or symbols. Optional
arguments use @racket[#f] to mean that the argument is omitted.

@section{Fetch Quick Start}

@racketblock[
(code:comment "Include the Fetch wrapper library.")
(include-lib fetch)

(code:comment "Create a request and a header set.")
(define req (make-fetch-request "/api/status"))
(define hdrs (make-fetch-headers))

(code:comment "Perform the request and inspect the response later.")
(define resp (fetch req))

(void req hdrs resp)
]

@section{Fetch API}

@defproc[(fetch [request any/c] [init (or/c #f any/c) #f]) external/raw]{
@(mdn-bar "fetch() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/fetch")
Starts a browser fetch request and returns the raw browser promise-like
result.
}

@defproc[(make-fetch-request [input any/c] [init (or/c #f any/c) #f]) fetch-request?]{
@(mdn-bar "Request() constructor"
          "https://developer.mozilla.org/en-US/docs/Web/API/Request/Request")
Creates a wrapped browser @racketid[Request] value.
}

@defproc[(make-fetch-headers [init (or/c #f any/c) #f]) fetch-headers?]{
@(mdn-bar "Headers() constructor"
          "https://developer.mozilla.org/en-US/docs/Web/API/Headers/Headers")
Creates a wrapped browser @racketid[Headers] value.
}

@defproc[(fetch-request? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a wrapped browser request.
}

@defproc[(fetch-response? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a wrapped browser response.
}

@defproc[(fetch-headers? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a wrapped browser headers value.
}

@defproc[(fetch-request-url [request fetch-request?]) any/c]{
@(mdn-bar "Request: url property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Request/url")
Returns the request URL.
}

@defproc[(fetch-request-method [request fetch-request?]) any/c]{
@(mdn-bar "Request: method property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Request/method")
Returns the request method.
}

@defproc[(fetch-request-headers [request fetch-request?]) fetch-headers?]{
@(mdn-bar "Request: headers property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Request/headers")
Returns the request headers wrapped as @racket[fetch-headers].
}

@defproc[(fetch-response-status [response fetch-response?]) exact-nonnegative-integer?]{
@(mdn-bar "Response: status property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Response/status")
Returns the HTTP status code.
}

@defproc[(fetch-response-status-text [response fetch-response?]) string?]{
@(mdn-bar "Response: statusText property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Response/statusText")
Returns the HTTP status text.
}

@defproc[(fetch-response-ok? [response fetch-response?]) boolean?]{
@(mdn-bar "Response: ok property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Response/ok")
Returns @racket[#t] for successful responses.
}

@defproc[(fetch-response-headers [response fetch-response?]) fetch-headers?]{
@(mdn-bar "Response: headers property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Response/headers")
Returns the response headers wrapped as @racket[fetch-headers].
}

@defproc[(fetch-headers-get [headers fetch-headers?] [name (or/c string? symbol?)]) (or/c #f string?)]{
@(mdn-bar "Headers: get() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Headers/get")
Reads a header value by name.
}

@defproc[(fetch-headers-set! [headers fetch-headers?]
                             [name (or/c string? symbol?)]
                             [value (or/c string? symbol?)])
         void?]{
@(mdn-bar "Headers: set() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Headers/set")
Sets a header value.
}

@defproc[(fetch-headers-for-each [headers fetch-headers?]
                                 [proc (or/c procedure? external?)]
                                 [this-arg (or/c #f any/c) #f])
         void?]{
@(mdn-bar "Headers: forEach() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Headers/forEach")
Iterates over the headers with a callback.
}
