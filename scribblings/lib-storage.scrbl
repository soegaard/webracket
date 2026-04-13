#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-storage-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt"
          )

@title{Library: @racketid[storage]}
@declare-exporting[(lib "scribblings/lib-storage-labels.rkt" "webracket")]

@(how-to-require include-lib storage (lib "libs/storage.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[storage] library provides checked wrappers for the browser
@tt{Storage} API. It gives WebRacket programs direct access to
@racketid[localStorage] and @racketid[sessionStorage] without exposing
raw browser objects.

Use @racket[storage] when you want to:

@itemlist[
  @item{read or write local and session storage entries}
  @item{inspect stored keys and lengths}
  @item{keep browser state in a simple key/value store}
]

String-like arguments accept either strings or symbols. Optional
arguments use @racket[#f] to mean that the argument is omitted.

@section{Storage Quick Start}

@racketblock[
(code:comment "Include the storage wrapper library.")
(include-lib storage)

(code:comment "Read the two built-in browser storage areas.")
(define ls (local-storage))
(define ss (session-storage))

(code:comment "Store a value and read it back later.")
(storage-set-item! ls 'webracket-demo "123")
(storage-get-item ls 'webracket-demo)
]

@section{Storage API}

@defproc[(storage [raw external/raw]) storage?]{
Wraps a browser @racketid[Storage] object.
}

@defproc[(storage? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a wrapped browser storage value.
}

@defproc[(local-storage) storage?]{
Returns the current page's @racketid[localStorage] object as a checked wrapper.
}

@defproc[(session-storage) storage?]{
Returns the current page's @racketid[sessionStorage] object as a checked wrapper.
}

@defproc[(storage-length [value storage?]) exact-nonnegative-integer?]{
Returns the number of stored entries.
}

@defproc[(storage-key [value storage?] [index exact-nonnegative-integer?]) (or/c #f string?)]{
Returns the key stored at a numeric position.
}

@defproc[(storage-get-item [value storage?] [key (or/c string? symbol?)]) (or/c #f string?)]{
Returns the stored value for @racket[key], if present.
}

@defproc[(storage-set-item! [value storage?]
                            [key (or/c string? symbol?)]
                            [data (or/c string? symbol?)])
         void?]{
Stores a string value under @racket[key].
}

@defproc[(storage-remove-item! [value storage?]
                               [key (or/c string? symbol?)])
         void?]{
Removes the entry for @racket[key].
}

@defproc[(storage-clear! [value storage?]) void?]{
Removes all stored values.
}
