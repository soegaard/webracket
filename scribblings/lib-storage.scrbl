#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-storage-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt"
          )

@title{Library: @racketid[storage]}
@declare-exporting[(lib "scribblings/lib-storage-labels.rkt" "webracket")]

@(how-to-require include-lib storage (lib "libs/storage.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The Web Storage API gives browsers a simple synchronous key/value
store. It is the usual choice when a page needs a small amount of
client-side state that should be available through @racketid[localStorage]
or @racketid[sessionStorage].

In WebRacket, the @racket[storage] library provides checked wrappers
for the browser @tt{Storage} object. That lets programs read and write
browser storage without handling raw JavaScript objects directly.

@(mdn-bar "Storage API"
          "https://developer.mozilla.org/en-US/docs/Web/API/Storage")

The two storage areas behave slightly differently:

@itemlist[
  @item{@racket[session-storage] is partitioned by both browser tab and origin, so each tab gets its own origin-scoped area and the data disappears when the tab closes}
  @item{@racket[local-storage] is partitioned by origin only, so every document with the same origin shares the same area and the data survives browser restarts}
]

Both storage areas are synchronous, so a large amount of storage work
can block other browser activity while it runs. String-like arguments
accept either strings or symbols.

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
@(mdn-bar "Storage"
          "https://developer.mozilla.org/en-US/docs/Web/API/Storage")
Wraps a browser @racketid[Storage] object in a checked struct.
}

@defproc[(storage? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a wrapped browser storage value.
}

@defproc[(local-storage) storage?]{
@(mdn-bar "Window: localStorage property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage")
Returns the current page's @racketid[localStorage] object as a checked wrapper.
}

@defproc[(session-storage) storage?]{
@(mdn-bar "Window: sessionStorage property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Window/sessionStorage")
Returns the current page's @racketid[sessionStorage] object as a checked wrapper.
}

@defproc[(storage-length [value storage?]) exact-nonnegative-integer?]{
@(mdn-bar "Storage: length property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Storage/length")
Returns the number of stored entries.
}

@defproc[(storage-key [value storage?] [index exact-nonnegative-integer?]) (or/c #f string?)]{
@(mdn-bar "Storage: key() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Storage/key")
Returns the key stored at a numeric position.
}

@defproc[(storage-get-item [value storage?] [key (or/c string? symbol?)]) (or/c #f string?)]{
@(mdn-bar "Storage: getItem() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Storage/getItem")
Returns the stored value for @racket[key], if present.
}

@defproc[(storage-set-item! [value storage?]
                            [key (or/c string? symbol?)]
                            [data (or/c string? symbol?)])
         void?]{
@(mdn-bar "Storage: setItem() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Storage/setItem")
Stores a string value under @racket[key].
}

@defproc[(storage-remove-item! [value storage?]
                               [key (or/c string? symbol?)])
         void?]{
@(mdn-bar "Storage: removeItem() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Storage/removeItem")
Removes the entry for @racket[key].
}

@defproc[(storage-clear! [value storage?]) void?]{
@(mdn-bar "Storage: clear() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Storage/clear")
Removes all stored values.
}
