#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-indexed-db-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt"
          )

@title{Library: @racketid[indexed-db]}
@declare-exporting[(lib "scribblings/lib-indexed-db-labels.rkt" "webracket")]

@(how-to-require include-lib indexed-db (lib "libs/indexed-db.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[indexed-db] library provides a checked wrapper for the
browser's IndexedDB factory object. It gives WebRacket programs a more
direct entry point than the lower-level @racket[window-indexed-db]
accessor.

Use @racket[indexed-db] when you want to:

@itemlist[
  @item{open and delete databases}
  @item{compare IndexedDB keys}
  @item{inspect the browser's IndexedDB factory object}
]

@section{IndexedDB Quick Start}

@racketblock[
(code:comment "Include the IndexedDB wrapper library.")
(include-lib indexed-db)

(code:comment "Get the browser's IndexedDB factory object.")
(define db (indexed-db))

(code:comment "Open a database.")
(define req (indexed-db-open db "demo-db"))

(void db req)
]

@section{IndexedDB API}

@defproc[(indexed-db [raw external/raw]) indexed-db?]{
Wraps the browser IndexedDB factory object.
}

@defproc[(indexed-db? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a wrapped browser IndexedDB factory value.
}

@defproc[(indexed-db-open [value indexed-db?]
                          [name (or/c string? symbol?)]
                          [version (or/c #f any/c) #f])
         external/raw]{
Opens a database and returns the raw browser request object.
}

@defproc[(indexed-db-delete-database! [value indexed-db?]
                                      [name (or/c string? symbol?)])
         external/raw]{
Requests deletion of a database.
}

@defproc[(indexed-db-cmp [value indexed-db?] [a any/c] [b any/c]) integer?]{
Compares two IndexedDB keys.
}

@defproc[(indexed-db-databases [value indexed-db?]) external/raw]{
Returns the browser promise-like request for the list of known databases.
}
