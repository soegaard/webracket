#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-array-labels.rkt" "webracket"))
          )

@title{Library: @racketid[array]}
@declare-exporting[(lib "scribblings/lib-array-labels.rkt" "webracket")]

@(how-to-require include-lib array (lib "libs/array.rkt"))
@(compile-option-bar "Compile option: " "--ffi array")

The @racket[array] library provides a small checked surface for
JavaScript @tt{Array} values. It keeps the public API focused on
construction, indexing, and the conversion helpers that are useful when
moving between JavaScript arrays and Racket vectors or lists.

Use @racket[array] when you want to:

@itemlist[
  @item{build a JavaScript array from Racket values}
  @item{read array elements or lengths}
  @item{convert a JavaScript array to a vector or list}
  @item{apply the common non-callback Array methods used by WebRacket}
]

The library returns raw JavaScript array values rather than checked
wrapper structs. That keeps it lightweight and makes it easy to pass the
results on to other browser APIs.

String-like arguments accept either strings or symbols. Optional
arguments use @racket[#f] to mean that the argument is omitted.

@section{Array Quick Start}

Start by including the array library, building an array, and converting
it back to a Racket vector.

@racketblock[
(code:comment "Include the array helper library.")
(include-lib array)

(code:comment "Build a JavaScript array.")
(define arr (array-of 1 2 3))

(code:comment "Read it back as a Racket vector.")
(array->vector arr)
]

@section{Array Conversion Helpers}

@defproc[(array-is-array? [value any/c]) boolean?]{
Returns @racket[#t] when @racket[value] is a JavaScript Array.
}

@defproc[(array-from [source any/c]
                     [map-function any/c #f]
                     [this-arg any/c #f])
         external/raw]{
Builds an array from an iterable or array-like value.
}

@defproc[(array-from-async [source any/c]
                           [map-function any/c #f]
                           [this-arg any/c #f])
         external/raw]{
Builds an array from an async iterable or array-like value.
}

@defproc[(array-of [item any/c] ...) external/raw]{
Builds a JavaScript array from the given items.
}

@defproc[(array-length [arr external/raw]) exact-nonnegative-integer?]{
Returns the length of a JavaScript array.
}

@defproc[(array-ref [arr external/raw] [index exact-integer?]) any/c]{
Reads an array element using JavaScript's @tt{at} semantics.
}

@defproc[(array-at [arr external/raw] [index exact-integer?]) any/c]{
Alias for @racket[array-ref].
}

@defproc[(array->vector [arr any/c]) vector?]{
Converts a JavaScript array into a Racket vector.
}

@defproc[(array->list [arr any/c]) list?]{
Converts a JavaScript array into a Racket list.
}

@defproc[(vector->array [vec vector?]) external/raw]{
Converts a Racket vector into a JavaScript array.
}

@defproc[(list->array [xs list?]) external/raw]{
Converts a Racket list into a JavaScript array.
}

@defproc[(sequence->array [items (or/c list? vector?)]) external/raw]{
Converts a Racket list or vector into a JavaScript array.
}

@section{Array Methods}

@defproc[(array-join [arr external/raw] [separator (or/c #f string? symbol?) #f]) string?]{
Joins the array elements into a string.
}

@defproc[(array-slice [arr external/raw]
                      [start any/c #f]
                      [end any/c #f])
         external/raw]{
Returns a sliced copy of the array.
}

@defproc[(array-concat [arr external/raw] [item any/c] ...) external/raw]{
Concatenates arrays and values into a new array.
}

@defproc[(array-copy-within! [arr external/raw]
                             [target exact-integer?]
                             [start exact-integer?]
                             [end any/c #f])
         external/raw]{
Copies a slice of the array within the array itself.
}

@defproc[(array-fill! [arr external/raw]
                      [value any/c]
                      [start any/c #f]
                      [end any/c #f])
         external/raw]{
Fills a range of the array in place.
}

@defproc[(array-push! [arr external/raw] [item any/c] ...) exact-nonnegative-integer?]{
Appends one or more values to the end of the array.
}

@defproc[(array-pop [arr external/raw]) any/c]{
Removes and returns the last array element.
}

@defproc[(array-reverse! [arr external/raw]) external/raw]{
Reverses the array in place.
}

@defproc[(array-shift [arr external/raw]) any/c]{
Removes and returns the first array element.
}

@defproc[(array-sort! [arr external/raw] [compare-function any/c #f]) external/raw]{
Sorts the array in place.
}

@defproc[(array-splice! [arr external/raw]
                        [start exact-integer?]
                        [delete-count exact-integer?]
                        [item any/c] ...)
         external/raw]{
Replaces array elements in place and returns the removed items.
}

@defproc[(array-to-locale-string [arr external/raw]
                                 [locales-options any/c #f])
         string?]{
Converts the array to a localized string.
}

@defproc[(array-to-string [arr external/raw]) string?]{
Converts the array to a string.
}

@defproc[(array-unshift! [arr external/raw] [item any/c] ...) exact-nonnegative-integer?]{
Prepends one or more values to the array.
}

@defproc[(array-flat [arr external/raw] [depth any/c #f]) external/raw]{
Flattens the array by one level or an explicit depth.
}

@defproc[(array-to-reversed [arr external/raw]) external/raw]{
Returns a reversed copy of the array.
}

@defproc[(array-to-sorted [arr external/raw] [compare-function any/c #f]) external/raw]{
Returns a sorted copy of the array.
}

@defproc[(array-to-spliced [arr external/raw]
                           [start exact-integer?]
                           [delete-count exact-integer?]
                           [item any/c] ...)
         external/raw]{
Returns the result of splicing without mutating the original array.
}

@defproc[(array-with [arr external/raw] [index exact-integer?] [value any/c]) external/raw]{
Returns a copy of the array with one value replaced.
}
