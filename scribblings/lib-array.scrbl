#lang scribble/manual

@(require scribble/manual
          (for-label (only-in racket/base struct))
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

The library returns checked wrapper structs. Use @racket[array-raw]
only when you need to hand the underlying browser value to a low-level
bridge helper or an API that has not been wrapped yet.

The expensive direction, from JavaScript arrays into Racket vectors,
uses a bulk bridge helper. In other words, @racket[array->vector] does
not walk the array one element at a time through the WebRacket bridge.
The list conversion @racket[array->list] reuses that vector path.

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

(code:comment "The result is a checked wrapper, not a raw browser object.")
(array? arr)

(code:comment "Read it back as a Racket vector.")
(array->vector arr)

(code:comment "Use the raw accessor only when you need the underlying browser value.")
(array-raw arr)
]

@section{Array Values}

@defstruct[array ([raw external/raw])]{
Wraps a browser JavaScript @tt{Array} object.
}

@defproc[(array-raw [arr array?]) external/raw]{
Returns the underlying JavaScript array stored in @racket[arr].
}

@section{Array Conversion Helpers}

@defproc[(array-is-array? [value any/c]) boolean?]{
Returns @racket[#t] when @racket[value] is a JavaScript Array.
}

@defproc[(array-from [source any/c]
                     [map-function any/c #f]
                     [this-arg any/c #f])
         array?]{
Builds an array from an iterable or array-like value.
}

@defproc[(array-from-async [source any/c]
                           [map-function any/c #f]
                           [this-arg any/c #f])
         array?]{
Builds an array from an async iterable or array-like value.
}

@defproc[(array-of [item any/c] ...) array?]{
Builds a JavaScript array from the given items.
}

@defproc[(array-length [arr array?]) exact-nonnegative-integer?]{
Returns the length of a JavaScript array.
}

@defproc[(array-ref [arr array?] [index exact-integer?]) any/c]{
Reads an array element using JavaScript's @tt{at} semantics.
}

@defproc[(array-at [arr array?] [index exact-integer?]) any/c]{
Alias for @racket[array-ref].
}

@defproc[(array->vector [arr array?]) vector?]{
Converts a JavaScript array into a Racket vector using a bulk bridge
conversion.
}

@defproc[(array->list [arr array?]) list?]{
Converts a JavaScript array into a Racket list by reusing
@racket[array->vector].
}

@defproc[(vector->array [vec vector?]) array?]{
Converts a Racket vector into a JavaScript array.
}

@defproc[(list->array [xs list?]) array?]{
Converts a Racket list into a JavaScript array.
}

@defproc[(sequence->array [items (or/c list? vector?)]) array?]{
Converts a Racket list or vector into a JavaScript array.
}

@section{Array Methods}

@defproc[(array-join [arr array?] [separator (or/c #f string? symbol?) #f]) string?]{
Joins the array elements into a string.
}

@defproc[(array-slice [arr array?]
                      [start any/c #f]
                      [end any/c #f])
         array?]{
Returns a sliced copy of the array.
}

@defproc[(array-concat [arr array?] [item any/c] ...) array?]{
Concatenates arrays and values into a new array.
}

@defproc[(array-copy-within! [arr array?]
                             [target exact-integer?]
                             [start exact-integer?]
                             [end any/c #f])
         array?]{
Copies a slice of the array within the array itself.
}

@defproc[(array-fill! [arr array?]
                      [value any/c]
                      [start any/c #f]
                      [end any/c #f])
         array?]{
Fills a range of the array in place.
}

@defproc[(array-push! [arr array?] [item any/c] ...) exact-nonnegative-integer?]{
Appends one or more values to the end of the array.
}

@defproc[(array-pop [arr array?]) any/c]{
Removes and returns the last array element.
}

@defproc[(array-reverse! [arr array?]) array?]{
Reverses the array in place.
}

@defproc[(array-shift [arr array?]) any/c]{
Removes and returns the first array element.
}

@defproc[(array-sort! [arr array?] [compare-function any/c #f]) array?]{
Sorts the array in place.
}

@defproc[(array-splice! [arr array?]
                        [start exact-integer?]
                        [delete-count exact-integer?]
                        [item any/c] ...)
         array?]{
Replaces array elements in place and returns the removed items.
}

@defproc[(array-to-locale-string [arr array?]
                                 [locales-options any/c #f])
         string?]{
Converts the array to a localized string.
}

@defproc[(array-to-string [arr array?]) string?]{
Converts the array to a string.
}

@defproc[(array-unshift! [arr array?] [item any/c] ...) exact-nonnegative-integer?]{
Prepends one or more values to the array.
}

@defproc[(array-flat [arr array?] [depth any/c #f]) array?]{
Flattens the array by one level or an explicit depth.
}

@defproc[(array-to-reversed [arr array?]) array?]{
Returns a reversed copy of the array.
}

@defproc[(array-to-sorted [arr array?] [compare-function any/c #f]) array?]{
Returns a sorted copy of the array.
}

@defproc[(array-to-spliced [arr array?]
                           [start exact-integer?]
                           [delete-count exact-integer?]
                           [item any/c] ...)
         array?]{
Returns the result of splicing without mutating the original array.
}

@defproc[(array-with [arr array?] [index exact-integer?] [value any/c]) array?]{
Returns a copy of the array with one value replaced.
}
