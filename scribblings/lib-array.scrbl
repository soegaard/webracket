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

When an @racket[array] value is displayed, the wrapped JavaScript
elements are shown in a vector-like form. That display is just a
readable representation of the underlying browser array; it is not a
Racket vector.

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

@section{Examples}

These examples show the most common array operations and their
results.

@subsection{Construction and Conversion}

@racketblock[
(define arr (array-of 1 2 3))
(array? arr)
]

@racketresultblock[
#t
]

@racketblock[
(array->vector arr)
]

@racketresultblock[
#(1 2 3)
]

@racketblock[
(vector->array '#(4 5 6))
]

@racketresultblock[
(array '#(4 5 6))
]

That result is a checked @racket[array] wrapper around a JavaScript
array with the elements @racket[4], @racket[5], and @racket[6].

@racketblock[
(array-from (array-raw arr))
]

@racketresultblock[
(array '#(1 2 3))
]

@racketblock[
(array-from (vector->array '#(7 8 9)))
]

@racketresultblock[
(array '#(7 8 9))
]

@racketblock[
(array-from-async (array-raw arr))
]

@racketresultblock[
(array '#(1 2 3))
]

@racketblock[
(array-from (vector->array '#(1 2 3))
            (lambda (value index) (+ value index)))
]

@racketresultblock[
(array '#(1 3 5))
]

The next example uses the optional @racket[this-arg] slot. The callback
still returns the same numbers, but the third argument is now being
forwarded as the JavaScript @tt{this} binding.

@racketblock[
(array-from (vector->array '#(1 2 3))
            (lambda (value index)
              (+ value index))
            'ignored-this)
]

@racketresultblock[
(array '#(1 3 5))
]

@racketblock[
(array-to-string arr)
]

@racketresultblock[
"1,2,3"
]

@subsection{Inspection}

@racketblock[
(array-length arr)
]

@racketresultblock[
3
]

@racketblock[
(array-ref arr 1)
]

@racketresultblock[
2
]

@racketblock[
(array-join arr ",")
]

@racketresultblock[
"1,2,3"
]

@racketblock[
(array-to-locale-string arr)
]

@racketresultblock[
"1,2,3"
]

@subsection{Mutation and Derived Copies}

@racketblock[
(array-slice arr 1 3)
]

@racketresultblock[
(array '#(2 3))
]

@racketblock[
(array-concat arr 4 5)
]

@racketresultblock[
(array '#(1 2 3 4 5))
]

@racketblock[
(array-fill! (vector->array '#(1 2 3 4)) 9 1 3)
]

@racketresultblock[
(array '#(1 9 9 4))
]

@racketblock[
(array-copy-within! (vector->array '#(1 2 3 4)) 2 0 2)
]

@racketresultblock[
(array '#(1 2 1 2))
]

@racketblock[
(array-sort! (vector->array '#(3 1 2)))
]

@racketresultblock[
(array '#(1 2 3))
]

@racketblock[
(array-to-sorted (vector->array '#(3 1 2)))
]

@racketresultblock[
(array '#(1 2 3))
]

@racketblock[
(array-flat (array-of 1 (array-of 2 3)) 1)
]

@racketresultblock[
(array '#(1 2 3))
]

@racketblock[
(array-with arr 1 99)
]

@racketresultblock[
(array '#(1 99 3))
]

@racketblock[
(array-splice! (vector->array '#(1 2 3 4)) 1 2 9 8)
]

@racketresultblock[
(array '#(2 3))
]

@section{Array Values}

@defstruct[array ([raw external/raw])]{
Wraps a browser JavaScript @tt{Array} object.
}

@defproc[(array-raw [arr array?]) external/raw]{
Returns the underlying JavaScript array stored in @racket[arr].
}

@section{Array Conversion Helpers}

@defproc[(array-from [source any/c]
                     [map-function (or/c procedure? external?) #f]
                     [this-arg any/c #f])
         array?]{
Builds an array from an iterable or array-like value.

If @racket[map-function] is supplied, it is called with each source
value and its index, and its result becomes the next array item. The
optional @racket[this-arg] value is used as the JavaScript
@tt{this} value while calling @racket[map-function].
}

@defproc[(array-from-async [source any/c]
                           [map-function (or/c procedure? external?) #f]
                           [this-arg any/c #f])
         array?]{
Builds an array from an async iterable or array-like value.

If @racket[map-function] is supplied, it is called with each source
value and its index, and its result becomes the next array item. The
optional @racket[this-arg] value is used as the JavaScript
@tt{this} value while calling @racket[map-function].
}

@defproc[(array-of [item any/c] ...) array?]{
Builds a JavaScript array from the given JavaScript values.
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

The optional @racket[start] and @racket[end] positions are passed to
JavaScript's @tt{slice} method as-is, so they follow JavaScript's own
index coercion rules.
}

@defproc[(array-concat [arr array?] [item any/c] ...) array?]{
Concatenates arrays and values into a new array. Non-array items are
included as individual elements.
}

@defproc[(array-copy-within! [arr array?]
                             [target exact-integer?]
                             [start exact-integer?]
                             [end any/c #f])
         array?]{
Copies a slice of the array within the array itself.

The @racket[target] and @racket[start] arguments are array positions.
The optional @racket[end] position is forwarded to JavaScript's
@tt{copyWithin} method unchanged.
}

@defproc[(array-fill! [arr array?]
                      [value any/c]
                      [start any/c #f]
                      [end any/c #f])
         array?]{
Fills a range of the array in place with the given JavaScript value.

The optional @racket[start] and @racket[end] positions are passed to
JavaScript's @tt{fill} method as-is, so they follow JavaScript's own
index coercion rules.
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

@defproc[(array-sort! [arr array?]
                     [compare-function (or/c procedure? external?) #f])
         array?]{
Sorts the array in place.

If @racket[compare-function] is supplied, it is called with two
elements and should return a negative, zero, or positive number.
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

The optional @racket[locales-options] value is forwarded to JavaScript
as the locale/options argument.
}

@defproc[(array-to-string [arr array?]) string?]{
Converts the array to a string.
}

@defproc[(array-unshift! [arr array?] [item any/c] ...) exact-nonnegative-integer?]{
Prepends one or more values to the array.
}

@defproc[(array-flat [arr array?] [depth any/c #f]) array?]{
Flattens the array by one level or an explicit depth.

The optional @racket[depth] is forwarded directly to JavaScript's
@tt{flat} method.
}

@defproc[(array-to-reversed [arr array?]) array?]{
Returns a reversed copy of the array.
}

@defproc[(array-to-sorted [arr array?]
                          [compare-function (or/c procedure? external?) #f])
         array?]{
Returns a sorted copy of the array.

If @racket[compare-function] is supplied, it is called with two
elements and should return a negative, zero, or positive number.
}

@defproc[(array-to-spliced [arr array?]
                           [start exact-integer?]
                           [delete-count exact-integer?]
                           [item any/c] ...)
         array?]{
Returns the result of splicing without mutating the original array.

The @racket[start] and @racket[delete-count] arguments are array
positions.
}

@defproc[(array-with [arr array?] [index exact-integer?] [value any/c]) array?]{
Returns a copy of the array with one value replaced by the given
JavaScript value.

The @racket[index] argument is an array position.
}
