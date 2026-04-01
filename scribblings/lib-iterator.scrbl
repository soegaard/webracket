#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/iterator-labels.rkt" "webracket")))

@title{Library: @racketid[iterator]}
@declare-exporting[(lib "scribblings/iterator-labels.rkt" "webracket")]

@(how-to-require include-lib iterator (lib "libs/iterator.rkt"))
@(compile-option-bar "Compile option: " "--ffi standard")

The @racket[iterator] library gives you a checked wrapper around JavaScript's
@tt{Iterator} helper objects. These helpers let you build, transform, and
consume lazy streams of values without dropping down to raw browser calls.

Use this library when you want to work with the JavaScript iterator helper
family from WebRacket code. The library covers the constructor, the helper
prototype, the static helpers, and the common iterator-transform methods.

@section{Iterator Quick Start}

Start by including the library, turning a JavaScript iterable into an
@tt{Iterator}, and asking it for its first step.

@racketblock[
(code:comment "Include the checked Iterator wrapper library.")
(include-lib iterator)

(code:comment "Create a JavaScript iterator in the browser.")
(define source
  (js-eval "[1, 2, 3].values()"))

(code:comment "Convert the source into the standard Iterator form.")
(define iter
  (iterator-from source))

(code:comment "Ask the iterator for its first step.")
(define first-step
  (iterator-next iter))
]

The quick start shows the core pattern: convert an existing iterator or
iterable with @racket[iterator-from], then use the Rackety wrapper functions to
advance or transform the iterator.

@section{Iterator Example}

This example starts with a JavaScript array, turns it into an iterator, maps
each value to a new one, and then collects the results back into a WebRacket
value.

@racketblock[
(code:comment "Include the Iterator wrapper library.")
(include-lib iterator)

(code:comment "Create a JavaScript iterator from an array.")
(define source
  (js-eval "[1, 2, 3].values()"))

(code:comment "Convert it to the standard Iterator form.")
(define iter
  (iterator-from source))

(code:comment "Double each value as it flows through the iterator.")
(define doubled
  (iterator-map iter
                (lambda (n)
                  (* n 2))))

(code:comment "Take the results back into a normal WebRacket value.")
(define result
  (iterator-to-array doubled))
]

After this code runs, @racket[result] is @racket[#(2 4 6)].

@section{Static Helpers}

@defproc[(iterator) external/raw]{
@(mdn-bar "Iterator" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator")
Returns the JavaScript @tt{Iterator} constructor/global object.
}

@defproc[(iterator-prototype) external/raw]{
@(mdn-bar "Iterator" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator")
Returns @tt{Iterator.prototype}, the shared prototype for iterator helpers.
}

@defproc[(iterator-prototype-constructor) external/raw]{
@(mdn-bar "Iterator" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator")
Returns the constructor stored on @tt{Iterator.prototype}.
}

@defproc[(iterator-prototype-to-string-tag) string?]{
@(mdn-bar "Iterator" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator")
Returns the default @tt{Symbol.toStringTag} value for @tt{Iterator.prototype}.
}

@defproc[(iterator-from [object any/c]) external/raw]{
@(mdn-bar "Iterator.from()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/from")
Returns a JavaScript iterator produced from @racket[object]. The value may be
an iterator or an iterable.
}

@defproc[(iterator-concat [iterable any/c] ...) external/raw]{
@(mdn-bar "Iterator.concat()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/concat")
Concatenates several iterables into a single iterator.
}

@defproc[(iterator-zip [iterables any/c] [options any/c (void)]) external/raw]{
@(mdn-bar "Iterator.zip()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/zip")
Zips a collection of iterables into a new iterator. The optional @racket[options]
argument is accepted for compatibility, and the wrapper follows the default
shortest-zip behavior.
}

@defproc[(iterator-zip-keyed [iterables any/c] [options any/c (void)]) external/raw]{
@(mdn-bar "Iterator.zipKeyed()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/zipKeyed")
Zips a keyed collection of iterables into a new iterator. The optional
@racket[options] argument is accepted for compatibility, and the wrapper
follows the default shortest-zip behavior.
}

@section{Instance Methods}

@defproc[(iterator-next [iter external/raw]) external/raw]{
@(mdn-bar "Iterator.prototype.next()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/next")
Returns the next iteration result from @racket[iter].
}

@defproc[(iterator-return [iter external/raw] [value any/c (void)]) external/raw]{
@(mdn-bar "Iterator.prototype.return()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator")
Asks @racket[iter] to finish early and returns the resulting iteration record.
}

@defproc[(iterator-drop [iter external/raw] [count exact-nonnegative-integer?]) external/raw]{
@(mdn-bar "Iterator.prototype.drop()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/drop")
Skips the first @racket[count] values from @racket[iter].
}

@defproc[(iterator-every [iter external/raw] [callback (or/c procedure? external?)] [this-arg any/c (void)]) boolean?]{
@(mdn-bar "Iterator.prototype.every()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/every")
Checks whether every yielded value satisfies @racket[callback].
}

@defproc[(iterator-filter [iter external/raw] [callback (or/c procedure? external?)] [this-arg any/c (void)]) external/raw]{
@(mdn-bar "Iterator.prototype.filter()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/filter")
Keeps only the values accepted by @racket[callback].
}

@defproc[(iterator-find [iter external/raw] [callback (or/c procedure? external?)] [this-arg any/c (void)]) any/c]{
@(mdn-bar "Iterator.prototype.find()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/find")
Returns the first yielded value accepted by @racket[callback].
}

@defproc[(iterator-flat-map [iter external/raw] [callback (or/c procedure? external?)] [this-arg any/c (void)]) external/raw]{
@(mdn-bar "Iterator.prototype.flatMap()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/flatMap")
Maps each yielded value to an iterable and flattens the results one level.
}

@defproc[(iterator-for-each [iter external/raw] [callback (or/c procedure? external?)] [this-arg any/c (void)]) void?]{
@(mdn-bar "Iterator.prototype.forEach()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/forEach")
Calls @racket[callback] for every yielded value and discards the result.
}

@defproc[(iterator-map [iter external/raw] [callback (or/c procedure? external?)] [this-arg any/c (void)]) external/raw]{
@(mdn-bar "Iterator.prototype.map()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/map")
Transforms each yielded value.
}

@defproc[(iterator-reduce [iter external/raw] [callback (or/c procedure? external?)] [initial-value any/c (void)]) any/c]{
@(mdn-bar "Iterator.prototype.reduce()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/reduce")
Combines the iterator's values into a single result.
}

@defproc[(iterator-some [iter external/raw] [callback (or/c procedure? external?)] [this-arg any/c (void)]) boolean?]{
@(mdn-bar "Iterator.prototype.some()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/some")
Checks whether any yielded value satisfies @racket[callback].
}

@defproc[(iterator-take [iter external/raw] [count exact-nonnegative-integer?]) external/raw]{
@(mdn-bar "Iterator.prototype.take()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/take")
Keeps only the first @racket[count] values from @racket[iter].
}

@defproc[(iterator-to-array [iter external/raw]) any/c]{
@(mdn-bar "Iterator.prototype.toArray()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/toArray")
Collects the yielded values into a JavaScript array and converts the result to a
WebRacket value.
}

@section{Protocol Helpers}

@defproc[(iterator-symbol-iterator [iter external/raw]) external/raw]{
@(mdn-bar "Iterator" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator")
Returns @racket[iter] itself, matching the behavior of @tt{[Symbol.iterator]}.
}

@defproc[(iterator-dispose! [iter external/raw]) void?]{
@(mdn-bar "Symbol.dispose" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/dispose")
Disposes an iterator by calling @tt{return()} when the iterator provides it.
}
