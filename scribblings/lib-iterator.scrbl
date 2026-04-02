#lang scribble/manual

@(require scribble/manual
          (for-label (only-in racket/base struct))
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

@section{Iterator Values}

The wrapper keeps raw browser iterator objects tucked away in a single
@racket[iterator] struct. That gives the library room to grow with checked
helpers while still letting you recover the underlying browser object when you
explicitly need it.

@defstruct[iterator ([raw external/raw])]{
Wraps a browser @tt{Iterator} object in the checked struct used by the
library.
}

@defproc[(iterator-raw [iter iterator?]) external/raw]{
Returns the wrapped browser Iterator object stored inside @racket[iter].
}

@section{Iterator Quick Start}

Start by including the library, turning a JavaScript iterable into an
@tt{Iterator}, and asking it for its first step. The resulting value is an
@racket[iterator] struct, not a raw browser object.

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
  (iterator->vector doubled))
]

After this code runs, @racket[result] is @racket[#(2 4 6)].

@section{Iterator Helpers}

@defproc[(iterable? [value any/c]) boolean?]{
This predicate recognizes values that follow the JavaScript iterable protocol:
they provide a callable @tt{[Symbol.iterator]} method. Common iterable values
include arrays, strings, Sets, Maps, and wrapped iterator values.
}

@defstruct[iterator-zip-options ([mode symbol?] [padding any/c])]{
This struct describes the optional MDN @tt{options} object accepted by
@racket[iterator-zip] and @racket[iterator-zip-keyed].

The @racket[mode] field selects how iterables are combined. Use one of
@racket['shortest], @racket['longest], or @racket['strict].

The @racket[padding] field is reserved for future support of the MDN padding
object. Passing @racket[#f] for the whole options value uses the default
shortest-zip behavior.
}

@defproc[(iterator-from [object iterable?]) iterator?]{
@(mdn-bar "Iterator.from()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/from")
Returns a wrapped JavaScript iterator produced from @racket[object]. Common
inputs include arrays, strings, and other iterable JavaScript values. The value
may also be an existing iterator.
}

@defproc[(iterator-concat [iterable iterable?] ...) iterator?]{
@(mdn-bar "Iterator.concat()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/concat")
Concatenates several JavaScript iterables into a single iterator. Common
inputs include arrays, strings, Sets, Maps, and wrapped iterator values.
}

@defproc[(iterator-zip [iterables iterable?] [options (or/c #f iterator-zip-options?) #f]) iterator?]{
@(mdn-bar "Iterator.zip()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/zip")
Zips a collection of iterables into a new iterator. Pass @racket[#f] for the
default shortest-zip behavior, or pass an @racket[iterator-zip-options] struct
to describe the MDN options object.
}

@defproc[(iterator-zip-keyed [iterables iterable?] [options (or/c #f iterator-zip-options?) #f]) iterator?]{
@(mdn-bar "Iterator.zipKeyed()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/zipKeyed")
Zips a keyed collection of iterables into a new iterator. Pass @racket[#f] for
the default shortest-zip behavior, or pass an @racket[iterator-zip-options]
struct to describe the MDN options object.
}

@defproc[(iterator-drop [iter iterator?] [count exact-nonnegative-integer?]) iterator?]{
@(mdn-bar "Iterator.prototype.drop()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/drop")
Skips the first @racket[count] values from @racket[iter].
}

@defproc[(iterator-every [iter iterator?] [callback (or/c (-> any/c exact-nonnegative-integer? iterator? any/c) external?)] [this-arg any/c #f]) boolean?]{
@(mdn-bar "Iterator.prototype.every()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/every")
Calls @racket[callback] with three arguments:
@itemlist[
 @item{the yielded value,}
 @item{the zero-based index, and}
 @item{the wrapped source iterator.}
]
If @racket[callback] is an external browser value, it should be a
browser JavaScript function. The function receives the yielded value,
the zero-based index, and the wrapped source iterator.
Use @racket[#f] to omit @racket[this-arg]. If you need a literal @racket[#f]
receiver, pass a thunk such as @racket[(lambda () #f)].
The callback result is treated as true unless it is @racket[#f]. The result is
@racket[#t] only if every call succeeds.
}

@defproc[(iterator-filter [iter iterator?] [callback (or/c (-> any/c exact-nonnegative-integer? iterator? any/c) external?)] [this-arg any/c #f]) iterator?]{
@(mdn-bar "Iterator.prototype.filter()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/filter")
Calls @racket[callback] with the same three arguments as
@racket[iterator-every]. Keep the values for which the callback result is
treated as true unless it is @racket[#f].
If @racket[callback] is an external browser value, it should be a browser
JavaScript function with the same argument shape as @racket[iterator-every].
Use @racket[#f] to omit @racket[this-arg]. If you need a literal @racket[#f]
receiver, pass a thunk such as @racket[(lambda () #f)].
}

@defproc[(iterator-find [iter iterator?] [callback (or/c (-> any/c exact-nonnegative-integer? iterator? any/c) external?)] [this-arg any/c #f]) any/c]{
@(mdn-bar "Iterator.prototype.find()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/find")
Calls @racket[callback] with the same three arguments as
@racket[iterator-every]. Returns the first yielded value for which the callback
result is treated as true unless it is @racket[#f], or @racket[#f] if no value
matches.
If @racket[callback] is an external browser value, it should be a browser
JavaScript function with the same argument shape as @racket[iterator-every].
Use @racket[#f] to omit @racket[this-arg]. If you need a literal @racket[#f]
receiver, pass a thunk such as @racket[(lambda () #f)].
}

@defproc[(iterator-flat-map [iter iterator?] [callback (or/c (-> any/c exact-nonnegative-integer? iterator? iterable?) external?)] [this-arg any/c #f]) iterator?]{
@(mdn-bar "Iterator.prototype.flatMap()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/flatMap")
Calls @racket[callback] with the same three arguments as
@racket[iterator-every]. The callback should return an iterable value, and the
wrapper flattens those results one level.
If @racket[callback] is an external browser value, it should be a browser
JavaScript function with the same argument shape as @racket[iterator-every].
Use @racket[#f] to omit @racket[this-arg]. If you need a literal @racket[#f]
receiver, pass a thunk such as @racket[(lambda () #f)].
}

@defproc[(iterator-for-each [iter iterator?] [callback (or/c (-> any/c exact-nonnegative-integer? iterator? any/c) external?)] [this-arg any/c #f]) void?]{
@(mdn-bar "Iterator.prototype.forEach()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/forEach")
Calls @racket[callback] with the same three arguments as
@racket[iterator-every]. The return value is ignored.
If @racket[callback] is an external browser value, it should be a browser
JavaScript function with the same argument shape as @racket[iterator-every].
Use @racket[#f] to omit @racket[this-arg]. If you need a literal @racket[#f]
receiver, pass a thunk such as @racket[(lambda () #f)].
}

@defproc[(iterator-map [iter iterator?] [callback (or/c (-> any/c exact-nonnegative-integer? iterator? any/c) external?)] [this-arg any/c #f]) iterator?]{
@(mdn-bar "Iterator.prototype.map()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/map")
Calls @racket[callback] with the same three arguments as
@racket[iterator-every]. The callback result becomes the next yielded value.
If @racket[callback] is an external browser value, it should be a browser
JavaScript function with the same argument shape as @racket[iterator-every].
Use @racket[#f] to omit @racket[this-arg]. If you need a literal @racket[#f]
receiver, pass a thunk such as @racket[(lambda () #f)].
}

@defproc[(iterator-reduce [iter iterator?] [callback (or/c (-> any/c any/c exact-nonnegative-integer? iterator? any/c) external?)] [initial-value any/c #f]) any/c]{
@(mdn-bar "Iterator.prototype.reduce()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/reduce")
Calls @racket[callback] with four arguments:
@itemlist[
 @item{the running accumulator,}
 @item{the yielded value,}
 @item{the zero-based index, and}
 @item{the wrapped source iterator.}
]
If @racket[callback] is an external browser value, it should be a browser
JavaScript function with the same argument shape as @racket[iterator-every],
plus the accumulator as the first argument.
Use @racket[#f] to omit @racket[initial-value]. If you need a literal
@racket[#f] initial value, pass a thunk such as @racket[(lambda () #f)].
The callback result becomes the next accumulator value.
}

@defproc[(iterator-some [iter iterator?] [callback (or/c (-> any/c exact-nonnegative-integer? iterator? any/c) external?)] [this-arg any/c #f]) boolean?]{
@(mdn-bar "Iterator.prototype.some()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/some")
Calls @racket[callback] with the same three arguments as
@racket[iterator-every]. Returns @racket[#t] as soon as one callback call
produces a result that is treated as true unless it is @racket[#f].
If @racket[callback] is an external browser value, it should be a browser
JavaScript function with the same argument shape as @racket[iterator-every].
Use @racket[#f] to omit @racket[this-arg]. If you need a literal @racket[#f]
receiver, pass a thunk such as @racket[(lambda () #f)].
}

@defproc[(iterator-take [iter iterator?] [count exact-nonnegative-integer?]) iterator?]{
@(mdn-bar "Iterator.prototype.take()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/take")
Keeps only the first @racket[count] values from @racket[iter].
}

@defproc[(iterator->vector [iter iterator?]) vector?]{
@(mdn-bar "Iterator.prototype.toArray()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/toArray")
Collects the yielded values into a WebRacket vector.
}

@section{Raw Accessors}

These helpers expose the underlying browser constructor object or raw
iteration records. They are useful when you need to interoperate with lower
level code.

@defproc[(Iterator) external/raw]{
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

@defproc[(iterator-next [iter iterator?]) external/raw]{
@(mdn-bar "Iterator.prototype.next()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/next")
Returns the next iteration result from @racket[iter].
}

@defproc[(iterator-return [iter iterator?] [value any/c (void)]) external/raw]{
@(mdn-bar "Iterator.prototype.return()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator")
Asks @racket[iter] to finish early and returns the resulting iteration record.
}

@defproc[(iterator-symbol-iterator [iter iterator?]) iterator?]{
@(mdn-bar "Iterator" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator")
Returns @racket[iter] itself, matching the behavior of @tt{[Symbol.iterator]}.
}

@defproc[(iterator-dispose! [iter iterator?]) void?]{
@(mdn-bar "Symbol.dispose" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/dispose")
Disposes an iterator by calling @tt{return()} when the iterator provides it.
}
