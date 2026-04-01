#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/iterator-labels.rkt" "webracket")))

@title{Library: @racketid[iterator]}
@declare-exporting[(lib "scribblings/iterator-labels.rkt" "webracket")]

@(how-to-require include-lib iterator (lib "libs/iterator.rkt"))
@(compile-option-bar "Compile option: " "--ffi standard")

The @racket[iterator] library gives you a small checked wrapper around
JavaScript's @tt{Iterator} global object.

An iterator is an object that produces values one step at a time. It is a
useful bridge between JavaScript's lazy iteration protocol and WebRacket code
that wants to hold on to a browser iterator without dealing with the raw
constructor directly.

Use this library when you want to adapt an existing iterator or iterable into
an @tt{Iterator} object that can be passed around in WebRacket code.

@section{Iterator Quick Start}

Start by including the library, converting a JavaScript iterator, and
looking at the first step.

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
  (js-send iter "next" (vector)))
]

The quick start shows the main idea: take an existing iterator or iterable,
normalize it with @racket[iterator-from], and then use the returned iterator
value in later code. The example stops after binding the values so you can
inspect them or pass them to later expressions.

@section{Iterator Example}

This example shows a small adaptation workflow. We start with a JavaScript array
iterator, convert it through @racket[iterator-from], and then inspect the first
result object.

@racketblock[
(code:comment "Include the Iterator wrapper library.")
(include-lib iterator)

(code:comment "Create a JavaScript iterator from an array.")
(define source
  (js-eval "[\"red\", \"green\", \"blue\"].values()"))

(code:comment "Normalize it to the standard Iterator form.")
(define iter
  (iterator-from source))

(code:comment "Pull the first step from the iterator.")
(define step
  (js-send iter "next" (vector)))

(code:comment "Inspect the result object returned by next().")
(define done?
  (js-ref step "done"))
(define value
  (js-ref step "value"))
]

After this code runs, @racket[done?] is @racket[#f] and @racket[value] is
@racket["red"].

@section{Iterator API}

@defproc[(iterator) external/raw]{
@(mdn-bar "Iterator" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator")
Returns the JavaScript @tt{Iterator} constructor/global object.
}

@defproc[(iterator-from [object any/c]) external/raw]{
@(mdn-bar "Iterator.from()" "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/from")
Returns a JavaScript iterator produced from @racket[object]. The value may be
an iterator or an iterable.
}
