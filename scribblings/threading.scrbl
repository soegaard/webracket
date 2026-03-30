#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt")

@title{Library: @racketid[threading]}

@(how-to-require require-lib threading (lib "libs/threading.rkt"))

The @tt{threading} library provides a small set of macros for writing
function pipelines.

If you have ever read code like @racket[(g (f x))], the threading
operators let you write the same idea in a left-to-right style. That is
often easier to read when a value flows through several transformations.

@racketblock[
(require-lib threading)

(~~> x
     (f _)
     (g _))
]

This library is a port of the Racket @racket[threading] library by
Alexis King.

@url{https://docs.racket-lang.org/threading/index.html}

The operator @racket[~>] was renamed to @racket[~~>].
This was done to avoid a clash with @tt{web-easy}, which
uses @racket[~>] for observables.

The library has an ISC License.
