#lang scribble/manual

@(require (for-label pict pict/color racket/base racket/contract)
          scribble/eval)

@(define the-eval (make-base-eval))
@(the-eval '(require pict pict/color))

@title{Color Helpers}

@defmodule[pict/color]


@deftogether[(
@defproc[(red [pict pict?]) pict?]
@defproc[(orange [pict pict?]) pict?]
@defproc[(yellow [pict pict?]) pict?]
@defproc[(green [pict pict?]) pict?]
@defproc[(blue [pict pict?]) pict?]
@defproc[(purple [pict pict?]) pict?]
@defproc[(black [pict pict?]) pict?]
@defproc[(brown [pict pict?]) pict?]
@defproc[(gray [pict pict?]) pict?]
@defproc[(white [pict pict?]) pict?]
@defproc[(cyan [pict pict?]) pict?]
@defproc[(magenta [pict pict?]) pict?]
)]{

These functions apply appropriate colors to picture @racket[p].

@examples[#:eval the-eval
(red (disk 20))
]
@history[#:added "1.4"]{}
}

@deftogether[(
@defproc[(light [color color/c]) color/c]
@defproc[(dark [color color/c]) color/c]
)]{

These functions produce lighter or darker versions of a color.

@examples[#:eval the-eval
(hc-append (colorize (disk 20) "red")
           (colorize (disk 20) (dark "red"))
           (colorize (disk 20) (light "red")))
]

@history[#:added "1.4"]{}
}

@defthing[color/c flat-contract?]{

This contract recognizes color strings, @racket[color%] instances, and RGB color
lists.

@history[#:added "1.4"]{}
}

@(close-eval the-eval)
