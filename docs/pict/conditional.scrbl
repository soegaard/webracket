#lang scribble/manual

@(require (for-label pict pict/conditional racket/base racket/contract)
          scribble/eval)

@(define the-eval (make-base-eval))
@(the-eval '(require pict pict/conditional))

@title{Conditional Combinations}

@defmodule[pict/conditional]

These pict control flow operators decide which pict of several to use.  All
branches are evaluated; the resulting pict is a combination of the pict chosen
by normal conditional flow with @racket[ghost] applied to all the other picts.
The result is a picture large enough to accommodate each alternative, but showing
only the chosen one.  This is useful for staged slides, as the pict chosen may
change with each slide but its size and position will not.

@defform/subs[(pict-if maybe-combine test-expr then-expr else-expr)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses either @racket[then-expr] or @racket[else-expr] based on
@racket[test-expr], similarly to @racket[if].  Combines the chosen, visible
image with the other, invisible image using @racket[combine-expr], defaulting to
@racket[lbl-superimpose].

@examples[#:eval the-eval
(let ([f (lambda (x)
           (pict-if x
                    (disk 20)
                    (disk 40)))])
  (hc-append 10
             (frame (f #t))
             (frame (f #f))))
]

@history[#:added "1.4"]{}
}

@defform/subs[(pict-cond maybe-combine [test-expr pict-expr] ...)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses a @racket[pict-expr] based on the first successful @racket[test-expr],
similarly to @racket[cond].  Combines the chosen, visible image with the other,
invisible images using @racket[combine-expr], defaulting to
@racket[lbl-superimpose].

@examples[#:eval the-eval
(let ([f (lambda (x)
           (pict-cond #:combine cc-superimpose
             [(eq? x 'circle) (circle 20)]
             [(eq? x 'disk) (disk 40)]
             [(eq? x 'text) (text "ok" null 20)]))])
  (hc-append 10
             (frame (f 'circle))
             (frame (f 'disk))
             (frame (f 'text))))
]

@history[#:added "1.4"]{}
}

@defform/subs[(pict-case test-expr maybe-combine [literals pict-expr] ...)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses a @racket[pict-expr] based on @racket[test-expr] and each list of
@racket[literals], similarly to @racket[case].  Combines the chosen, visible
image with the other, invisible images using @racket[combine-expr], defaulting
to @racket[lbl-superimpose].

@examples[#:eval the-eval
(let ([f (lambda (x)
           (pict-case x
             [(circle) (circle 20)]
             [(disk) (disk 40)]
             [(text) (text "ok" null 20)]))])
  (hc-append 10
             (frame (f 'circle))
             (frame (f 'disk))
             (frame (f 'text))))
]

@history[#:added "1.4"]{}
}

@deftogether[(
@defproc[(show [pict pict?] [show? any/c #t]) pict?]
@defproc[(hide [pict pict?] [hide? any/c #t]) pict?]
)]{

These functions conditionally show or hide an image, essentially choosing
between @racket[pict] and @racket[(ghost pict)].  The only difference between
the two is the default behavior and the opposite meaning of the @racket[show?]
and @racket[hide?] booleans.  Both functions are provided for mnemonic purposes.

@history[#:added "1.4"]{}
}

@(close-eval the-eval)
