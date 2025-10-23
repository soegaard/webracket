#lang scribble/manual

@(require (for-label pict pict/shadow racket/base racket/contract racket/class racket/draw racket/future)
          scribble/eval)

@(define the-eval (make-base-eval))
@(the-eval '(require pict pict/shadow))

@title{Shadows}

@defmodule[pict/shadow]

These pict transformations add shadows or blurring in various shapes and
forms.

@defproc[(blur [p pict?]
               [h-radius (and/c real? (not/c negative?))]
               [v-radius (and/c real? (not/c negative?)) h-radius])
         pict?]{

Blurs @racket[p] using an iterated box blur that approximates a
gaussian blur. The @racket[h-radius] and @racket[v-radius] arguments
control the strength of the horizontal and vertical components of the
blur, respectively. They are given in terms of pict units, which may
not directly correspond to screen pixels.

The @racket[blur] function takes work proportional to
@racketblock[(* (pict-width p) (pict-height p))]
but it may be sped up by a factor of up to @racket[(processor-count)]
due to the use of @racket[future]s.

@examples[#:eval the-eval
(blur (text "blur" null 40) 5)
(blur (text "more blur" null 40) 10)
(blur (text "much blur" null 40) 20)
(blur (text "horiz. blur" null 40) 10 0)
]
The resulting pict has the same bounding box as @racket[p], so when
picts are automatically @racket[clip]ped (as in Scribble documents),
the pict should be @racket[inset] by the blur radius.
@examples[#:eval the-eval
(inset (blur (text "more blur" null 40) 10) 10)
]

@history[#:added "1.4"]{}
}

@defproc[(shadow [p pict?]
                 [radius (and/c real? (not/c negative?))]
                 [dx real? 0]
                 [dy real? dx]
                 [#:color color (or/c #f string? (is-a?/c color%)) #f]
                 [#:shadow-color shadow-color (or/c #f string? (is-a?/c color%)) #f])
         pict?]{

Creates a shadow effect by superimposing @racket[p] over a
blurred version of @racket[p]. The shadow is offset from @racket[p] by
(@racket[dx], @racket[dy]) units.

If @racket[color] is not @racket[#f], the foreground part is
@racket[(colorize p color)]; otherwise it is just @racket[p]. If
@racket[shadow-color] is not @racket[#f], the shadow part is produced
by blurring @racket[(colorize p shadow-color)]; otherwise it is
produced by blurring @racket[p].

The resulting pict has the same bounding box as @racket[p].

@examples[#:eval the-eval
(inset (shadow (text "shadow" null 50) 10) 10)
(inset (shadow (text "shadow" null 50) 10 5) 10)
(inset (shadow (text "shadow" null 50) 
               5 0 2 #:color "white" #:shadow-color "red")
       10)
]

@history[#:added "1.4"]{}
}

@defproc[(shadow-frame [pict pict?] ...
                       [#:sep separation real? 5]
                       [#:margin margin real? 20]
                       [#:background-color bg-color (or/c string? (is-a?/c color%)) "white"]
                       [#:frame-color frame-color (or/c string? (is-a?/c color%)) "gray"]
                       [#:frame-line-width frame-line-width (or/c real? #f 'no-frame) 0]
                       [#:shadow-side-length shadow-side-length real? 4]
                       [#:shadow-top-y-offset shadow-top-y-offset real? 10]
                       [#:shadow-bottom-y-offset shadow-bottom-y-offset real? 4]
                       [#:shadow-descent shadow-descent (and/c real? (not/c negative?)) 40]
                       [#:shadow-alpha-factor shadow-alpha-factor real? 3/4]
                       [#:blur blur-radius (and/c real? (not/c negative?)) 20])
         pict?]{

Surrounds the @racket[pict]s with a rectangular frame that casts a
symmetric ``curled paper'' shadow.

The @racket[pict]s are vertically appended with @racket[separation]
space between them. They are placed on a rectangular background of
solid @racket[bg-color] with @racket[margin] space on all sides. A
frame of @racket[frame-color] and @racket[frame-line-width] is added
around the rectangle, unless @racket[frame-line-width] is
@racket['no-frame]. The rectangle casts a shadow that extends
@racket[shadow-side-length] to the left and right, starts
@racket[shadow-top-y-offset] below the top of the rectangle and
extends to @racket[shadow-bottom-y-offset] below the bottom of the
rectangle in the center and an additional @racket[shadow-descent]
below that on the sides. The shadow is painted using a linear
gradient; @racket[shadow-alpha-factor] determines its density at the
center. Finally, the shadow is blurred by @racket[blur-radius]; all
previous measurements are pre-blur measurements.

@examples[#:eval the-eval
(shadow-frame (text "text in a nifty frame" null 60))
]

@history[#:added "1.4"]{}
}

@(close-eval the-eval)
