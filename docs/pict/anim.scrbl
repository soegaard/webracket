#lang scribble/doc

@(require (for-label pict racket)
          scribble/example
          scribble/manual)

@(define ss-eval (make-base-eval))
@(ss-eval '(require pict pict/code racket/format))

@title{Animation Helpers}

These functions are designed to work with the
slide constructors in @racketmodname[slideshow/play].

@declare-exporting[pict slideshow/play]

@section{Pict Interpolations}

@defproc[(fade-pict [n (real-in 0.0 1.0)] [p1 pict?] [p2 pict?]
                    [#:combine combine (pict? pict? . -> . pict?) cc-superimpose]
                    [#:composite? composite? any/c #t])
         pict?]{

Interpolates @racket[p1] and @racket[p2], where the result with
@racket[n] as @racket[0.0] is @racket[p1], and the result with
@racket[n] as @racket[1.0] is @racket[p2]. For intermediate points,
@racket[p1] fades out while @racket[p2] fades in as @racket[n] changes
from @racket[0.0] to @racket[1.0]. At the same time, the width and
height of the generated pict are intermediate between
@racket[p1] and @racket[p2], and the relative baselines and last
pict correspondingly morph within the bounding box.

The @racket[combine] argument determines how @racket[p1] and
@racket[p2] are aligned for morphing. For example, if @racket[p1] and
@racket[p2] both contain multiple lines of text with the same line
height but different number of lines, then using
@racket[ctl-superimpose] would keep the ascent line in a fixed
location relative to the top of the resulting pict as the rest of the
shape morphs around it.

The @racket[composite?] argument is passed to the @racket[cellophane]
as used to implement fading.

@examples[#:eval ss-eval
  (define (do-fade n)
    (fade-pict n (rectangle 30 30) (disk 30)))
  (apply ht-append 10
         (for/list ([n (in-range 0 1.2 0.2)])
           (vc-append (text (~r n #:precision 2))
                      (do-fade n))))
]

@history[#:changed "1.15" @elem{Added @racket[#:composite?] and enabled compositing by default.}]}

@defproc[(fade-around-pict [n (real-in 0.0 1.0)]
                           [p1 pict?] 
                           [make-p2 (pict? . -> . pict?)]
                           [#:composite? composite? any/c #t])
         pict?]{

Similar to @racket[fade-pict], but the target is not a fixed
@racket[_p2], but instead a function @racket[make-p2] that takes a
@racket[launder]ed @racket[ghost] of @racket[p1] and places it into a
larger scene. Also, @racket[p1] does not fade out as @racket[n]
increases; instead, @racket[p1] is placed wherever its ghost appears
in the result of @racket[make-p2].

For example,

@examples[#:eval ss-eval #:label #f #:escape THERE-IS-NO-ESCAPE
(get-current-code-font-size (λ () 20))

(define do-fade
  (lambda (n)
    (fade-around-pict n
                      (code x)
                      (lambda (g) (code (+ #,g 1))))))

(apply ht-append 10
       (for/list ([n (in-range 0 1.2 0.2)])
         (vc-append (text (~r n #:precision 2))
                    (do-fade n))))
]

animates the wrapping of @racket[x] with a @racket[(+ .... 1)] form.

@history[#:changed "1.15" @elem{Added @racket[#:composite?] and enabled compositing by default.}]}

@defproc[(slide-pict [base pict?]
                     [p pict?]
                     [p-from pict?]
                     [p-to pict?]
                     [n (real-in 0.0 1.0)])
         pict?]{

Pins @racket[p] onto @racket[base], sliding from @racket[p-from] to
@racket[p-to] (which are picts within @racket[base]) as
@racket[n] goes from @racket[0.0] to @racket[1.0]. The top-left
locations of @racket[p-from] and @racket[p-to] determine the placement
of the top-left of @racket[p].

The @racket[p-from] and @racket[p-to] picts are typically
@racket[launder]ed @racket[ghost]s of @racket[p] within @racket[base],
but they can be any picts within @racket[base].

@examples[#:eval ss-eval
(define (do-slide n)
  (define p1 (disk 30 #:color "plum"))
  (define p2 (disk 30 #:color "palegreen"))
  (define p3 (frame (inset (hc-append 30 p1 p2) 10)))
  (slide-pict p3
              (disk 10)
              p1 p2 n))

(apply ht-append 10
       (for/list ([n (in-range 0 1.2 0.2)])
         (vc-append (text (~r n #:precision 2))
                    (do-slide n))))
]}

@defproc[(slide-pict/center [base pict?]
                            [p pict?]
                            [p-from pict?]
                            [p-to pict?]
                            [n (real-in 0.0 1.0)])
         pict?]{

  Like @racket[slide-pict], but aligns the center of @racket[p]
  with @racket[p-from] and @racket[p-to].

@examples[#:eval ss-eval
(define (do-slide n)
  (define p1 (disk 30 #:color "plum"))
  (define p2 (disk 30 #:color "palegreen"))
  (define p3 (frame (inset (hc-append 30 p1 p2) 10)))
  (slide-pict/center p3
                     (disk 10)
                     p1 p2 n))

(apply ht-append 10
       (for/list ([n (in-range 0 1.2 0.2)])
         (vc-append (text (~r n #:precision 2))
                    (do-slide n))))
]}

@; --------------------------------------------------

@section{Merging Animations}

@defproc[(sequence-animations [gen (-> (real-in 0.0 1.0) pict?)]
                              ...)
         (-> (real-in 0.0 1.0) pict?)]{

Converts a list of @racket[gen] functions into a single function that
uses each @racket[gen] in sequence.}

@defproc[(reverse-animations [gen (-> (real-in 0.0 1.0) pict?)]
                             ...)
         (-> (real-in 0.0 1.0) pict?)]{

Converts a list of @racket[gen] functions into a single function that
run @racket[(sequence-animations gen ...)] in reverse.}

@; --------------------------------------------------

@section{Stretching and Squashing Time}

@deftogether[(
@defproc[(fast-start [n (real-in 0.0 1.0)]) (real-in 0.0 1.0)]
@defproc[(fast-end [n (real-in 0.0 1.0)]) (real-in 0.0 1.0)]
@defproc[(fast-edges [n (real-in 0.0 1.0)]) (real-in 0.0 1.0)]
@defproc[(fast-middle [n (real-in 0.0 1.0)]) (real-in 0.0 1.0)]
)]{

Monotonically but non-uniformly maps @racket[n] with fixed
points at @racket[0.0] and @racket[1.0].

Suppose that we have the following definitions for our examples:

@examples[#:eval ss-eval #:label #f
(define (do-slide n fast-proc)
  (define p1 (filled-rectangle 20 20 #:color "yellowgreen"))
  (define p2 (filled-rectangle 20 20 #:color "khaki"))
  (define p3 (frame (inset (hc-append 25 p1 p2) 10)))
  (slide-pict/center
   p3
   (disk 10)
   p1 p2
   (code:comment "note use of fast-proc")
   (fast-proc n)))

(define (run-animation fast-proc)
  (apply ht-append 10
         (for/list ([n (in-range 0 1.09 0.1)])
           (vc-append (text (~r n #:precision 2))
                      (do-slide n fast-proc)))))
]

A normal use of the animation looks like this:

@examples[#:eval ss-eval #:label #f
  (run-animation (λ (n) n))
]

The @racket[fast-start] mapping is convex, so that

@racketblock[(slide-pict _base p _p1 _p2 (fast-start n))]

appears to move quickly away from @racket[_p1] and then slowly as it
approaches @racket[_p2], assuming that @racket[n] increases uniformly.

Applying it to the animation above produces this:

@examples[#:eval ss-eval #:label #f
  (run-animation fast-start)
]

The @racket[fast-end] mapping is concave, so that

@racketblock[(slide-pict _base _p _p1 _p2 (fast-end _n))]

appears to move slowly away from @racket[_p1] and then quickly as it
approaches @racket[_p2], assuming that @racket[_n] increases uniformly.

@examples[#:eval ss-eval #:label #f
  (run-animation fast-end)
]

The @racket[fast-edges] mapping is convex at first and concave at the
end, so that

@racketblock[(slide-pict _base _p _p1 _p2 (fast-edges _n))]

appears to move quickly away from @racket[_p1], then more slowly, and
then quickly again near @racket[_p2], assuming that @racket[_n] increases
uniformly.

@examples[#:eval ss-eval #:label #f
  (run-animation fast-edges)
]

The @racket[fast-middle] mapping is concave at first and convex at the
end, so that

@racketblock[(slide-pict _base _p _p1 _p2 (fast-middle _n))]

@examples[#:eval ss-eval #:label #f
  (run-animation fast-middle)
]

appears to move slowly away from @racket[_p1], then more quickly, and
then slowly again near @racket[_p2], assuming that @racket[_n] increases
uniformly.}

@defproc[(split-phase [n (real-in 0.0 1.0)])
         (values (real-in 0.0 1.0) (real-in 0.0 1.0))]{

Splits the progression of @racket[n] from @racket[0.0] to @racket[1.0]
into a progression from @racket[(values 0.0 0.0)] to @racket[(values
1.0 0.0)] and then @racket[(values 1.0 0.0)] to @racket[(values 1.0
1.0)].

Here is an example that shows how to apply @racket[split-phase] to
the animation from the examples for @racket[fast-start]:

@examples[#:eval ss-eval #:label #f
(apply ht-append 10
       (for/list ([n (in-range 0 1.09 0.1)])
         (define-values (n1 n2) (split-phase n))
         (vc-append (text (~r n #:precision 2))
                    (do-slide n1 (λ (n) n))
                    (do-slide n2 (λ (n) n)))))
]}

@close-eval[ss-eval]
