#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/define-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt")

@title{Library: @racketid[define]}
@declare-exporting[(lib "libs/define.rkt" "webracket")]

@(how-to-require require-lib define (lib "libs/define.rkt"))

Function definitions and function calls in Racket support keyword arguments.
WebRacket cannot currently reuse the implementation.
This means the normal WebRacket @racket[define] form does not support keyword arguments.
However, keyword arguments are useful, so this library provides a poor man's version
named @racket[define/key].

The form @racket[define/key] is used to define functions with keyword
arguments. A modified @racket[#%app] is provided, that rewrites call with
keywords into a form that keyword accepting functions expect. Other calls
are left untouched.

The @tt{define} library provides @racket[#%app], @racket[define/key], and
@racket[call/key].


This library is small and focused. It adds a syntax-layer wrapper for
defining procedures with required and optional positional arguments,
required and optional keyword arguments, and an optional rest argument.

@defform*/subs[[(define/key (head args) body ...+)]
               ([head id
                      (head args)]
                [args (code:line arg ...)
                      (code:line arg ... @#,racketparenfont{.} rest-id)]
                     [arg arg-id
                     [arg-id default-expr]
                     (code:line keyword arg-id)
                     (code:line keyword [arg-id default-expr])])]{

The form is provided by @racket[(require-lib define)].

The form creates a transformer binding for @racket[id].
The binding behaves as follows:

@itemlist[
  @item{In identifier position, @racket[id] expands to a procedure value.}
  @item{In application position, @racket[(id ...)] is rewritten to a call of
        that procedure, where keyword tokens are converted to keyword-value
        expressions.}
]

The generated procedure accepts required positional arguments, optional
positional arguments, an optional rest argument, required keyword arguments,
and optional keyword arguments.

In @tt{#lang webracket}, procedure application rewrites keyword tokens to
keyword values. This means keyword call syntax also works through aliases and
higher-order positions, not only in direct calls to the macro-bound name.

For direct calls to the macro-bound identifier, malformed keyword use is
reported during expansion, for example unknown or duplicate keywords.
Runtime calls raise exceptions for positional arity mismatches and missing
required keyword arguments.

@racketblock[
(require-lib define)

(define/key (f x #:k k)
  (+ x k))

(f 3 #:k 4)

(define/key (g x [y 10] . r)
  (list x y r))

(g 1)
(g 1 2 3 4)

(define h f)
(h 3 #:k 4)
]
}

@defform[(call/key f arg ...)]{

The form is provided by @racket[(require-lib define)].

Calls @racket[f] while allowing keyword-call surface syntax in argument
position. Each keyword token in @racket[arg ...] is rewritten as a keyword
value expression before the call.

This form is still available as an explicit keyword-call rewrite form.
In current @tt{#lang webracket}, direct applications already rewrite keyword
tokens, so @racket[call/key] is usually optional.

@racketblock[
(require-lib define)

(define/key (greet who #:style style)
  (string-append who ":" style))

(define g greet)

(call/key g "Ada" #:style "formal")
]
}
