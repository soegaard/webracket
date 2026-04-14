#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-simple-pretty-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt"
          )

@title{Library: @racketid[simple-pretty]}
@declare-exporting[(lib "scribblings/lib-simple-pretty-labels.rkt" "webracket")]

@(how-to-require include-lib simple-pretty (lib "libs/simple-pretty.rkt"))

The @racket[simple-pretty] library formats Racket values as readable
pretty-printed text. It is useful when you want a predictable
pretty-printer without depending on the full Racket pretty-printing
stack.

It provides four entry points:

@itemlist[
  @item{@racket[simple-pretty-print] for writing to the current output port}
  @item{@racket[simple-pretty-write] for printing in a write-style format}
  @item{@racket[simple-pretty-display] for printing in a display-style format}
  @item{@racket[simple-pretty-format] for producing a formatted string}
]

The printer is controlled by an association list of options. The
default settings are stored in @racket[default-pretty-options]. Common
options include the line width, nesting depth, whether to append a
newline, whether to show exactness prefixes, and whether to abbreviate
read macros such as @racket[quote] and @racket[quasiquote].

@section{Quick Start}

Start by including the library and formatting a small value.

@racketblock[
(include-lib simple-pretty)

(simple-pretty-format '(1 2 3))
]

@section{Examples}

Use @racket[simple-pretty-format] when you want a string result.

@racketblock[
(simple-pretty-format '(quote (alpha beta)))
]

@racketresultblock[
"'(alpha beta)"
]

Use @racket[simple-pretty-write] or @racket[simple-pretty-display] when
you want to send the formatted text directly to a port.

@racketblock[
(simple-pretty-write '(1 2 3))
(simple-pretty-display "hello world")
]

@section{API Reference}

@defthing[default-pretty-options list?]{
The default association list of pretty-print options.
}

@defproc[(simple-pretty-print [v any/c]
                              [port output-port? (current-output-port)]
                              [options list? '()])
         void?]{
Writes @racket[v] in pretty-printed form to @racket[port].
}

@defproc[(simple-pretty-write [v any/c]
                              [port output-port? (current-output-port)]
                              [options list? '()])
         void?]{
Writes @racket[v] using write-style formatting.
}

@defproc[(simple-pretty-display [v any/c]
                                [port output-port? (current-output-port)]
                                [options list? '()])
         void?]{
Writes @racket[v] using display-style formatting.
}

@defproc[(simple-pretty-format [v any/c]
                               [options list? '()])
         string?]{
Returns @racket[v] as a pretty-printed string.
}
