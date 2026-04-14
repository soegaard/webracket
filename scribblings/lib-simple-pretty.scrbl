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

The options list is an association list of @racket[(cons key value)]
pairs. If the same option appears more than once, the later entry wins.

@section{Quick Start}

Start by including the library and formatting a small value.

@racketblock[
(include-lib simple-pretty)

(simple-pretty-format '(1 2 3))
(code:comment "=> \"(1 2 3)\"")
]

@section{Examples}

Use @racket[simple-pretty-format] when you want a string result.

@racketblock[
(simple-pretty-format '(quote (alpha beta)))
(code:comment "=> \"'(alpha beta)\"")
]

Use @racket[simple-pretty-write] or @racket[simple-pretty-display] when
you want to send the formatted text directly to a port.

@racketblock[
(simple-pretty-write '(1 2 3))
(code:comment "writes \"(1 2 3)\" to the current output port")
(simple-pretty-display "hello world")
(code:comment "writes \"hello world\" to the current output port")
]

@section{Options}

@defthing[default-pretty-options list?]{
The default pretty-print configuration.

The current defaults are:

@itemlist[
  @item{@racket['columns] @racket[79] - break lines when a rendered line
        would exceed 79 columns. Use @racket['infinity] to disable
        width-based line breaking.}
  @item{@racket['depth] @racket[#f] - do not limit nesting depth. Set it
        to a nonnegative integer to replace deeper values with
        @racket["..."].}
  @item{@racket['newline?] @racket[#t] - append a trailing newline when
        writing to a port.}
  @item{@racket['show-inexactness] @racket[#f] - print numbers with
        Racket's standard write-like number formatting, which shows
        inexactness prefixes when needed.}
  @item{@racket['exact-as-decimal] @racket[#f] - render exact rational
        numbers as decimal notation when the conversion is finite and
        still exact enough to print cleanly.}
  @item{@racket['.-symbol-without-bars] @racket[#f] - print the symbol
        @racket['|.|] as @racket["."] instead of the bar-quoted symbol
        form.}
  @item{@racket['abbreviate-read-macros] @racket[#t] - use the usual
        read-macro abbreviations for quoted forms, such as quote,
        quasiquote, unquote, unquote-splicing, syntax, quasisyntax,
        unsyntax, and unsyntax-splicing.}
]
}

Supplying options works like this:

@racketblock[
(simple-pretty-format '(1 2 3)
                      '((columns . 20)
                        (newline? . #f)
                        (abbreviate-read-macros . #t)))
]

Each option is validated before it is used. The accepted value shapes
are:

@itemlist[
  @item{@racket['columns] accepts an exact integer or @racket['infinity].}
  @item{@racket['depth] accepts @racket[#f] or a nonnegative exact integer.}
  @item{@racket['newline?], @racket['show-inexactness],
        @racket['exact-as-decimal], @racket['.-symbol-without-bars], and
        @racket['abbreviate-read-macros] all accept booleans.}
]

@section{API Reference}

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
