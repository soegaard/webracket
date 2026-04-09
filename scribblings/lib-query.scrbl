#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-query-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-document-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-element-labels.rkt" "webracket"))
          )

@title{Library: @racketid[query]}
@declare-exporting[(lib "scribblings/lib-query-labels.rkt" "webracket")]

@(how-to-require include-lib query (lib "libs/query.rkt"))

The @racket[query] library provides a small set of tools for finding
DOM elements and working with the resulting selections.

@margin-note{The @racket[query] library is still experimental. Its API
may change as the selection helpers settle.}

Use @racket[query] when you want to:

@itemlist[
  @item{select elements with CSS selectors}
  @item{inspect the size or contents of a selection}
  @item{map or iterate over selected elements}
  @item{chain a selection through dot-prefixed helper calls}
]

Currently, selectors accept strings only.
Helper functions such as @racket[element-set-text-content!]
accept the same values as documented in the underlying DOM libraries.

@section{QuickStart}

Start by including the library, building a small page sample,
selecting a heading, and reading its text.

The page sample is equivalent to @tt{<div id="query-root"><h1 id="hw">Hello
World!</h1></div>}.

@racketblock[
(include-lib document)
(include-lib sxml)
(include-lib query)

(code:comment "Build a small page sample in the current document.")
(define at (string->symbol "@"))
(define page
  (sxml->dom
   `(div (,at (id "query-root"))
         (h1 (,at (id "hw"))
             "Hello World!"))))

(element-append! (document-body) page)

(code:comment "Select the heading by id.")
(define sel ($ "#hw"))

(code:comment "Read the text from the first selected element.")
(.text sel)
(code:comment "=> \"Hello World!\"")
]

The @racket[chain] macro gives you a convenient left-to-right style for
selection workflows:

@racketblock[
(chain ($ "#hw")
       .text)
(code:comment "=> \"Hello World!\"")
]

@section{Examples}

These examples show the most common selection operations.

@subsection{Selecting Elements}

@racketblock[
(define sel ($ ".card"))
]

@racketblock[
($selection? sel)
(code:comment "=> #t")
]

@racketblock[
($length sel)
(code:comment "=> 0")
]

@subsection{Inspecting a Selection}

@racketblock[
(define first-card ($first ($ ".card")))
]

When the selection is empty, @racket[$first] returns @racket[#f].

@racketblock[
(define nodes ($selection->vector ($ ".card")))
]

@subsection{Iterating and Mapping}

@racketblock[
(chain ($ ".card")
       .for-each
       (lambda (node)
         (element-set-attribute! node 'data-seen 'yes)))
]

@racketblock[
(chain ($ ".card")
       .map
       (lambda (node)
         (element-text-content node)))
]

@subsection{Text Content}

@racketblock[
(define title ($ "#hw"))
(.text title)
(code:comment "=> \"Hello World!\"")
]

The @racket[.text] helper reads the text content of the first selected
element and returns @racket[""] when the selection is empty.

@subsection{Chaining}

@racketblock[
(chain ($ "#hw") .text)
(code:comment "=> \"Hello World!\"")
]

The last step in a @racket[chain] form may return a non-selection
value. That makes it convenient to end a pipeline with a query result,
a string, or any other final answer.

@section{API Reference}

@defstruct[$selection ([elements external/raw])]{
Wraps the current query result.
}

@defproc[($ [selector string?]) $selection?]{
Selects elements from the current document using a CSS selector.
}

@defproc[($select [selector string?]) $selection?]{
Alias for @racket[$].
}

@defproc[($length [sel $selection?]) exact-nonnegative-integer?]{
Returns the number of elements in @racket[sel].
}

@defproc[($ref [sel $selection?]
              [i exact-nonnegative-integer?]) (or/c #f any/c)]{
Returns the element at index @racket[i], or @racket[#f] when the index
is out of range.
}

@defproc[($first [sel $selection?]) (or/c #f any/c)]{
Returns the first element in @racket[sel], or @racket[#f] when the
selection is empty.
}

@defproc[($selection->vector [sel $selection?]) vector?]{
Converts the selection into a Racket vector of wrapped elements.
}

@defproc[($map [f (any/c -> any/c)]
              [sel $selection?]) $selection?]{
Applies @racket[f] to each selected element and returns a new
selection.
}

@defproc[($for-each [f (any/c -> any/c)]
                   [sel $selection?]) $selection?]{
Applies @racket[f] to each selected element and returns the original
selection.
}

@defproc[(.map [sel $selection?]
               [f (any/c -> any/c)]) $selection?]{
Chainable alias for @racket[$map].
}

@defproc[(.for-each [sel $selection?]
                    [f (any/c -> any/c)]) $selection?]{
Chainable alias for @racket[$for-each].
}

@defproc[(.text [sel $selection?]) string?]{
Returns the text content of the first selected element.
}

@defform[(chain seed .method arg ...)]{
Threads @racket[seed] through one or more dot-prefixed helper calls.
Each step receives the current selection as its first argument.
The final step may return any value, including a non-selection result.
}
