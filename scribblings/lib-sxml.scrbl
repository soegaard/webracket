#lang scribble/manual

@(require scribble/manual
          (for-label (lib "scribblings/lib-sxml-labels.rkt" "webracket"))
          "webracket-scribble-utils.rkt"
          )

@title{Library: @racketid[sxml]}
@declare-exporting[(lib "scribblings/lib-sxml-labels.rkt" "webracket")]

@(how-to-require include-lib sxml (lib "libs/sxml.rkt"))

The @racket[sxml] library converts SXML trees into HTML strings.
It is a small helper for programs that want to build HTML with
plain Racket data instead of writing string concatenation by hand.

SXML represents HTML as nested lists. The first element of a list is
the tag name, an optional attribute list headed by the symbol
@racket[|@|] holds attributes, and the
remaining elements are children. Text content may be strings, numbers,
or symbols.

Use @racket[sxml] when you want to:

@itemlist[
  @item{turn a small SXML tree into an HTML string}
  @item{build static HTML fragments with Racket data}
  @item{escape text and attribute values automatically}
]

@section{Quick Start}

Start by including the library and converting a tiny page fragment.

@racketblock[
(include-lib sxml)

(sxml->html
 `(div (|@| (class "notice"))
       (h1 "Hello")
       (p "This HTML was built from SXML.")))
]

@section{Examples}

The converter handles both a single node and a list of sibling nodes.
One common use is to build an HTML string and then place it into the
current page body.

@racketblock[
(include-lib document)
(include-lib element)
(include-lib sxml)

(define page
  (sxml->html
   `(div (|@| (class "card"))
         (h1 "Hello")
         (p "This page was built from SXML."))))

(define body (document-body))
(when body
  (element-set-inner-html! body page))
]

The HTML string can also be used on its own when you just want the
serialized markup.

@racketblock[
(sxml->html
 `(div (|@| (class "card"))
       (p "First paragraph.")
       (p "Second paragraph.")))
]

@racketresultblock[
"<div class=\"card\"><p>First paragraph.</p><p>Second paragraph.</p></div>"
]

Text and attribute values are escaped automatically.

@racketblock[
(sxml->html
 `(p "Use < and > safely, and keep \"quotes\" in attributes."
     (|@| (title "A \"quoted\" title"))))
]

@racketresultblock[
"<p title=\"A &quot;quoted&quot; title\">Use &lt; and &gt; safely, and keep &quot;quotes&quot; in attributes.</p>"
]

@section{API Reference}

@defproc[(sxml->html [sxml any/c]) string?]{
Converts an SXML tree or list of SXML nodes into an HTML string.
}
