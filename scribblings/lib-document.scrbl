#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[document]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib document (lib "libs/document.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[document] library is the checked wrapper for the browser's
current page document.

If you are new to browser programming, think of the document as the
tree of things currently on the page. It is where the page's elements
live, and it is the starting point for finding or creating content.

Use @racket[Document] when you want to:

@itemlist[
  @item{look up the current page}
  @item{find an element by id or CSS selector}
  @item{create a fresh element before inserting it elsewhere}
  @item{inspect the document head or body}
]

The @racket[document] library provides a checked wrapper for the current
document, plus wrapped element helpers for lookup and selector queries.

@section{Document Values}

@defstruct[document ([raw external/raw])]{
Wraps a browser Document object.
}

@defproc[(document-raw [doc document?]) external/raw]{
Returns the wrapped browser Document object.
}

@defproc[(Document) document?]{
@(mdn-bar "Document"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document")
Returns the current browser document wrapped in a checked struct.
}

@section{Document Quick Start}

Start by including the library, grabbing the current document, and
finding a useful part of the page.

@racketblock[
(code:comment "Include the checked document wrapper library.")
(include-lib document)

(code:comment "Get the current document.")
(define doc
  (Document))

(code:comment "Look for the page body or another element you care about.")
(define body
  (document-body))

(code:comment "Create a new paragraph element for later use.")
(define note
  (document-create-element "p"))
]

The quick start shows the three basic ideas: access the current
document, inspect the existing page, and create a new element.

@section{Document Example}

This example shows the usual pattern for page work: get the document,
create a new node, and prepare it to be inserted into the page.

@racketblock[
(code:comment "Include the document wrapper library.")
(include-lib document)

(code:comment "Get the current document and its body element.")
(define doc
  (Document))
(define body
  (document-body))

(code:comment "Create a new element that can be configured before insertion.")
(define note
  (document-create-element "p"))

(code:comment "Use a selector when you need to find an existing part of the page.")
(define main-area
  (document-query-selector "main"))
]

If you already know the page element you want, the most useful entry
points are usually @racket[document-body], @racket[document-head],
@racket[document-get-element-by-id], @racket[document-query-selector],
and @racket[document-create-element].

@defproc[(document-body) (or/c #f element?)]{
@(mdn-bar "Document: body property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/body")
Returns the document body element, if present.
}

@defproc[(document-head) (or/c #f element?)]{
@(mdn-bar "Document: head property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/head")
Returns the document head element, if present.
}

@defproc[(document-create-element [tag string?]) element?]{
@(mdn-bar "Document: createElement() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/createElement")
Creates an element for @racket[tag].
}

@defproc[(document-get-element-by-id [id string?]) (or/c #f element?)]{
@(mdn-bar "Document: getElementById() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById")
Looks up a single element by id.
}

@defproc[(document-query-selector [selector string?]) (or/c #f element?)]{
@(mdn-bar "Document: querySelector() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector")
Returns the first matching descendant.
}

@defproc[(document-element) element?]{
@(mdn-bar "Document: documentElement property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/documentElement")
Returns the document's root element.
}

@defproc[(document-open) document?]{
@(mdn-bar "Document: open() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/open")
Opens a document stream for writing and returns the wrapped document.
}

@defproc[(document-element-from-point [x real?] [y real?]) (or/c #f element?)]{
@(mdn-bar "Document: elementFromPoint() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/elementFromPoint")
Returns the topmost element at the given viewport position, or @racket[#f].
}
