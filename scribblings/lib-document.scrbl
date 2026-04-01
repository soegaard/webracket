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

Use @racket[document] when you want to:

@itemlist[
  @item{look up the current page}
  @item{find an element by id or CSS selector}
  @item{create a fresh element before inserting it elsewhere}
  @item{inspect the document head or body}
]

The @racket[document] library provides checked wrappers for the current
document, element lookup, and selector queries.

@section{Document Quick Start}

Start by including the library, grabbing the current document, and
finding a useful part of the page.

@racketblock[
(code:comment "Include the checked document wrapper library.")
(include-lib document)

(code:comment "Get the current document.")
(define doc
  (document))

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
  (document))
(define body
  (document-body))

(code:comment "Create a new element that can be configured before insertion.")
(define note
  (document-create-element "p"))

(code:comment "Use a selector when you need to find an existing part of the page.")
(define main-area
  (document-query-selector "main"))

(void doc body note main-area)
]

If you already know the page element you want, the most useful entry
points are usually @racket[document-body], @racket[document-head],
@racket[document-get-element-by-id], @racket[document-query-selector],
and @racket[document-create-element].

@defproc[(document) external/raw]{
@(mdn-bar "Document"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document")
Returns the current document object.
}

@defproc[(document-body) (or/c #f external?)]{
@(mdn-bar "Document: body property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/body")
Returns the document body element, if present.
}

@defproc[(document-head) (or/c #f external?)]{
@(mdn-bar "Document: head property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/head")
Returns the document head element, if present.
}

@defproc[(document-create-element [tag string?]) external/raw]{
@(mdn-bar "Document: createElement() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/createElement")
Creates an element for @racket[tag].
}

@defproc[(document-get-element-by-id [id string?]) (or/c #f external?)]{
@(mdn-bar "Document: getElementById() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById")
Looks up a single element by id.
}

@defproc[(document-query-selector [selector string?]) (or/c #f external?)]{
@(mdn-bar "Document: querySelector() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector")
Returns the first matching descendant.
}
