#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-document-labels.rkt" "webracket")))

@title{Library: @racketid[document]}
@declare-exporting[(lib "libs/document.rkt" "webracket")]

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
The raw accessors for the document family live in the
@seclink["raw-accessors"]{Raw Accessors} appendix.

When a browser method expects a string, the wrapper also accepts a
symbol and normalizes it to a string.

@section{Document Values}

@defstruct[document ([raw external/raw])]{
Wraps a browser Document object.
}

@defproc[(Document) document?]{
@(mdn-bar "Document"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document")
Returns the current browser document wrapped in a checked struct.
}

@section{Text Values}

The text-node helper returns wrapped browser Text values so the document
library can hand back text nodes without exposing a raw browser object.

@defstruct[text ([raw external/raw])]{
Wraps a browser Text node.
}

@section{Node Values}

The document node helpers return wrapped browser Node values for node
types that are not already specialized as text, element, or attr.

@defstruct[node ([raw external/raw])]{
Wraps a browser Node object.
}

@section{Attr Values}

The document attribute helpers return wrapped browser Attr values so
attribute nodes stay on the WebRacket side.

@defstruct[attr ([raw external/raw])]{
Wraps a browser Attr node.
}

@section{Selection Values}

The selection helper returns a wrapped browser Selection object so the
current selection stays on the WebRacket side.

@defstruct[selection ([raw external/raw])]{
Wraps a browser Selection object.
}

@defproc[(selection-range-count [sel selection?]) exact-nonnegative-integer?]{
@(mdn-bar "Selection: rangeCount property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Selection/rangeCount")
Returns the number of ranges in the selection.
}

@defproc[(selection-is-collapsed? [sel selection?]) boolean?]{
@(mdn-bar "Selection: isCollapsed property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Selection/isCollapsed")
Reports whether the selection is collapsed.
}

@defproc[(selection-anchor-node [sel selection?]) (or/c #f node?)]{
@(mdn-bar "Selection: anchorNode property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Selection/anchorNode")
Returns the anchor node for the selection, if there is one.
}

@defproc[(selection-focus-node [sel selection?]) (or/c #f node?)]{
@(mdn-bar "Selection: focusNode property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Selection/focusNode")
Returns the focus node for the selection, if there is one.
}

@defproc[(selection-to-string [sel selection?]) string?]{
@(mdn-bar "Selection: toString() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Selection/toString")
Returns the selected text as a string.
}

@defproc[(selection-remove-all-ranges! [sel selection?]) void?]{
@(mdn-bar "Selection: removeAllRanges() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Selection/removeAllRanges")
Removes all ranges from the selection.
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

@defproc[(document-create-element [tag (or/c string? symbol?)]) element?]{
@(mdn-bar "Document: createElement() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/createElement")
Creates an element for @racket[tag].
}

@defproc[(document-create-attribute [name (or/c string? symbol?)]) attr?]{
@(mdn-bar "Document: createAttribute() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/createAttribute")
Creates a wrapped browser attribute node for @racket[name].
}

@defproc[(document-create-attribute-ns [ns (or/c string? symbol?)]
                                       [name (or/c string? symbol?)]) attr?]{
@(mdn-bar "Document: createAttributeNS() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/createAttributeNS")
Creates a wrapped namespaced browser attribute node for @racket[name].
}

@defproc[(document-create-text-node [text (or/c string? symbol?)]) text?]{
@(mdn-bar "Document: createTextNode() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/createTextNode")
Creates a wrapped browser text node for @racket[text].
}

@defproc[(document-create-comment [text (or/c string? symbol?)]) node?]{
@(mdn-bar "Document: createComment() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/createComment")
Creates a wrapped browser comment node for @racket[text].
}

@defproc[(document-create-cdata-section [text (or/c string? symbol?)]) node?]{
@(mdn-bar "Document: createCDATASection() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/createCDATASection")
Creates a wrapped browser CDATA section node for @racket[text].
}

@defproc[(document-create-document-fragment) node?]{
@(mdn-bar "Document: createDocumentFragment() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/createDocumentFragment")
Creates a wrapped browser document fragment.
}

@defproc[(document-create-processing-instruction [target (or/c string? symbol?)]
                                                 [data (or/c string? symbol?)]) node?]{
@(mdn-bar "Document: createProcessingInstruction() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/createProcessingInstruction")
Creates a wrapped browser processing instruction node.
}

@defproc[(document-adopt-node [node node?]) node?]{
@(mdn-bar "Document: adoptNode() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/adoptNode")
Adopts @racket[node] into the current document and returns the wrapped result.
}

@defproc[(document-get-element-by-id [id (or/c string? symbol?)]) (or/c #f element?)]{
@(mdn-bar "Document: getElementById() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById")
Looks up a single element by id.
}

@defproc[(document-query-selector [selector (or/c string? symbol?)]) (or/c #f element?)]{
@(mdn-bar "Document: querySelector() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector")
Returns the first matching descendant.
}

@defproc[(document-query-selector-all [selector (or/c string? symbol?)]) (or/c #f node-list?)]{
@(mdn-bar "Document: querySelectorAll() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelectorAll")
Returns all matching descendants as a wrapped NodeList.
}

@defproc[(document-get-selection) (or/c #f selection?)]{
@(mdn-bar "Document: getSelection() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/getSelection")
Returns the current selection as a wrapped @racket[selection] value.
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

@defproc[(document-elements-from-point [x real?] [y real?]) vector?]{
@(mdn-bar "Document: elementsFromPoint() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document/elementsFromPoint")
Returns the matching elements at the given viewport position as a
WebRacket vector of wrapped elements.
}
