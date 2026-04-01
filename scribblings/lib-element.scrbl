#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[element]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib element (lib "libs/element.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[element] library is the checked wrapper for common DOM
element operations.

Browser elements are the pieces of the page you can point at, style,
move, or insert into the DOM tree. They are the things that make up the
visible structure of the page.

Use @racket[element] when you want to:

@itemlist[
  @item{attach a child node to an element}
  @item{set or read element attributes}
  @item{measure an element's position and size}
  @item{scroll an element into view}
]

The @racket[element] library provides checked wrappers for common DOM
element operations.

@section{Element Quick Start}

Start by creating an element, adding an attribute, and inserting it
into the page body.

@racketblock[
(code:comment "Include the document and element wrapper libraries.")
(include-lib document)
(include-lib element)

(code:comment "Create a paragraph element.")
(define note
  (document-create-element "p"))

(code:comment "Give the element a class so it can be styled later.")
(set-attribute! note "class" "notice")

(code:comment "Insert the new element into the page body.")
(append-child! (document-body) note)
]

The quick start shows the basic browser pattern: create an element,
configure it, and insert it into the DOM.

@section{Element Example}

This example shows how to build a small card-like element, inspect its
rectangle, and then scroll it into view.

@racketblock[
(code:comment "Include the DOM helper libraries.")
(include-lib document)
(include-lib element)

(code:comment "Create a section element to act as a card.")
(define card
  (document-create-element "section"))

(code:comment "Add attributes before inserting it.")
(set-attribute! card "class" "card")

(code:comment "Insert the card into the page body.")
(append-child! (document-body) card)

(code:comment "Measure where the card lands on the page.")
(define rect
  (get-bounding-client-rect card))

(code:comment "Bring the card into view if needed.")
(scroll-into-view! card #t)

(void rect)
]

When you only need a few common operations, the most useful entry points
are @racket[append-child!], @racket[set-attribute!],
@racket[get-bounding-client-rect], and @racket[scroll-into-view!].

@defproc[(append-child! [parent external?] [child external?]) void?]{
@(mdn-bar "Node: appendChild() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Node/appendChild")
Appends a child node.
}

@defproc[(set-attribute! [element external?] [name string?] [value string?]) void?]{
@(mdn-bar "Element: setAttribute() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute")
Sets an attribute on an element.
}

@defproc[(scroll-into-view! [element external?] [align-to-top? any/c #t]) void?]{
@(mdn-bar "Element: scrollIntoView() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView")
Scrolls ancestors until the element is visible.
}

@defproc[(get-bounding-client-rect [element external?]) external/raw]{
@(mdn-bar "Element: getBoundingClientRect() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect")
Returns the element's bounding rectangle.
}
