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
  @item{inspect or update an element's id or class name}
  @item{check whether an element matches a selector or find its closest ancestor}
  @item{attach a child node to an element}
  @item{set, read, remove, or test element attributes}
  @item{measure an element's position and size}
  @item{scroll an element into view}
]

The @racket[element] library provides checked wrappers for common DOM
element operations.

When a browser method expects a string, the wrapper also accepts a
symbol and normalizes it to a string.

@section{Element Values}

The element helpers pass around wrapped browser element values, so the
public API stays on the WebRacket side instead of exposing raw DOM
nodes.

@defstruct[element ([raw external/raw])]{
Wraps a browser Element object.
}

@defproc[(element-raw [elem element?]) external/raw]{
Returns the wrapped browser Element object.
}

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

@section{Element Properties and Queries}

The following helpers cover the most common element identity and
attribute operations.

@defproc[(element-id [element element?]) string?]{
@(mdn-bar "Element: id property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/id")
Returns the element id string.
}

@defproc[(element-set-id! [element element?]
                          [id (or/c string? symbol?)]) void?]{
@(mdn-bar "Element: id property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/id")
Sets the element id. Symbols are accepted and normalized to strings.
}

@defproc[(element-class-name [element element?]) string?]{
@(mdn-bar "Element: className property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/className")
Returns the element class name string.
}

@defproc[(element-set-class-name! [element element?]
                                  [class-name (or/c string? symbol?)]) void?]{
@(mdn-bar "Element: className property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/className")
Sets the element class name. Symbols are accepted and normalized to strings.
}

@defproc[(element-has-attribute? [element element?]
                                 [name (or/c string? symbol?)]) boolean?]{
@(mdn-bar "Element: hasAttribute() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/hasAttribute")
Reports whether the element has the named attribute.
}

@defproc[(element-has-attributes? [element element?]) boolean?]{
@(mdn-bar "Element: hasAttributes() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/hasAttributes")
Reports whether the element has any attributes.
}

@defproc[(element-remove-attribute! [element element?]
                                    [name (or/c string? symbol?)]) void?]{
@(mdn-bar "Element: removeAttribute() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/removeAttribute")
Removes the named attribute.
}

@defproc[(element-remove-attribute-ns! [element element?]
                                       [ns (or/c string? symbol?)]
                                       [name (or/c string? symbol?)]) void?]{
@(mdn-bar "Element: removeAttributeNS() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/removeAttributeNS")
Removes a namespaced attribute.
}

@defproc[(element-get-attribute-names [element element?]) vector?]{
@(mdn-bar "Element: getAttributeNames() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttributeNames")
Returns the element's attribute names as a WebRacket vector of strings.
}

@defproc[(element-matches? [element element?]
                           [selector (or/c string? symbol?)]) boolean?]{
@(mdn-bar "Element: matches() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/matches")
Checks whether the element matches a CSS selector.
}

@defproc[(element-closest [element element?]
                          [selector (or/c string? symbol?)]) (or/c #f element?)]{
@(mdn-bar "Element: closest() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/closest")
Returns the closest ancestor matching a CSS selector, or @racket[#f] if
there is no match.
}

@section{Element Content and Collections}

These helpers cover element children, HTML content, and layout
collections that are usually easiest to read as WebRacket vectors.

@defproc[(element-children [element element?]) vector?]{
@(mdn-bar "Element: children property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/children")
Returns the child elements as a WebRacket vector of wrapped elements.
}

@defproc[(element-child-element-count [element element?]) exact-nonnegative-integer?]{
@(mdn-bar "Element: childElementCount property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/childElementCount")
Returns the number of child elements.
}

@defproc[(element-first-element-child [element element?]) (or/c #f element?)]{
@(mdn-bar "Element: firstElementChild property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/firstElementChild")
Returns the first child element, or @racket[#f] if there is none.
}

@defproc[(element-last-element-child [element element?]) (or/c #f element?)]{
@(mdn-bar "Element: lastElementChild property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/lastElementChild")
Returns the last child element, or @racket[#f] if there is none.
}

@defproc[(element-inner-html [element element?]) string?]{
@(mdn-bar "Element: innerHTML property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/innerHTML")
Returns the element's inner HTML.
}

@defproc[(element-set-inner-html! [element element?]
                                  [html (or/c string? symbol?)]) void?]{
@(mdn-bar "Element: innerHTML property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/innerHTML")
Sets the element's inner HTML. Symbols are accepted and normalized to strings.
}

@defproc[(element-outer-html [element element?]) string?]{
@(mdn-bar "Element: outerHTML property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/outerHTML")
Returns the element's outer HTML.
}

@defproc[(element-set-outer-html! [element element?]
                                  [html (or/c string? symbol?)]) void?]{
@(mdn-bar "Element: outerHTML property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/outerHTML")
Sets the element's outer HTML. Symbols are accepted and normalized to strings.
}

@defproc[(element-text-content [element element?]) (or/c #f string?)]{
@(mdn-bar "Node: textContent property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Node/textContent")
Returns the element's text content.
}

@defproc[(element-set-text-content! [element element?]
                                    [text (or/c string? symbol?)]) void?]{
@(mdn-bar "Node: textContent property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Node/textContent")
Sets the element's text content. Symbols are accepted and normalized to strings.
}

@defproc[(get-client-rects [element element?]) vector?]{
@(mdn-bar "Element: getClientRects() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getClientRects")
Returns the element's client rectangles as a WebRacket vector of wrapped DOMRect values.
}

@defproc[(query-selector-all [element (or/c element? external?)]
                             [selector (or/c string? symbol?)]) vector?]{
@(mdn-bar "Element: querySelectorAll() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/querySelectorAll")
Returns all matching descendants as a WebRacket vector of wrapped elements.
}

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
]

When you only need a few common operations, the most useful entry points
are @racket[append-child!], @racket[set-attribute!],
@racket[get-bounding-client-rect], @racket[element-id],
@racket[element-children], @racket[element-inner-html], and
@racket[scroll-into-view!].

@defproc[(append-child! [parent (or/c element? external?)] [child (or/c element? external?)]) void?]{
@(mdn-bar "Node: appendChild() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Node/appendChild")
Appends a child node.

If an @racket[external] is passed as an argument, it should be a browser
@racketid[Node] value, such as an element or a text node. Wrapped
@racket[element] and @racket[text] values are also accepted.
}

@defproc[(set-attribute! [element element?]
                         [name (or/c string? symbol?)]
                         [value (or/c string? symbol?)]) void?]{
@(mdn-bar "Element: setAttribute() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute")
Sets an attribute on an element.
}

@defproc[(scroll-into-view! [element element?] [align-to-top? boolean? #t]) void?]{
@(mdn-bar "Element: scrollIntoView() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView")
Scrolls ancestors until the element is visible.
}

@defproc[(get-bounding-client-rect [element element?]) dom-rect?]{
@(mdn-bar "Element: getBoundingClientRect() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect")
Returns the element's bounding rectangle.
}

@defproc[(get-client-rects [element element?]) vector?]{
@(mdn-bar "Element: getClientRects() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getClientRects")
Returns the element's client rectangles as a WebRacket vector of
wrapped DOMRect values.
}

@defproc[(query-selector-all [element (or/c element? external?)]
                             [selector (or/c string? symbol?)]) vector?]{
@(mdn-bar "Element: querySelectorAll() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/querySelectorAll")
Returns all matching descendants as a WebRacket vector of wrapped elements.
}
