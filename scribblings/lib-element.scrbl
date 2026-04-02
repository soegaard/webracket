#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-document-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-domrect-labels.rkt" "webracket"))
          (for-label (lib "scribblings/lib-element-labels.rkt" "webracket")))

@title{Library: @racketid[element]}
@declare-exporting[(lib "scribblings/lib-element-labels.rkt" "webracket")]

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
(element-set-attribute! note "class" "notice")

(code:comment "Insert the new element into the page body.")
(element-append! (document-body) note)
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

@defproc[(element-tag-name [element element?]) string?]{
@(mdn-bar "Element: tagName property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/tagName")
Returns the element tag name.
}

@defproc[(element-local-name [element element?]) string?]{
@(mdn-bar "Element: localName property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/localName")
Returns the element local name.
}

@defproc[(element-namespace-uri [element element?]) (or/c #f string?)]{
@(mdn-bar "Element: namespaceURI property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/namespaceURI")
Returns the element namespace URI, or @racket[#f] if there is none.
}

@defproc[(element-prefix [element element?]) (or/c #f string?)]{
@(mdn-bar "Element: prefix property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/prefix")
Returns the element namespace prefix, or @racket[#f] if there is none.
}

@defproc[(element-is-connected? [element element?]) boolean?]{
@(mdn-bar "Element: isConnected property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/isConnected")
Reports whether the element is connected to a document.
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

@defproc[(element-get-attribute-ns [element element?]
                                   [ns (or/c string? symbol?)]
                                   [name (or/c string? symbol?)]) (or/c #f string?)]{
@(mdn-bar "Element: getAttributeNS() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttributeNS")
Returns the value of a namespaced attribute.
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

@defproc[(element-query-selector [element element?]
                                 [selector (or/c string? symbol?)]) (or/c #f element?)]{
@(mdn-bar "Element: querySelector() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/querySelector")
Returns the first matching descendant as a wrapped element, or @racket[#f]
if there is no match.
}

@section{Element Class Lists}

The @racket[classList] property is exposed as a wrapped browser
@racket[dom-token-list] value so class tokens can be inspected and
updated from WebRacket.

@defstruct[dom-token-list ([raw external/raw])]{
Wraps a browser DOMTokenList object.
}

@defproc[(element-class-list [element element?]) (or/c #f dom-token-list?)]{
@(mdn-bar "Element: classList property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/classList")
Returns the element's class list, or @racket[#f] if one is not available.
}

@defproc[(dom-token-list-value [class-list dom-token-list?]) (or/c #f string?)]{
@(mdn-bar "DOMTokenList: value property"
          "https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList/value")
Returns the class list as a single string.
}

@defproc[(dom-token-list-length [class-list dom-token-list?]) exact-nonnegative-integer?]{
@(mdn-bar "DOMTokenList: length property"
          "https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList/length")
Returns the number of class tokens.
}

@defproc[(dom-token-list-item [class-list dom-token-list?]
                              [index exact-nonnegative-integer?]) (or/c #f string?)]{
@(mdn-bar "DOMTokenList: item() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList/item")
Returns the token at @racket[index], or @racket[#f] if the index is out of range.
}

@defproc[(dom-token-list-contains? [class-list dom-token-list?]
                                   [token (or/c string? symbol?)]) boolean?]{
@(mdn-bar "DOMTokenList: contains() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList/contains")
Reports whether the token is present in the class list.
}

@defproc[(dom-token-list-add! [class-list dom-token-list?]
                              [token (or/c string? symbol?)] ...)
         void?]{
@(mdn-bar "DOMTokenList: add() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList/add")
Adds one or more tokens to the class list. Symbols are accepted and normalized to strings.
}

@defproc[(dom-token-list-remove! [class-list dom-token-list?]
                                 [token (or/c string? symbol?)] ...)
         void?]{
@(mdn-bar "DOMTokenList: remove() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList/remove")
Removes one or more tokens from the class list. Symbols are accepted and normalized to strings.
}

@defproc[(dom-token-list-toggle! [class-list dom-token-list?]
                                 [token (or/c string? symbol?)]
                                 [force (or/c boolean? procedure?) #f]) boolean?]{
@(mdn-bar "DOMTokenList: toggle() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList/toggle")
Toggles a token in the class list. Use @racket[#f] to omit @racket[force]; if you need a literal @racket[#f], pass a thunk such as @racket[(lambda () #f)].
}

@defproc[(dom-token-list-replace! [class-list dom-token-list?]
                                  [old-token (or/c string? symbol?)]
                                  [new-token (or/c string? symbol?)]) boolean?]{
@(mdn-bar "DOMTokenList: replace() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList/replace")
Replaces one token in the class list with another.
}

@section{Element Shadow Roots}

Shadow roots are the browser objects that hold shadow DOM content. The
wrapper keeps them on the WebRacket side so element code can work with
them directly.

@defstruct[shadow-root ([raw external/raw])]{
Wraps a browser ShadowRoot object.
}

@defproc[(element-shadow-root [element element?]) (or/c #f shadow-root?)]{
@(mdn-bar "Element: shadowRoot property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/shadowRoot")
Returns the element's shadow root, or @racket[#f] if it has none.
}

@defproc[(element-attach-shadow! [element element?]
                                 [options any/c]) shadow-root?]{
@(mdn-bar "Element: attachShadow() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/attachShadow")
Attaches a shadow root to the element and returns the wrapped browser
ShadowRoot value. If a raw @racket[external] is passed as @racket[options],
it should be a browser @racketid[ShadowRootInit] value.
}

@defproc[(shadow-root-host [shadow-root shadow-root?]) element?]{
@(mdn-bar "ShadowRoot: host property"
          "https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot/host")
Returns the host element for the shadow root.
}

@defproc[(shadow-root-mode [shadow-root shadow-root?]) string?]{
@(mdn-bar "ShadowRoot: mode property"
          "https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot/mode")
Returns the shadow root mode string.
}

@defproc[(shadow-root-delegates-focus? [shadow-root shadow-root?]) boolean?]{
@(mdn-bar "ShadowRoot: delegatesFocus property"
          "https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot/delegatesFocus")
Reports whether the shadow root delegates focus.
}

@section{Element Content and Collections}

These helpers cover element children, HTML content, and the browser
collection objects that expose groups of descendants.

@defstruct[node-list ([raw external/raw])]{
Wraps a browser NodeList object.
}

@defproc[(node-list-length [node-list node-list?]) exact-nonnegative-integer?]{
@(mdn-bar "NodeList: length property"
          "https://developer.mozilla.org/en-US/docs/Web/API/NodeList/length")
Returns the number of nodes in the NodeList.
}

@defproc[(node-list-item [node-list node-list?]
                         [index exact-nonnegative-integer?]) (or/c #f node?)]{
@(mdn-bar "NodeList: item() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/NodeList/item")
Returns the node at the given index, or @racket[#f] if there is none.
}

@defstruct[html-collection ([raw external/raw])]{
Wraps a browser HTMLCollection object.
}

@defproc[(html-collection-length [collection html-collection?]) exact-nonnegative-integer?]{
@(mdn-bar "HTMLCollection: length property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLCollection/length")
Returns the number of elements in the HTMLCollection.
}

@defproc[(html-collection-item [collection html-collection?]
                               [index exact-nonnegative-integer?]) (or/c #f element?)]{
@(mdn-bar "HTMLCollection: item() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLCollection/item")
Returns the element at the given index, or @racket[#f] if there is none.
}

@defproc[(html-collection-named-item [collection html-collection?]
                                     [name (or/c string? symbol?)]) (or/c #f element?)]{
@(mdn-bar "HTMLCollection: namedItem() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLCollection/namedItem")
Returns the named element, or @racket[#f] if there is none.
}

@defproc[(element-children [element element?]) (or/c #f html-collection?)]{
@(mdn-bar "Element: children property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/children")
Returns the child elements as a wrapped HTMLCollection.
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

@defproc[(element-scroll-top [element element?]) real?]{
@(mdn-bar "Element: scrollTop property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollTop")
Returns the element's vertical scroll offset.
}

@defproc[(element-set-scroll-top! [element element?]
                                  [value real?]) void?]{
@(mdn-bar "Element: scrollTop property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollTop")
Sets the element's vertical scroll offset.
}

@defproc[(element-scroll-left [element element?]) real?]{
@(mdn-bar "Element: scrollLeft property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollLeft")
Returns the element's horizontal scroll offset.
}

@defproc[(element-set-scroll-left! [element element?]
                                   [value real?]) void?]{
@(mdn-bar "Element: scrollLeft property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollLeft")
Sets the element's horizontal scroll offset.
}

@defproc[(element-scroll-width [element element?]) exact-nonnegative-integer?]{
@(mdn-bar "Element: scrollWidth property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollWidth")
Returns the element's scroll width.
}

@defproc[(element-scroll-height [element element?]) exact-nonnegative-integer?]{
@(mdn-bar "Element: scrollHeight property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollHeight")
Returns the element's scroll height.
}

@defproc[(element-client-width [element element?]) exact-nonnegative-integer?]{
@(mdn-bar "Element: clientWidth property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/clientWidth")
Returns the element's client width.
}

@defproc[(element-client-height [element element?]) exact-nonnegative-integer?]{
@(mdn-bar "Element: clientHeight property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/clientHeight")
Returns the element's client height.
}

@defproc[(element-offset-width [element element?]) exact-nonnegative-integer?]{
@(mdn-bar "Element: offsetWidth property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/offsetWidth")
Returns the element's offset width.
}

@defproc[(element-offset-height [element element?]) exact-nonnegative-integer?]{
@(mdn-bar "Element: offsetHeight property"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/offsetHeight")
Returns the element's offset height.
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

@defproc[(element-get-client-rects [element element?]) dom-rect-list?]{
@(mdn-bar "Element: getClientRects() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getClientRects")
Returns the element's client rectangles as a wrapped DOMRectList.
}

@defproc[(element-query-selector-all [element element?]
                                     [selector (or/c string? symbol?)]) (or/c #f node-list?)]{
@(mdn-bar "Element: querySelectorAll() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/querySelectorAll")
Returns all matching descendants as a wrapped NodeList.
}

@defproc[(element-get-elements-by-class-name [element element?]
                                             [class-name (or/c string? symbol?)]) (or/c #f html-collection?)]{
@(mdn-bar "Element: getElementsByClassName() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getElementsByClassName")
Returns descendant elements with the matching class name as a wrapped HTMLCollection.
}

@defproc[(element-get-elements-by-tag-name [element element?]
                                           [tag-name (or/c string? symbol?)]) (or/c #f html-collection?)]{
@(mdn-bar "Element: getElementsByTagName() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getElementsByTagName")
Returns descendant elements with the matching tag name as a wrapped HTMLCollection.
}

@defproc[(element-get-elements-by-tag-name-ns [element element?]
                                              [ns (or/c string? symbol?)]
                                              [tag-name (or/c string? symbol?)]) (or/c #f html-collection?)]{
@(mdn-bar "Element: getElementsByTagNameNS() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getElementsByTagNameNS")
Returns descendant elements with the matching namespaced tag name as a wrapped HTMLCollection.
}

@section{Element Insertion}

These helpers insert nodes, text, or HTML around an element. When a raw
@racket[external] is passed as a node, it should be a browser
@racketid[Node] value. Wrapped @racket[node], @racket[element], and
@racket[text] values are also accepted.

@defproc[(element-append! [element element?] [child any/c]) void?]{
@(mdn-bar "Element: append() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/append")
Appends a node or text to an element.
}

@defproc[(element-prepend! [element element?] [child any/c]) void?]{
@(mdn-bar "Element: prepend() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/prepend")
Prepends a node or text to an element.
}

@defproc[(element-before! [element element?] [sibling any/c]) void?]{
@(mdn-bar "Element: before() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/before")
Inserts a node or text before an element.
}

@defproc[(element-after! [element element?] [sibling any/c]) void?]{
@(mdn-bar "Element: after() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/after")
Inserts a node or text after an element.
}

@defproc[(element-insert-adjacent-element! [element element?]
                                           [position (or/c string? symbol?)]
                                           [child any/c]) (or/c #f element?)]{
@(mdn-bar "Element: insertAdjacentElement() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentElement")
Inserts an element relative to another element.
}

@defproc[(element-insert-adjacent-html! [element element?]
                                        [position (or/c string? symbol?)]
                                        [html (or/c string? symbol?)]) void?]{
@(mdn-bar "Element: insertAdjacentHTML() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentHTML")
Inserts HTML relative to an element.
}

@defproc[(element-insert-adjacent-text! [element element?]
                                        [position (or/c string? symbol?)]
                                        [text (or/c string? symbol?)]) void?]{
@(mdn-bar "Element: insertAdjacentText() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentText")
Inserts text relative to an element.
}

@section{Element Mutation and Scrolling}

These helpers use the Rackety @racket[element-] prefix and keep the
DOM mutation operations on the WebRacket side.

@defproc[(element-remove! [element element?]) void?]{
@(mdn-bar "Element: remove() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/remove")
Removes the element from the DOM.
}

@defproc[(element-replace-children! [element element?]
                                    [child any/c]) void?]{
@(mdn-bar "Element: replaceChildren() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/replaceChildren")
Replaces the child list with one node.
}

@defproc[(element-replace-with! [element element?]
                                [sibling any/c]) void?]{
@(mdn-bar "Element: replaceWith() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/replaceWith")
Replaces the element with another node.
}

@defproc[(element-request-fullscreen [element element?]) external/raw]{
@(mdn-bar "Element: requestFullscreen() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/requestFullscreen")
Requests fullscreen for the element and returns the raw browser result.
}

@defproc[(element-request-pointer-lock [element element?]) void?]{
@(mdn-bar "Element: requestPointerLock() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/requestPointerLock")
Requests pointer lock for the element.
}

@defproc[(element-scroll! [element element?]
                          [x real?]
                          [y real?]) void?]{
@(mdn-bar "Element: scroll() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scroll")
Scrolls the element to an absolute position.
}

@defproc[(element-scroll-by! [element element?]
                             [x real?]
                             [y real?]) void?]{
@(mdn-bar "Element: scrollBy() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollBy")
Scrolls the element by a relative offset.
}

@defproc[(element-scroll-to! [element element?]
                             [x real?]
                             [y real?]) void?]{
@(mdn-bar "Element: scrollTo() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollTo")
Scrolls the element to an absolute position.
}

@section{Element Style and Animation}

These helpers return wrapped browser objects when the browser API does.
The attribute-node helpers use wrapped browser @racket[attr] values,
which are created by the document library.

@defstruct[computed-style-map ([raw external/raw])]{
Wraps a browser ComputedStyleMap object.
}

@defstruct[animation ([raw external/raw])]{
Wraps a browser Animation object.
}

@defproc[(element-computed-style-map [element element?]) computed-style-map?]{
@(mdn-bar "Element: computedStyleMap() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/computedStyleMap")
Returns the wrapped computed style map for the element.
}

@defproc[(element-get-animations [element element?]) vector?]{
@(mdn-bar "Element: getAnimations() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getAnimations")
Returns the animations affecting the element as a WebRacket vector of wrapped animations.
}

@defproc[(element-attach-shadow! [element element?] [options any/c]) shadow-root?]{
@(mdn-bar "Element: attachShadow() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/attachShadow")
Attaches a shadow root to the element and returns the wrapped browser ShadowRoot value.
}

@defproc[(element-animate [element element?]
                          [keyframes any/c]
                          [options any/c #f]) animation?]{
@(mdn-bar "Element: animate() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/animate")
Starts an animation on the element and returns a wrapped browser animation object.
}

@defproc[(element-get-attribute-node [element element?]
                                     [name (or/c string? symbol?)]) (or/c #f attr?)]{
@(mdn-bar "Element: getAttributeNode() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttributeNode")
Returns the attribute node for the named attribute.
}

@defproc[(element-get-attribute-node-ns [element element?]
                                        [ns (or/c string? symbol?)]
                                        [name (or/c string? symbol?)]) (or/c #f attr?)]{
@(mdn-bar "Element: getAttributeNodeNS() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttributeNodeNS")
Returns the namespaced attribute node for the named attribute.
}

@defproc[(element-set-attribute-node! [element element?]
                                      [node (or/c attr? external?)]) (or/c #f attr?)]{
@(mdn-bar "Element: setAttributeNode() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttributeNode")
Attaches an attribute node to the element. If a raw @racket[external]
is passed as the node, it should be a browser @racketid[Attr] value.
}

@defproc[(element-set-attribute-node-ns! [element element?]
                                         [node (or/c attr? external?)]) (or/c #f attr?)]{
@(mdn-bar "Element: setAttributeNodeNS() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttributeNodeNS")
Attaches a namespaced attribute node to the element. If a raw
@racket[external] is passed as the node, it should be a browser
@racketid[Attr] value.
}

@defproc[(element-remove-attribute-node! [element element?]
                                         [node (or/c attr? external?)]) (or/c #f attr?)]{
@(mdn-bar "Element: removeAttributeNode() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/removeAttributeNode")
Removes an attribute node from the element. If a raw @racket[external]
is passed as the node, it should be a browser @racketid[Attr] value.
}

@defproc[(element-has-pointer-capture? [element element?]
                                       [pointer-id exact-nonnegative-integer?]) boolean?]{
@(mdn-bar "Element: hasPointerCapture() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/hasPointerCapture")
Reports whether the element currently has pointer capture for the given pointer id.
}

@defproc[(element-set-pointer-capture! [element element?]
                                      [pointer-id exact-nonnegative-integer?]) void?]{
@(mdn-bar "Element: setPointerCapture() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/setPointerCapture")
Captures pointer events for the given pointer id.
}

@defproc[(element-release-pointer-capture! [element element?]
                                           [pointer-id exact-nonnegative-integer?]) void?]{
@(mdn-bar "Element: releasePointerCapture() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/releasePointerCapture")
Releases pointer capture for the given pointer id.
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
(element-set-attribute! card "class" "card")

(code:comment "Insert the card into the page body.")
(element-append! (document-body) card)

(code:comment "Measure where the card lands on the page.")
(define rect
  (element-get-bounding-client-rect card))

(code:comment "Bring the card into view if needed.")
(element-scroll-into-view! card #t)
]

When you only need a few common operations, the most useful entry points
are @racket[element-append!], @racket[element-set-attribute!],
@racket[element-get-bounding-client-rect], @racket[element-id],
@racket[element-children], @racket[element-inner-html], and
@racket[element-scroll-into-view!].

@defproc[(element-append! [parent element?] [child (or/c element? external?)]) void?]{
@(mdn-bar "Node: appendChild() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Node/appendChild")
Appends a child node.

If an @racket[external] is passed as an argument, it should be a browser
@racketid[Node] value, such as an element or a text node. Wrapped
@racket[element] and @racket[text] values are also accepted.
}

@defproc[(element-set-attribute! [element element?]
                                 [name (or/c string? symbol?)]
                                 [value (or/c string? symbol?)]) void?]{
@(mdn-bar "Element: setAttribute() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute")
Sets an attribute on an element.
}

@defproc[(element-get-attribute [element element?]
                                [name (or/c string? symbol?)]) (or/c #f string?)]{
@(mdn-bar "Element: getAttribute() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttribute")
Returns an attribute value or @racket[#f] if the attribute is missing.
}

@defproc[(element-set-attribute-ns! [element element?]
                                    [ns (or/c string? symbol?)]
                                    [name (or/c string? symbol?)]
                                    [value (or/c string? symbol?)]) void?]{
@(mdn-bar "Element: setAttributeNS() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttributeNS")
Sets a namespaced attribute on an element.
}

@defproc[(element-toggle-attribute! [element element?]
                                    [name (or/c string? symbol?)]
                                    [force (or/c boolean? procedure?) #f]) boolean?]{
@(mdn-bar "Element: toggleAttribute() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/toggleAttribute")
Toggles an attribute or forces a specific state when @racket[force] is provided.
}

@defproc[(element-scroll-into-view! [element element?] [align-to-top? boolean? #t]) void?]{
@(mdn-bar "Element: scrollIntoView() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView")
Scrolls ancestors until the element is visible.
}

@defproc[(element-get-bounding-client-rect [element element?]) dom-rect?]{
@(mdn-bar "Element: getBoundingClientRect() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect")
Returns the element's bounding rectangle.
}

@defproc[(element-get-client-rects [element element?]) dom-rect-list?]{
@(mdn-bar "Element: getClientRects() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/getClientRects")
Returns the element's client rectangles as a wrapped DOMRectList.
}

@defproc[(element-query-selector-all [element element?]
                                     [selector (or/c string? symbol?)]) node-list?]{
@(mdn-bar "Element: querySelectorAll() method"
          "https://developer.mozilla.org/en-US/docs/Web/API/Element/querySelectorAll")
Returns all matching descendants as a wrapped NodeList.
}
