#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/raw-accessors-labels.rkt" "webracket")))

@title[#:tag "raw-accessors"]{Raw Accessors}
@declare-exporting[(lib "scribblings/raw-accessors-labels.rkt" "webracket")]

This appendix collects the raw bridge accessors for the core wrapper
libraries in one place. Use the checked wrappers by default and reach
for these accessors only when you explicitly need the underlying browser
object.

@section{Window Raw Accessor}

@defproc[(dom-window-raw [win any/c]) external/raw]{
Returns the wrapped browser Window object.
}

@defproc[(window-document-info-raw [doc any/c]) external/raw]{
Returns the wrapped browser Document object stored in the Window wrapper.
}

@defproc[(window-location-info-raw [loc any/c]) external/raw]{
Returns the wrapped browser Location object stored in the Window wrapper.
}

@defproc[(media-query-list-raw [query any/c]) external/raw]{
Returns the wrapped browser MediaQueryList object.
}

@defproc[(css-style-declaration-raw [style any/c]) external/raw]{
Returns the wrapped browser CSSStyleDeclaration object.
}

@section{Document Raw Accessors}

@defproc[(dom-document-raw [doc any/c]) external/raw]{
Returns the wrapped browser Document object.
}

@defproc[(dom-text-raw [node any/c]) external/raw]{
Returns the wrapped browser Text node.
}

@defproc[(dom-node-raw [node any/c]) external/raw]{
Returns the wrapped browser Node object.
}

@defproc[(dom-attr-raw [node any/c]) external/raw]{
Returns the wrapped browser Attr node.
}

@defproc[(dom-selection-raw [sel any/c]) external/raw]{
Returns the wrapped browser Selection object.
}

@section{Element Raw Accessors}

@defproc[(dom-element-raw [elem any/c]) external/raw]{
Returns the wrapped browser Element object.
}

@defproc[(dom-token-list-raw [class-list any/c]) external/raw]{
Returns the wrapped browser DOMTokenList object.
}

@defproc[(dom-shadow-root-raw [shadow-root any/c]) external/raw]{
Returns the wrapped browser ShadowRoot object.
}

@defproc[(dom-node-list-raw [node-list any/c]) external/raw]{
Returns the wrapped browser NodeList object.
}

@defproc[(html-collection-raw [collection any/c]) external/raw]{
Returns the wrapped browser HTMLCollection object.
}

@defproc[(dom-rect-list-raw [rect-list any/c]) external/raw]{
Returns the wrapped browser DOMRectList object.
}

@defproc[(dom-rect-raw [rect any/c]) external/raw]{
Returns the wrapped browser DOMRect object.
}

@defproc[(dom-computed-style-map-raw [style-map any/c]) external/raw]{
Returns the wrapped browser ComputedStyleMap object.
}

@defproc[(dom-animation-raw [animation any/c]) external/raw]{
Returns the wrapped browser Animation object.
}

@section{Array Raw Accessor}

@defproc[(array-raw [arr any/c]) external/raw]{
Returns the wrapped browser Array object.
}

@section{Iterator Raw Accessor}

@defproc[(iterator-raw [iter any/c]) external/raw]{
Returns the wrapped browser Iterator object stored inside @racket[iter].
}

@section{Audio Raw Accessor}

@defproc[(audio-listener-raw [listener any/c]) external/raw]{
Returns the wrapped browser AudioListener object.
}

@section{Performance Raw Accessor}

@defproc[(performance-event-count-map-raw [counts any/c]) external/raw]{
Returns the wrapped browser EventCounts object.
}

@section{WebSocket Raw Accessor}

@defproc[(websocket-raw [ws any/c]) external/raw]{
Returns the wrapped browser WebSocket object.
}
