#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/raw-accessors-labels.rkt" "webracket")))

@title{Raw Accessors}
@declare-exporting[(lib "scribblings/raw-accessors-labels.rkt" "webracket")]

This appendix collects the raw bridge accessors for the core wrapper
libraries in one place. Use the checked wrappers by default and reach
for these accessors only when you explicitly need the underlying browser
object.

@section{Window Raw Accessor}

@defproc[(window-raw [win any/c]) external/raw]{
Returns the wrapped browser Window object.
}

@section{Document Raw Accessors}

@defproc[(document-raw [doc any/c]) external/raw]{
Returns the wrapped browser Document object.
}

@defproc[(text-raw [node any/c]) external/raw]{
Returns the wrapped browser Text node.
}

@defproc[(node-raw [node any/c]) external/raw]{
Returns the wrapped browser Node object.
}

@defproc[(attr-raw [node any/c]) external/raw]{
Returns the wrapped browser Attr node.
}

@defproc[(selection-raw [sel any/c]) external/raw]{
Returns the wrapped browser Selection object.
}

@section{Element Raw Accessors}

@defproc[(element-raw [elem any/c]) external/raw]{
Returns the wrapped browser Element object.
}

@defproc[(dom-token-list-raw [class-list any/c]) external/raw]{
Returns the wrapped browser DOMTokenList object.
}

@defproc[(shadow-root-raw [shadow-root any/c]) external/raw]{
Returns the wrapped browser ShadowRoot object.
}

@defproc[(node-list-raw [node-list any/c]) external/raw]{
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

@defproc[(computed-style-map-raw [style-map any/c]) external/raw]{
Returns the wrapped browser ComputedStyleMap object.
}

@defproc[(animation-raw [animation any/c]) external/raw]{
Returns the wrapped browser Animation object.
}

@section{Iterator Raw Accessor}

@defproc[(iterator-raw [iter any/c]) external/raw]{
Returns the wrapped browser Iterator object stored inside @racket[iter].
}

@section{Audio Raw Accessor}

@defproc[(audio-listener-raw [listener any/c]) external/raw]{
Returns the wrapped browser AudioListener object.
}
