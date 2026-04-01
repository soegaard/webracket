#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[document]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib document (lib "libs/document.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[document] library provides checked wrappers for the current
document, element lookup, and selector queries.

@defproc[(document) external/raw]{
@(mdn-bar "Document"
          "https://developer.mozilla.org/en-US/docs/Web/API/Document")
Returns the current document object.
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
