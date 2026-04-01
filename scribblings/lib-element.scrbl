#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[element]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib element (lib "libs/element.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[element] library provides checked wrappers for common DOM
element operations.

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
