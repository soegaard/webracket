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
Appends a child node.
}

@defproc[(set-attribute! [element external?] [name string?] [value string?]) void?]{
Sets an attribute on an element.
}

@defproc[(scroll-into-view! [element external?] [align-to-top? any/c #t]) void?]{
Scrolls ancestors until the element is visible.
}
