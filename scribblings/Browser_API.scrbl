#lang scribble/manual

@title{Browser API}

This chapter lists the browser bindings in reference-manual style.

@section{FFI Return Types}

FFI signatures in @tt{.ffi} files use a small set of return-type tags.
In this manual, entries present those tags with user-facing wording:

@itemlist[
  @item{@tt{extern/raw}: shown as @racket[external]}
  @item{@tt{extern}: shown as @racket[(or/c #f external)]}
  @item{@tt{extern/undefined}: shown as @racket[(or/c undefined external)]}
  @item{@tt{extern/nullish}: shown as @racket[(or/c #f undefined external)]}
  @item{@tt{value}: generic WebRacket value}
  @item{@tt{string}: string result}
  @item{@tt{boolean}: boolean result}
  @item{@tt{i32}: 32-bit signed integer}
  @item{@tt{u32}: 32-bit unsigned integer}
  @item{@tt{f64}: 64-bit floating-point number}
  @item{@tt{()}: no result (void)}
]

@include-section["DOM.scrbl"]
