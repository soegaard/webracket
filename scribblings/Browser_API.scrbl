#lang scribble/manual

@title{FFI Reference}

This chapter documents the low-level bindings provided by @tt{ffi/*.ffi}.

The intention is to build Rackety libraries on top of these low-level bindings.
When these libraries are implemented, the hope is that most users
won't need to consult this chapter.

However, there are currently low-level bindings for more JavaScript APIs than
we have had time to wrap in Rackety libraries.

For the time being, these short, reference-style listings are better than
nothing. The descriptions of each binding are intentionally brief. They serve
two purposes: to jog your memory when you know a name, and to make it possible
to use text search effectively on the page.

Consult the MDN manual for in-depth explanations of the bindings.

@section{The WebRacket / JavaScript Bridge}

When a WebRacket program calls a JavaScript function,
the following dance happens (assuming a single return value):

1. The arguments (WebRacket values) are converted to JavaScript values.
2. Control is transferred to the JavaScript side.
3. The function is called and returns a JavaScript value.
4. The JavaScript value is converted into a WebRacket value.
5. Control returns to the WebRacket caller.

@margin-note{It is worth mentioning that the value conversion bridge isn't
specific to WebRacket - all languages that compile to WebAssembly
need to do value conversion.}
A few details are omitted (arity checking and exception handling)
to make the discussion simpler.

Whenever values cross the bridge in either direction, they must be converted.
Unfortunately, there is no single conversion strategy that works best in all cases.

Therefore it is important to choose the right conversion type,
when specifying types in static bindings. Likewise, care must
be taken when calling invoking JavaScript methods dynamically.

Let's begin with the conventions for return value conversion.


@section{Conversion of Return Values}

The @racket[value] return convention means "use the normal bridge
conversion". This convention is also used for @racket[js-send].

When a JavaScript result comes back into WebRacket, this will
convert the JavaScript value to an ordinary WebRacket
value when at all possible.

The bridge recognizes these common JavaScript-to-WebRacket conversions:

@tabular[
 (list (list @bold{JavaScript value} @bold{WebRacket result})
       (list @tt{number}      "Exact integers stay exact; non-integers become inexact numbers.")
       (list @tt{string}      "Racket string.")
       (list @tt{boolean}     "#t or #f")
       (list @tt{array}       "Vector, with elements converted recursively.")
       (list @tt{null}        "null")
       (list @tt{undefined}   "void")
       (list @tt{object}      "Usually becomes an @racket[external] value unless a more specific conversion is available."))]

Exceptions to the generic @tt{object} fallback include JavaScript arrays,
@tt{Uint8Array} values, wrapper objects that represent numbers, and the
bridge's own tagged sentinels for characters and pairs. Those cases are
decoded before the result reaches the final external-value fallback.

The conversion is recursive, so cyclic data structures are not
supported and can overflow the stack during encoding.

If a value cannot be converted, the bridge raises an error.

Use @racket[value] when the binding should behave like a regular
WebRacket function. Use one of the @racket[extern]-based conventions
when you need to keep the raw JavaScript object or when a result may be
@racket[#f] or @racket[undefined] instead of a converted WebRacket
value.



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
@include-section["ffi-window.scrbl"]
@include-section["ffi-performance.scrbl"]
@include-section["ffi-document.scrbl"]
@include-section["ffi-event.scrbl"]
@include-section["ffi-domrect.scrbl"]
@include-section["ffi-element.scrbl"]
@include-section["ffi-canvas.scrbl"]
@include-section["ffi-media.scrbl"]
@include-section["ffi-image.scrbl"]
@include-section["ffi-audio.scrbl"]
@include-section["ffi-console.scrbl"]
@include-section["ffi-websocket.scrbl"]
