#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-dom-labels.rkt" "webracket")))

@title{Library: dom}

@(how-to-require include-lib dom (lib "libs/dom.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @tt{dom} library is the top-level Rackety facade for the split DOM
wrapper surface. It reexports the family libraries for windows, documents,
events, geometry, canvas, media, and images.

Use @tt{dom} when you want one import point for the browser DOM surface.

@section{DOM Facade}

@defthing[dom any/c]{
The named top-level DOM facade library.
}
