#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          )

@title{Library: dom}
@declare-exporting[(lib "scribblings/lib-dom-labels.rkt" "webracket")]

@(how-to-require include-lib dom (lib "libs/dom.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @tt{dom} library brings together browser-facing libraries for:

@tabular[
 #:style 'boxed
 (list
  (list @bold{Library} @bold{What it is for})
  (list
   @hyperlink["Library__window.html"]{@tt{window}}
   "Window-level APIs for the browser context, dialogs, timers, and navigation.")
  (list
   @hyperlink["Library__document.html"]{@tt{document}}
   "Document accessors for DOM trees, selectors, and element creation.")
  (list
   @hyperlink["Library__element.html"]{@tt{element}}
   "Element wrappers for inspecting and mutating node content and attributes.")
  (list
   @hyperlink["Library__event.html"]{@tt{event}}
   "Event registration and handler support for user input and browser events.")
  (list
   @hyperlink["Library__domrect.html"]{@tt{domrect}}
   "Geometry values for layout and dimension metadata like DOMRect.")
  (list
   @hyperlink["Library__canvas.html"]{@tt{canvas}}
   "Canvas context access and drawing helpers.")
  (list
   @hyperlink["Library__media.html"]{@tt{media}}
   "Media playback and streaming support.")
  (list
   @hyperlink["Library__image.html"]{@tt{image}}
   "Image resource loading and image-element helpers.")
  (list
   @hyperlink["Library__fetch.html"]{@tt{fetch}}
   "HTTP request APIs for network access from browser programs.")
  (list
   @hyperlink["Library__storage.html"]{@tt{storage}}
   "Browser storage access for key/value persistence and retrieval.")
  (list
   @hyperlink["Library__indexed-db.html"]{@tt{indexed-db}}
   "Persistent structured client-side storage for larger data sets."))
]

Use @tt{dom} when you want a single import point for browser DOM work.
