#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/lib-image-labels.rkt" "webracket")))

@title{Library: @racketid[image]}

@(how-to-require include-lib image (lib "libs/image.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[image] library is the checked wrapper for HTML image
elements.

Images are the browser's built-in way to load and display pictures. An
image element knows its source URL, its loading state, and its natural
size once the browser has fetched it.

Use @racket[image] when you want to:

@itemlist[
  @item{create a new image element}
  @item{set the source URL or alternative text}
  @item{check whether the image has finished loading}
  @item{read the browser's current source URL}
]

The @racket[image] library wraps HTMLImageElement creation and common
image properties.

String-like image properties accept either strings or symbols.
Optional size arguments use @racket[#f] to mean that the argument is
omitted.

@section{Image Quick Start}

Start by creating an image element, pointing it at a source, and then
checking whether the browser has finished loading it.

@racketblock[
(code:comment "Include the checked image wrapper library.")
(include-lib image)

(code:comment "Create a new image element.")
(define img
  (image-new))

(code:comment "Set the image source and a short description.")
(image-set-src! img "/images/logo.png")
(image-set-alt! img "Project logo")

(code:comment "Ask whether the browser has finished loading it.")
(image-complete? img)
]

The quick start shows the main pattern: create the image, configure it,
and then read the browser's loading state.

@section{Image Example}

This example shows how to prepare an image for display and then inspect
the browser's view of it.

@racketblock[
(code:comment "Include the image wrapper library.")
(include-lib image)

(code:comment "Create an image element and configure it before use.")
(define img
  (image-new))
(image-set-src! img "/images/logo.png")
(image-set-alt! img "Project logo")

(code:comment "Read the current source URL and loading state.")
(define current
  (image-src img))
(define loaded?
  (image-complete? img))

(void current loaded?)
]

If you just need the basics, the main entry points are
@racket[image-new], @racket[image-src], @racket[image-set-src!],
@racket[image-set-alt!], and @racket[image-complete?].

@defproc[(image-new [width any/c #f] [height any/c #f]) external/raw]{
@(mdn-bar "HTMLImageElement: constructor"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/Image")
Creates a new image element.
}

@defproc[(image-src [img external?]) string?]{
@(mdn-bar "HTMLImageElement: src property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/src")
The raw @racket[img] argument should be a browser
@racketid[HTMLImageElement] value. Returns the current image source URL.
}

@defproc[(image-complete? [img external?]) boolean?]{
@(mdn-bar "HTMLImageElement: complete property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/complete")
The raw @racket[img] argument should be a browser
@racketid[HTMLImageElement] value. Reports whether the image has finished loading.
}

@defproc[(image-set-src! [img external?] [src (or/c string? symbol?)]) void?]{
@(mdn-bar "HTMLImageElement: src property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/src")
The raw @racket[img] argument should be a browser
@racketid[HTMLImageElement] value. Sets the image source URL.
}

@defproc[(image-set-alt! [img external?] [alt (or/c string? symbol?)]) void?]{
@(mdn-bar "HTMLImageElement: alt property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/alt")
The raw @racket[img] argument should be a browser
@racketid[HTMLImageElement] value. Sets the alternative text for the image.
}
