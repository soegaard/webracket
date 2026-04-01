#lang scribble/manual

@(require scribble/manual
          "webracket-scribble-utils.rkt"
          (for-label (lib "scribblings/dom-family-labels.rkt" "webracket")))

@title{Library: @racketid[image]}
@declare-exporting[(lib "scribblings/dom-family-labels.rkt" "webracket")]

@(how-to-require include-lib image (lib "libs/image.rkt"))
@(compile-option-bar "Compile option: " "--ffi dom")

The @racket[image] library wraps HTMLImageElement creation and common
image properties.

@defproc[(image-new [width any/c (void)] [height any/c (void)]) external/raw]{
@(mdn-bar "HTMLImageElement: constructor"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/Image")
Creates a new image element.
}

@defproc[(image-src [img external?]) string?]{
@(mdn-bar "HTMLImageElement: src property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/src")
Returns the current image source URL.
}

@defproc[(image-complete? [img external?]) boolean?]{
@(mdn-bar "HTMLImageElement: complete property"
          "https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/complete")
Reports whether the image has finished loading.
}
