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
Creates a new image element.
}

@defproc[(image-src [img external?]) string?]{
Returns the current image source URL.
}

@defproc[(image-complete? [img external?]) boolean?]{
Reports whether the image has finished loading.
}
