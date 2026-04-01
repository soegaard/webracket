#lang scribble/manual

@(require scribble/manual
          scribble/example
          scribble-tools
          racket/string
          "../structs.rkt"
          "ffi-doc.rkt")

@(define (mdn path [label #f])
   (hyperlink (string-append "https://developer.mozilla.org/en-US/docs/Web/API/" path)
              (or label path)))

@(define dom-ffi-index (load-ffi-doc-index "ffi/dom.ffi"))
@(define dom-ffi-docs (load-ffi-docs "ffi/dom.ffi"))
@(define dom-doc-specs
   (for/list ([fd (in-list dom-ffi-docs)])
     (define name (foreign-racket-name (foreign-doc-foreign fd)))
     (define arg-contracts (ffi-doc-argument-contracts dom-ffi-index name))
     (define args
       (for/list ([c (in-list arg-contracts)]
                  [i (in-naturals 1)])
         (list (string->symbol (format "arg~a" i)) c)))
     (define result (ffi-doc-result-contract dom-ffi-index name))
     (list name args result)))
@(define dom-documented-bindings (map car dom-doc-specs))
@(define audio-ffi-index (load-ffi-doc-index "ffi/audio.ffi"))
@(define audio-ffi-docs (load-ffi-docs "ffi/audio.ffi"))
@(define audio-doc-specs
   (for/list ([fd (in-list audio-ffi-docs)])
     (define name (foreign-racket-name (foreign-doc-foreign fd)))
     (define arg-contracts (ffi-doc-argument-contracts audio-ffi-index name))
     (define args
       (for/list ([c (in-list arg-contracts)]
                  [i (in-naturals 1)])
         (list (string->symbol (format "arg~a" i)) c)))
     (define result (ffi-doc-result-contract audio-ffi-index name))
     (list name args result)))
@(define audio-documented-bindings (map car audio-doc-specs))
@(define (audio-desc name)
   (or (ffi-doc-description audio-ffi-index name)
       (ffi-doc-default-description audio-ffi-index name)))
@(define (audio-sig name)
   (ffi-doc-signature-line audio-ffi-index name "audio.ffi"))
@(define (audio-return-line name)
   (ffi-doc-return-note audio-ffi-index name))
@(define (audio-mdn-link name)
   (define path (ffi-doc-mdn-path/default audio-ffi-index name))
   (mdn path (mdn-label path)))
@(define (render-audio-defproc spec)
   (define name   (list-ref spec 0))
   (define args   (list-ref spec 1))
   (define result (list-ref spec 2))
   (eval
    `(defproc* [[(,name ,@args) ,result]]
       (para (audio-desc ',name))
       (if (audio-return-line ',name)
           (para (audio-return-line ',name))
           (list))
      (para (list (bold "MDN:") " " (audio-mdn-link ',name)))
      (para (tt (audio-sig ',name))))
    scribble-ns))
@(define console-ffi-index (load-ffi-doc-index "ffi/console.ffi"))
@(define console-ffi-docs (load-ffi-docs "ffi/console.ffi"))
@(define console-doc-specs
   (for/list ([fd (in-list console-ffi-docs)])
     (define name (foreign-racket-name (foreign-doc-foreign fd)))
     (define arg-contracts (ffi-doc-argument-contracts console-ffi-index name))
     (define args
       (for/list ([c (in-list arg-contracts)]
                  [i (in-naturals 1)])
         (list (string->symbol (format "arg~a" i)) c)))
     (define result (ffi-doc-result-contract console-ffi-index name))
     (list name args result)))
@(define console-documented-bindings (map car console-doc-specs))
@(define (console-desc name)
   (or (ffi-doc-description console-ffi-index name)
       (ffi-doc-default-description console-ffi-index name)))
@(define (console-sig name)
   (ffi-doc-signature-line console-ffi-index name "console.ffi"))
@(define (console-return-line name)
   (ffi-doc-return-note console-ffi-index name))
@(define (console-mdn-link name)
   (define path (ffi-doc-mdn-path/default console-ffi-index name))
   (mdn path (mdn-label path)))
@(define (render-console-defproc spec)
   (define name   (list-ref spec 0))
   (define args   (list-ref spec 1))
   (define result (list-ref spec 2))
   (eval
    `(defproc* [[(,name ,@args) ,result]]
       (para (console-desc ',name))
       (if (console-return-line ',name)
           (para (console-return-line ',name))
           (list))
       (para (list (bold "MDN:") " " (console-mdn-link ',name)))
       (para (tt (console-sig ',name))))
    scribble-ns))
@(define (dom-desc name)
   (or (ffi-doc-description dom-ffi-index name)
       (ffi-doc-default-description dom-ffi-index name)))
@(define (dom-sig name)
   (ffi-doc-signature-line dom-ffi-index name "dom.ffi"))
@(define (dom-return-line name)
   (ffi-doc-return-note dom-ffi-index name))
@(define (mdn-label path)
   (define parts (string-split path "/"))
   (if (= (length parts) 2)
       (string-append (car parts) "." (cadr parts))
       path))
@(define (dom-mdn-link name)
   (define path (ffi-doc-mdn-path/default dom-ffi-index name))
   (mdn path (mdn-label path)))
@(define scribble-ns (variable-reference->namespace (#%variable-reference)))
@(define (render-dom-defproc spec)
   (define name   (list-ref spec 0))
   (define args   (list-ref spec 1))
   (define result (list-ref spec 2))
   (eval
    `(defproc* [[(,name ,@args) ,result]]
       (para (dom-desc ',name))
       (if (dom-return-line ',name)
           (para (dom-return-line ',name))
           (list))
       (para (list (bold "MDN:") " " (dom-mdn-link ',name)))
       (para (tt (dom-sig ',name))))
    scribble-ns))
@title{WebRacket Manual}
@table-of-contents[]


@include-section["Introduction.scrbl"]
@include-section["WebRacket_at_a_Glance.scrbl"]

@include-section["Installation.scrbl"]

@include-section["Command-Line_Tool.scrbl"]

@;-------------------------------------------------------------------

@section{Libraries}

WebRacket libraries come in two forms:

@itemlist[
  @item{@racket[(include-lib lib-id)] for libraries whose code is inserted
        textually into your program. This form can be used only at the module
        top level, and including the same library more than once has no extra
        effect.}
  @item{@racket[(require-lib lib-id)] for libraries that export syntactic forms.}
]

Currently available libraries include:

@itemlist[
  @item{@racket[(require-lib define)]}
  @item{@racket[(require-lib threading)]}
  @item{@racket[(include-lib dom)]}
  @item{@racket[(include-lib window)]}
  @item{@racket[(include-lib performance)]}
  @item{@racket[(include-lib document)]}
  @item{@racket[(include-lib event)]}
  @item{@racket[(include-lib domrect)]}
  @item{@racket[(include-lib element)]}
  @item{@racket[(include-lib canvas)]}
  @item{@racket[(include-lib media)]}
  @item{@racket[(include-lib image)]}
  @item{@racket[(include-lib iterator)]}
  @item{@racket[(include-lib web-easy)]}
  @item{@racket[(include-lib audio)]}
  @item{@racket[(include-lib console)]}
  @item{@racket[(include-lib websocket)]}
]

See their respective documentation pages.


@;-------------------------------------------------------------------

@include-section["define.scrbl"]

@;-------------------------------------------------------------------

@include-section["threading.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-dom.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-window.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-performance.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-document.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-event.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-domrect.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-element.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-canvas.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-media.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-image.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-iterator.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-audio.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-console.scrbl"]

@;-------------------------------------------------------------------

@include-section["lib-websocket.scrbl"]

@;-------------------------------------------------------------------

@include-section["web-easy.scrbl"]

@;-------------------------------------------------------------------
@include-section["special-forms.scrbl"]

@;-------------------------------------------------------------------

@include-section["implemented-primitives.scrbl"]

@;-------------------------------------------------------------------

@include-section["Browser_API.scrbl"]
