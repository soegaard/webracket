#lang racket

(require scribble/manual
         scribble/example
         scribble-tools
         racket/string
         "../structs.rkt"
         "ffi-doc.rkt"
         "webracket-scribble-utils.rkt")

(provide dom-doc-specs
         render-dom-defproc
         audio-doc-specs
         render-audio-defproc
         console-doc-specs
         render-console-defproc
         websocket-doc-specs
         render-websocket-defproc
         window-doc-specs
         render-window-defproc
         performance-doc-specs
         render-performance-defproc
         document-doc-specs
         render-document-defproc
         event-doc-specs
         render-event-defproc
         domrect-doc-specs
         render-domrect-defproc
         element-doc-specs
         render-element-defproc
         canvas-doc-specs
         render-canvas-defproc
         media-doc-specs
         render-media-defproc
         image-doc-specs
         render-image-defproc)

(define (mdn path [label #f])
  (hyperlink (string-append "https://developer.mozilla.org/en-US/docs/Web/API/" path)
             (or label path)))

(define (mdn-label path)
  (define parts (string-split path "/"))
  (if (= (length parts) 2)
      (string-append (car parts) "." (cadr parts))
      path))
(define scribble-ns (variable-reference->namespace (#%variable-reference)))

(define (make-doc-specs ffi-path)
  (define idx (load-ffi-doc-index ffi-path))
  (define docs (load-ffi-docs ffi-path))
  (values idx
          docs
          (for/list ([fd (in-list docs)])
            (define name (foreign-racket-name (foreign-doc-foreign fd)))
            (define arg-contracts (ffi-doc-argument-contracts idx name))
            (define args
              (for/list ([c (in-list arg-contracts)]
                         [i (in-naturals 1)])
                (list (string->symbol (format "arg~a" i)) c)))
            (define result (ffi-doc-result-contract idx name))
            (list name args result))))

(define (make-family-doc-set ffi-path ffi-file)
  (define-values (idx docs specs) (make-doc-specs ffi-path))
  (define (desc name)
    (or (ffi-doc-description idx name)
        (ffi-doc-default-description idx name)))
  (define (sig name)
    (ffi-doc-signature-line idx name ffi-file))
  (define (return-line name)
    (ffi-doc-return-note idx name))
  (define (mdn-link name)
    (define path (ffi-doc-mdn-path/default idx name))
    (mdn path (mdn-label path)))
  (define (render spec)
    (define name   (list-ref spec 0))
    (define args   (list-ref spec 1))
    (define result (list-ref spec 2))
    (define desc-text (desc name))
    (define return-text (return-line name))
    (define mdn-node (mdn-link name))
    (define sig-text (sig name))
    (eval
     `(defproc* [[(,name ,@args) ,result]]
        (para ,desc-text)
        (if ,return-text
            (para ,return-text)
            (list))
        (para (list (bold "MDN:") " " ,mdn-node))
        (para (tt ,sig-text)))
     scribble-ns))
  (values idx docs specs render))

(define-values (dom-ffi-index dom-ffi-docs dom-doc-specs)
  (make-doc-specs "ffi/dom.ffi"))
(define dom-documented-bindings (map car dom-doc-specs))
(define (dom-desc name)
  (or (ffi-doc-description dom-ffi-index name)
      (ffi-doc-default-description dom-ffi-index name)))
(define (dom-sig name)
  (ffi-doc-signature-line dom-ffi-index name "dom.ffi"))
(define (dom-return-line name)
  (ffi-doc-return-note dom-ffi-index name))
(define (dom-mdn-link name)
  (define path (ffi-doc-mdn-path/default dom-ffi-index name))
  (mdn path (mdn-label path)))
(define (render-dom-defproc spec)
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

(define-values (audio-ffi-index audio-ffi-docs audio-doc-specs)
  (make-doc-specs "ffi/audio.ffi"))
(define audio-documented-bindings (map car audio-doc-specs))
(define (audio-desc name)
  (or (ffi-doc-description audio-ffi-index name)
      (ffi-doc-default-description audio-ffi-index name)))
(define (audio-sig name)
  (ffi-doc-signature-line audio-ffi-index name "audio.ffi"))
(define (audio-return-line name)
  (ffi-doc-return-note audio-ffi-index name))
(define (audio-mdn-link name)
  (define path (ffi-doc-mdn-path/default audio-ffi-index name))
  (mdn path (mdn-label path)))
(define (render-audio-defproc spec)
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

(define-values (console-ffi-index console-ffi-docs console-doc-specs)
  (make-doc-specs "ffi/console.ffi"))
(define console-documented-bindings (map car console-doc-specs))
(define (console-desc name)
  (or (ffi-doc-description console-ffi-index name)
      (ffi-doc-default-description console-ffi-index name)))
(define (console-sig name)
  (ffi-doc-signature-line console-ffi-index name "console.ffi"))
(define (console-return-line name)
  (ffi-doc-return-note console-ffi-index name))
(define (console-mdn-link name)
  (define path (ffi-doc-mdn-path/default console-ffi-index name))
  (mdn path (mdn-label path)))
(define (console-mdn-bar name)
  (define path (ffi-doc-mdn-path/default console-ffi-index name))
  (define leaf (last (string-split path "/")))
  (define method-name
    (regexp-replace #rx"_static$" leaf ""))
  (mdn-bar (format "Console: ~a() method" method-name)
           (string-append "https://developer.mozilla.org/en-US/docs/Web/API/" path)))
(define (render-console-defproc spec)
  (define name   (list-ref spec 0))
  (define args   (list-ref spec 1))
  (define result (list-ref spec 2))
  (eval
   `(defproc* [[(,name ,@args) ,result]]
      (console-mdn-bar ',name)
      (para (console-desc ',name))
      (if (console-return-line ',name)
          (para (console-return-line ',name))
          (list))
      (para (tt (console-sig ',name))))
   scribble-ns))

(define-values (websocket-ffi-index websocket-ffi-docs websocket-doc-specs)
  (make-doc-specs "ffi/websocket.ffi"))
(define websocket-documented-bindings (map car websocket-doc-specs))
(define (websocket-desc name)
  (or (ffi-doc-description websocket-ffi-index name)
      (ffi-doc-default-description websocket-ffi-index name)))
(define (websocket-sig name)
  (ffi-doc-signature-line websocket-ffi-index name "websocket.ffi"))
(define (websocket-return-line name)
  (ffi-doc-return-note websocket-ffi-index name))
(define (websocket-mdn-link name)
  (define path (ffi-doc-mdn-path/default websocket-ffi-index name))
  (mdn path (mdn-label path)))
(define (render-websocket-defproc spec)
  (define name   (list-ref spec 0))
  (define args   (list-ref spec 1))
  (define result (list-ref spec 2))
  (eval
   `(defproc* [[(,name ,@args) ,result]]
      (para (websocket-desc ',name))
      (if (websocket-return-line ',name)
          (para (websocket-return-line ',name))
          (list))
      (para (list (bold "MDN:") " " (websocket-mdn-link ',name)))
      (para (tt (websocket-sig ',name))))
   scribble-ns))

(define-values (window-ffi-index window-ffi-docs window-doc-specs render-window-defproc)
  (make-family-doc-set "ffi/window.ffi" "window.ffi"))
(define-values (performance-ffi-index performance-ffi-docs performance-doc-specs render-performance-defproc)
  (make-family-doc-set "ffi/performance.ffi" "performance.ffi"))
(define-values (document-ffi-index document-ffi-docs document-doc-specs render-document-defproc)
  (make-family-doc-set "ffi/document.ffi" "document.ffi"))
(define-values (event-ffi-index event-ffi-docs event-doc-specs render-event-defproc)
  (make-family-doc-set "ffi/event.ffi" "event.ffi"))
(define-values (domrect-ffi-index domrect-ffi-docs domrect-doc-specs render-domrect-defproc)
  (make-family-doc-set "ffi/domrect.ffi" "domrect.ffi"))
(define-values (element-ffi-index element-ffi-docs element-doc-specs render-element-defproc)
  (make-family-doc-set "ffi/element.ffi" "element.ffi"))
(define-values (canvas-ffi-index canvas-ffi-docs canvas-doc-specs render-canvas-defproc)
  (make-family-doc-set "ffi/canvas.ffi" "canvas.ffi"))
(define-values (media-ffi-index media-ffi-docs media-doc-specs render-media-defproc)
  (make-family-doc-set "ffi/media.ffi" "media.ffi"))
(define-values (image-ffi-index image-ffi-docs image-doc-specs render-image-defproc)
  (make-family-doc-set "ffi/image.ffi" "image.ffi"))
