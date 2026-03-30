#lang racket

(require scribble/manual
         scribble/example
         scribble-tools
         racket/string
         "../structs.rkt"
         "ffi-doc.rkt")

(provide dom-doc-specs
         render-dom-defproc
         websocket-doc-specs
         render-websocket-defproc)

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
