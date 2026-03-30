#lang racket

(require scribble/manual
         scribble/example
         scribble-tools
         racket/string
         "../structs.rkt"
         "ffi-doc.rkt")

(provide dom-doc-specs
         render-dom-defproc)

(define (mdn path [label #f])
  (hyperlink (string-append "https://developer.mozilla.org/en-US/docs/Web/API/" path)
             (or label path)))

(define dom-ffi-index (load-ffi-doc-index "ffi/dom.ffi"))
(define dom-ffi-docs (load-ffi-docs "ffi/dom.ffi"))
(define dom-doc-specs
  (for/list ([fd (in-list dom-ffi-docs)])
    (define name (foreign-racket-name (foreign-doc-foreign fd)))
    (define arg-contracts (ffi-doc-argument-contracts dom-ffi-index name))
    (define args
      (for/list ([c (in-list arg-contracts)]
                 [i (in-naturals 1)])
        (list (string->symbol (format "arg~a" i)) c)))
    (define result (ffi-doc-result-contract dom-ffi-index name))
    (list name args result)))
(define dom-documented-bindings (map car dom-doc-specs))
(define (dom-desc name)
  (or (ffi-doc-description dom-ffi-index name)
      (ffi-doc-default-description dom-ffi-index name)))
(define (dom-sig name)
  (ffi-doc-signature-line dom-ffi-index name "dom.ffi"))
(define (dom-return-line name)
  (ffi-doc-return-note dom-ffi-index name))
(define (mdn-label path)
  (define parts (string-split path "/"))
  (if (= (length parts) 2)
      (string-append (car parts) "." (cadr parts))
      path))
(define (dom-mdn-link name)
  (define path (ffi-doc-mdn-path/default dom-ffi-index name))
  (mdn path (mdn-label path)))
(define scribble-ns (variable-reference->namespace (#%variable-reference)))
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
