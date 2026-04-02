#lang racket

(require (for-syntax racket/base
                     racket/string
                     "../structs.rkt"
                     "ffi-doc.rkt"))

(provide render-ffi-docs)

(begin-for-syntax
  (define (mdn-label* path)
    (define parts (string-split path "/"))
    (if (= (length parts) 2)
        (string-append (car parts) "." (cadr parts))
        path))

  (define (ffi-docs->syntax use-stx ffi-path source-tag)
    (define idx (load-ffi-doc-index ffi-path))
    (define docs (load-ffi-docs ffi-path))
    (define forms
      (for/list ([fd (in-list docs)])
        (define f (foreign-doc-foreign fd))
        (define name (foreign-racket-name f))
        (define arg-contracts (ffi-doc-argument-contracts idx name))
        (define args
          (for/list ([c (in-list arg-contracts)]
                     [i (in-naturals 1)])
            (list (string->symbol (format "arg~a" i)) c)))
        (define result (ffi-doc-result-contract idx name))
        (define desc-text
          (or (ffi-doc-description idx name)
              (ffi-doc-default-description idx name)))
        (define return-text (ffi-doc-return-note idx name))
        (define mdn-path (ffi-doc-mdn-path/default idx name))
        (define mdn-url
          (string-append "https://developer.mozilla.org/en-US/docs/Web/API/" mdn-path))
        (define mdn-text (mdn-label* mdn-path))
        (define sig-text (ffi-doc-signature-line idx name source-tag))
        (define arg-datums
          (for/list ([a (in-list args)])
            (define aname (list-ref a 0))
            (define acon (list-ref a 1))
            (list aname acon)))
        (define form-datum
          `(defproc* [((,name ,@(for/list ([a (in-list arg-datums)])
                                  (list (car a) (cadr a))))
                       ,result)]
              (para ,desc-text)
              ,@(if return-text `((para ,return-text)) '())
              (para (list (bold "MDN:") " " (hyperlink ,mdn-url ,mdn-text)))
              (para (tt ,sig-text))))
        (datum->syntax use-stx form-datum)))
    #`(begin #,@forms)))

(define-syntax (render-ffi-docs stx)
  (syntax-case stx ()
    [(_ ffi-path source-tag)
     (and (string? (syntax-e #'ffi-path))
          (string? (syntax-e #'source-tag)))
     (ffi-docs->syntax stx
                       (syntax-e #'ffi-path)
                       (syntax-e #'source-tag))]))
