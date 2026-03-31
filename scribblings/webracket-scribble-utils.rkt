#lang at-exp racket
(require scribble/manual
         scribble/core
         scribble/html-properties)

(provide wiki
          margin-wiki
          em
          how-to-require
          compile-option-bar
          mdn-bar)

(define (wiki title page)
   (hyperlink
    (string-append "https://en.wikipedia.org/wiki/" page)
    (list title " Ⓦ")))

(define (margin-wiki title page)
   (margin-note
    (hyperlink
     (string-append "https://en.wikipedia.org/wiki/" page)
     (list "Ⓦ " title))))

(define (em . xs) (apply italic xs))

(define-syntax-rule (how-to-require req-form name mod-path)
  @defmodule[#:require-form (lambda (modname)
                               (list (racket (req-form name))))
             mod-path
             #:link-target? #f
             #:packages ()
             #:no-declare])

;; compile-option-bar : content? ... -> block?
;;   Render a compile-option banner styled like the module require bar.
(define (compile-option-bar . text)
  (make-table
   (make-style "defmodule"
               (list (background-color-property "#eef5ff")))
   (list (list (apply para (append (list (hspace 1)) text))))))

;; mdn-bar : string? string? -> block?
;;   Render an MDN banner linking to a browser API page.
(define (mdn-bar label url)
  (make-table
   (make-style "defmodule"
               (list (background-color-property "#f7fbff")))
   (list (list (para (list (hspace 1)
                           (tt "MDN")
                           (hspace 2)
                           (hyperlink url (tt label))))))))
