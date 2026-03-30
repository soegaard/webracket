#lang at-exp racket
(require scribble/manual)

(provide wiki
          margin-wiki
          em
          how-to-require)

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
