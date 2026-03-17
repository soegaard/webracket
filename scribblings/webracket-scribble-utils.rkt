#lang racket
(require scribble/manual)

(provide wiki
          margin-wiki
          em)

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
