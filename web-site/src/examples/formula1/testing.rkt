#lang webracket
(require (for-syntax racket/base)
         (for-syntax racket/file))

(define-syntax (compile-time-file-to-string stx)
  (syntax-case stx ()
    [(_ file-name)
     (datum->syntax #'here (file->string (syntax-e #'file-name)))]))

(define (get-f1-calendar-ics)
  (compile-time-file-to-string "better-f1-calendar.ics"))
