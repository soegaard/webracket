#lang racket
(require (for-syntax racket/base racket/file))

(define-syntax (get-calendar-ics stx)
  (datum->syntax #'here (file->string "better-f1-calendar.ics")))

(get-calendar-ics)

