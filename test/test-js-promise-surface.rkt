#lang webracket/base

(require js/promise)

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(void
 js-promise
 js-promise?
 js-promise-raw
 js-promise-wrap
 js-promise-unwrap
 js-promise-thenable?
 js-promise-map-fulfilled
 js-promise-resolved
 js-promise-rejected
 js-promise-then
 js-promise-catch
 js-promise-finally)

(define resolved-promise (js-promise-resolved "ok"))
(define rejected-promise (js-promise-rejected "bad"))

(check-true (js-promise? resolved-promise)
            "js-promise-resolved returns a wrapped promise")
(check-true (js-promise? rejected-promise)
            "js-promise-rejected returns a wrapped promise")
(check-true (js-promise? (js-promise-map-fulfilled resolved-promise
                                                     (lambda (value) value)))
            "js-promise-map-fulfilled returns a wrapped promise")
(check-true (js-promise? (js-promise-then resolved-promise
                                          (lambda (value) value)))
            "js-promise-then returns a wrapped promise")
(check-true (js-promise? (js-promise-catch rejected-promise
                                           (lambda (value) value)))
            "js-promise-catch returns a wrapped promise")
(check-true (js-promise? (js-promise-finally resolved-promise
                                             (lambda () (void))))
            "js-promise-finally returns a wrapped promise")

#t
