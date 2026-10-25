#lang racket/base

(require webracket/unsafe/standard
         (only-in webracket/primitives
                  external?
                  procedure->external))

(provide (except-out (struct-out js-promise) js-promise?)
         (rename-out [js-promise/surface? js-promise?])
         js-promise-wrap
         js-promise-wrap-result
         js-promise-unwrap
         js-promise-thenable?
         js-promise-map-fulfilled
         js-promise-resolved
         js-promise-rejected
         js-promise-then
         js-promise-catch
         js-promise-finally)

;;;
;;; JavaScript Promise wrappers
;;;

;; js-promise : external/raw -> js-promise?
;;   Wrap a JavaScript Promise-like value.
(struct js-promise (raw [fulfillment-transform #:auto])
  #:transparent
  #:mutable
  #:auto-value #f)

;; js-promise-raw-host-value? : any/c -> boolean?
;;   Report whether raw is a non-nullish external host value.
(define (js-promise-raw-host-value? raw)
  (and (external? raw)
       (not (js-nullish? raw))))

;; js-promise-thenable-method? : external/raw string? -> boolean?
;;   Report whether raw exposes a function-valued method called method-name.
(define (js-promise-thenable-method? raw method-name)
  (and (js-promise-raw-host-value? raw)
       (let ([method (js-ref/extern raw method-name)])
         (and (not (js-nullish? method))
              (string=? (js-typeof method) "function")))))

;; check-js-promise-host-method : symbol? external/raw string? string? -> void?
;;   Ensure a required Promise host method is available.
(define (check-js-promise-host-method who raw receiver-name method-name)
  (unless (js-promise-thenable-method? raw method-name)
    (raise-arguments-error who
                           (string-append receiver-name
                                          "."
                                          method-name
                                          " is not available"))))

;; check-js-promise-global-method : symbol? string? -> external/raw
;;   Return the Promise constructor after checking a required static method.
(define (check-js-promise-global-method who method-name)
  (define promise-ctor (js-var "Promise"))
  (unless (js-constructible? promise-ctor)
    (raise-arguments-error who "Promise is not available"))
  (check-js-promise-host-method who promise-ctor "Promise" method-name)
  promise-ctor)

;; js-promise-wrap-result : symbol? string? any/c [string?] -> js-promise?
;;   Wrap a host method result after checking that it is Promise-like.
(define (js-promise-wrap-result who method-name raw [receiver-name "Promise"])
  (unless (js-promise-thenable-method? raw "then")
    (raise-arguments-error who
                           (string-append receiver-name
                                          "."
                                          method-name
                                          " result must be Promise-like")))
  (js-promise raw))

;; js-promise-wrap : external/raw -> js-promise?
;;   Wrap a Promise-like value after validating that it is thenable.
(define (js-promise-wrap raw)
  (unless (js-promise-thenable-method? raw "then")
    (raise-argument-error 'js-promise-wrap "Promise-like external/raw" raw))
  (js-promise raw))

;; js-promise-unwrap : js-promise? -> external/raw
;;   Extract the raw Promise-like value.
(define (js-promise-unwrap promise)
  (js-promise-raw promise))

;; js-promise-thenable? : any/c -> boolean?
;;   Report whether v is a wrapped Promise-like value.
(define (js-promise-thenable? v)
  (and (js-promise? v)
       (js-promise-thenable-method? (js-promise-unwrap v) "then")))

;; js-promise/surface? : any/c -> boolean?
;;   Report whether v is a wrapped Promise-like value.
(define (js-promise/surface? v)
  (js-promise-thenable? v))

;; js-promise-map-fulfilled : js-promise? procedure? -> js-promise?
;;   Return a wrapper that transforms fulfilled values before Racket handlers receive them.
(define (js-promise-map-fulfilled promise transform)
  (unless (js-promise-thenable? promise)
    (raise-argument-error 'js-promise-map-fulfilled "js-promise?" promise))
  (unless (procedure? transform)
    (raise-argument-error 'js-promise-map-fulfilled "procedure?" transform))
  (define prior-transform (js-promise-fulfillment-transform promise))
  (define mapped (js-promise (js-promise-unwrap promise)))
  (set-js-promise-fulfillment-transform!
   mapped
   (if prior-transform
       (lambda (value)
         (transform (prior-transform value)))
       transform))
  mapped)

;; js-promise-handler->external : symbol? (or/c procedure? external?) -> external?
;;   Convert a handler to the callback retained by the native Promise.
(define (js-promise-handler->external who handler)
  (cond
    [(external? handler) handler]
    [(procedure? handler)
     ;; A Promise has no removal operation. Its pending reaction owns this
     ;; bridge until settlement, so a module-global cache would retain
     ;; completed handlers forever.
     (procedure->external
      (lambda args
        (apply handler args)))]
    [else
     (raise-argument-error who "(or/c procedure? external?)" handler)]))

;; js-promise-resolved : any/c -> js-promise?
;;   Build an already-resolved JavaScript Promise.
(define (js-promise-resolved value)
  (define promise-ctor (check-js-promise-global-method 'js-promise-resolved "resolve"))
  (js-promise-wrap-result
   'js-promise-resolved
   "resolve"
   (js-send/extern promise-ctor "resolve" (vector value))))

;; js-promise-rejected : any/c -> js-promise?
;;   Build an already-rejected JavaScript Promise.
(define (js-promise-rejected value)
  (define promise-ctor (check-js-promise-global-method 'js-promise-rejected "reject"))
  (js-promise-wrap-result
   'js-promise-rejected
   "reject"
   (js-send/extern promise-ctor "reject" (vector value))))

;; js-promise-then : js-promise? (or/c procedure? external?) [(or/c #f procedure? external?)] -> js-promise?
;;   Attach fulfillment and optional rejection handlers to a Promise.
(define (js-promise-then promise on-fulfilled [on-rejected #f])
  (unless (js-promise-thenable? promise)
    (raise-argument-error 'js-promise-then "js-promise?" promise))
  (define transform (js-promise-fulfillment-transform promise))
  (define on-fulfilled*
    (js-promise-handler->external
     'js-promise-then
     (if (and transform (procedure? on-fulfilled))
         (lambda args
           (cond [(null? args)
                  (on-fulfilled)]
                 [else
                  (apply on-fulfilled
                         (cons (transform (car args))
                               (cdr args)))]))
         on-fulfilled)))
  (js-promise-wrap-result
   'js-promise-then
   "then"
   (if (eq? on-rejected #f)
       (js-send/extern (js-promise-unwrap promise) "then" (vector on-fulfilled*))
       (js-send/extern (js-promise-unwrap promise)
                       "then"
                       (vector on-fulfilled*
                               (js-promise-handler->external 'js-promise-then on-rejected))))))

;; js-promise-catch : js-promise? (or/c procedure? external?) -> js-promise?
;;   Attach a rejection handler to a Promise.
(define (js-promise-catch promise on-rejected)
  (unless (js-promise-thenable? promise)
    (raise-argument-error 'js-promise-catch "js-promise?" promise))
  (check-js-promise-host-method 'js-promise-catch
                                (js-promise-unwrap promise)
                                "Promise"
                                "catch")
  (js-promise-wrap-result
   'js-promise-catch
   "catch"
   (js-send/extern (js-promise-unwrap promise)
                   "catch"
                   (vector (js-promise-handler->external 'js-promise-catch on-rejected)))))

;; js-promise-finally : js-promise? (or/c procedure? external?) -> js-promise?
;;   Attach a finally handler to a Promise.
(define (js-promise-finally promise on-finally)
  (unless (js-promise-thenable? promise)
    (raise-argument-error 'js-promise-finally "js-promise?" promise))
  (check-js-promise-host-method 'js-promise-finally
                                (js-promise-unwrap promise)
                                "Promise"
                                "finally")
  (js-promise-wrap-result
   'js-promise-finally
   "finally"
   (js-send/extern (js-promise-unwrap promise)
                   "finally"
                   (vector (js-promise-handler->external 'js-promise-finally on-finally)))))
