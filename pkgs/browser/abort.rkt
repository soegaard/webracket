#lang racket/base

(require webracket/unsafe/standard
         (only-in webracket/primitives external?))

(provide (except-out (struct-out abort-controller-info) abort-controller-info?)
         (except-out (struct-out abort-signal-info) abort-signal-info?)
         (rename-out [abort-controller-info/surface? abort-controller?]
                     [abort-signal-info/surface? abort-signal?])
         abort-controller-supported?
         make-abort-controller
         abort-controller-signal
         abort-controller-abort!
         abort-controller-abort/with-reason!
         abort-signal-aborted?
         abort-signal-reason)

;; abort-controller-info : external/raw -> abort-controller-info?
;;   Wrap a native AbortController without changing its identity.
(struct abort-controller-info (raw) #:transparent)

;; abort-signal-info : external/raw -> abort-signal-info?
;;   Wrap a native AbortSignal without changing its identity.
(struct abort-signal-info (raw) #:transparent)

;; abort-raw-host-value? : any/c -> boolean?
;;   Report whether value is a non-nullish browser host object.
(define (abort-raw-host-value? value)
  (and (external? value)
       (not (js-nullish? value))))

;; abort-host-method? : external/raw string? -> boolean?
;;   Report whether raw exposes a callable host method.
(define (abort-host-method? raw method-name)
  (and (abort-raw-host-value? raw)
       (let ([method (js-ref/extern raw method-name)])
         (and (not (js-nullish? method))
              (string=? (js-typeof method) "function")))))

;; abort-controller-supported? : -> boolean?
;;   Report whether the browser provides a constructible AbortController.
(define (abort-controller-supported?)
  (js-constructible? (js-var "AbortController")))

;; abort-controller-info/surface? : any/c -> boolean?
;;   Report whether value is a usable AbortController wrapper.
(define (abort-controller-info/surface? value)
  (and (abort-controller-info? value)
       (abort-raw-host-value? (abort-controller-info-raw value))
       (abort-host-method? (abort-controller-info-raw value) "abort")))

;; abort-signal-info/surface? : any/c -> boolean?
;;   Report whether value is a usable AbortSignal wrapper.
(define (abort-signal-info/surface? value)
  (and (abort-signal-info? value)
       (abort-raw-host-value? (abort-signal-info-raw value))
       (not (js-nullish? (js-ref/extern (abort-signal-info-raw value) "aborted")))))

;; make-abort-controller : -> abort-controller?
;;   Construct a native AbortController.
(define (make-abort-controller)
  (unless (abort-controller-supported?)
    (raise-arguments-error 'make-abort-controller
                           "AbortController is not available"))
  (abort-controller-info (js-new (js-var "AbortController") (vector))))

;; abort-controller-signal : abort-controller? -> abort-signal?
;;   Read the controller's associated AbortSignal.
(define (abort-controller-signal controller)
  (unless (abort-controller-info/surface? controller)
    (raise-argument-error 'abort-controller-signal "abort-controller?" controller))
  (define raw (js-ref/extern (abort-controller-info-raw controller) "signal"))
  (unless (abort-raw-host-value? raw)
    (raise-arguments-error 'abort-controller-signal
                           "AbortController.signal is not available"))
  (abort-signal-info raw))

;; abort-controller-abort! : abort-controller? -> void?
;;   Abort without supplying a native reason.
(define (abort-controller-abort! controller)
  (unless (abort-controller-info/surface? controller)
    (raise-argument-error 'abort-controller-abort! "abort-controller?" controller))
  (js-send/extern/nullish (abort-controller-info-raw controller) "abort" (vector))
  (void))

;; abort-controller-abort/with-reason! : abort-controller? any/c -> void?
;;   Abort with reason, preserving #f rather than treating it as omitted.
(define (abort-controller-abort/with-reason! controller reason)
  (unless (abort-controller-info/surface? controller)
    (raise-argument-error 'abort-controller-abort/with-reason!
                          "abort-controller?"
                          controller))
  (js-send/extern/nullish (abort-controller-info-raw controller)
                          "abort"
                          (vector reason))
  (void))

;; abort-signal-aborted? : abort-signal? -> boolean?
;;   Report whether the signal has been aborted.
(define (abort-signal-aborted? signal)
  (unless (abort-signal-info/surface? signal)
    (raise-argument-error 'abort-signal-aborted? "abort-signal?" signal))
  (define value (js-ref (abort-signal-info-raw signal) "aborted"))
  (unless (boolean? value)
    (raise-arguments-error 'abort-signal-aborted?
                           "AbortSignal.aborted did not return a boolean"))
  value)

;; abort-signal-reason : abort-signal? -> (or/c #f any/c)
;;   Return the nullable native abort reason, preserving ordinary JavaScript values.
(define (abort-signal-reason signal)
  (unless (abort-signal-info/surface? signal)
    (raise-argument-error 'abort-signal-reason "abort-signal?" signal))
  (define value (js-ref (abort-signal-info-raw signal) "reason"))
  ;; `js-ref` returns a WebRacket value rather than an external/raw value, so
  ;; a JavaScript nullish reason has already become #f at this boundary.
  (if value value #f))
