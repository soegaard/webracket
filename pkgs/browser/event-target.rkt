#lang racket/base

(require browser/abort
         browser/document
         browser/element
         browser/window
         webracket/unsafe/standard
         (only-in webracket/primitives
                  external?
                  procedure->external))

(provide (struct-out event-listener-options)
         event-target?
         make-event-listener-options
         event-target-add-listener!
         event-target-remove-listener!
         event-target-listener-active?
         event-target-dispatch!)

;; event-listener-options : boolean? boolean? boolean? (or/c #f abort-signal?) -> event-listener-options?
;;   Describe EventTarget listener options without exposing a JavaScript dictionary.
(struct event-listener-options (capture? passive? once? signal)
  #:transparent
  #:constructor-name make-event-listener-options
  #:guard
  (lambda (capture? passive? once? signal name)
    (unless (boolean? capture?)
      (raise-argument-error name "boolean?" capture?))
    (unless (boolean? passive?)
      (raise-argument-error name "boolean?" passive?))
    (unless (boolean? once?)
      (raise-argument-error name "boolean?" once?))
    (unless (or (not signal) (abort-signal? signal))
      (raise-argument-error name "(or/c #f abort-signal?)" signal))
    (values capture? passive? once? signal)))

;; event-listener-token : external/raw string? external? event-listener-options? box? -> event-listener-token?
;;   Keep one native listener bridge reachable until it is removed or its token is released.
(struct event-listener-token (target type callback options active-box) #:transparent)

;; event-target-event-name : symbol? any/c -> string?
;;   Normalize a browser event name.
(define (event-target-event-name who name)
  (cond
    [(string? name) name]
    [(symbol? name) (symbol->string name)]
    [else (raise-argument-error who "(or/c string? symbol?)" name)]))

;; event-target-raw : symbol? any/c -> external/raw
;;   Unwrap one of the supported public EventTarget wrappers.
(define (event-target-raw who target)
  (define raw
    (cond
      [(abort-signal? target) (abort-signal-info-raw target)]
      [(dom-document? target) (dom-document-raw target)]
      [(dom-window? target) (dom-window-raw target)]
      [else (dom-node-unwrap target)]))
  (unless (and (external? raw)
               (not (js-nullish? raw)))
    (raise-argument-error who
                          "EventTarget wrapper or non-nullish external?"
                          target))
  (define method (js-ref/extern raw "addEventListener"))
  (unless (and (external? method)
               (string=? (js-typeof method) "function"))
    (raise-arguments-error who "EventTarget.addEventListener is not available"))
  raw)

;; event-listener-options->raw : event-listener-options? -> external/raw
;;   Convert listener options while preserving an optional AbortSignal identity.
(define (event-listener-options->raw options)
  (define signal (event-listener-options-signal options))
  (js-object
   (vector (vector "capture" (event-listener-options-capture? options))
           (vector "passive" (event-listener-options-passive? options))
           (vector "once" (event-listener-options-once? options))
           (vector "signal"
                   (if signal
                       (abort-signal-info-raw signal)
                       (js-undefined))))))

;; event-listener->external : symbol? (or/c procedure? external?) box? boolean? -> external?
;;   Build one listener bridge, deliberately without a module-global callback cache.
(define (event-listener->external who listener active-box once?)
  (cond
    [(external? listener) listener]
    [(procedure? listener)
     (procedure->external
      (lambda args
        (when (unbox active-box)
          ;; Native `once` removes before a nested dispatch can invoke the
          ;; listener again. Mirror that observable state in the wrapper.
          (when once? (set-box! active-box #f))
          (apply listener args))
        (void)))]
    [else
     (raise-argument-error who "(or/c procedure? external?)" listener)]))

;; event-target? : any/c -> boolean?
;;   Report whether value wraps or is a usable browser EventTarget.
(define (event-target? value)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (event-target-raw 'event-target? value)
    #t))

;; event-target-add-listener! : EventTarget (or/c string? symbol?) (or/c procedure? external?) [event-listener-options?] -> event-listener-token?
;;   Install a listener and return the token required for deterministic removal.
(define (event-target-add-listener! target type listener
                                    [options (make-event-listener-options #f #f #f #f)])
  (unless (event-listener-options? options)
    (raise-argument-error 'event-target-add-listener! "event-listener-options?" options))
  (define raw (event-target-raw 'event-target-add-listener! target))
  (define type* (event-target-event-name 'event-target-add-listener! type))
  (define active-box (box #t))
  (define callback
    (event-listener->external 'event-target-add-listener!
                              listener
                              active-box
                              (event-listener-options-once? options)))
  (js-send/extern/nullish raw
                          "addEventListener"
                          (vector type* callback (event-listener-options->raw options)))
  (event-listener-token raw type* callback options active-box))

;; event-target-remove-listener! : event-listener-token? -> void?
;;   Remove an installed listener and release this token's callback bridge.
(define (event-target-remove-listener! token)
  (unless (event-listener-token? token)
    (raise-argument-error 'event-target-remove-listener! "event-listener-token?" token))
  (when (unbox (event-listener-token-active-box token))
    (js-send/extern/nullish
     (event-listener-token-target token)
     "removeEventListener"
     (vector (event-listener-token-type token)
             (event-listener-token-callback token)
             (event-listener-options->raw (event-listener-token-options token))))
    (set-box! (event-listener-token-active-box token) #f))
  (void))

;; event-target-listener-active? : event-listener-token? -> boolean?
;;   Report whether the token still represents an installed listener bridge.
(define (event-target-listener-active? token)
  (unless (event-listener-token? token)
    (raise-argument-error 'event-target-listener-active? "event-listener-token?" token))
  (unbox (event-listener-token-active-box token)))

;; event-target-dispatch! : EventTarget external? -> boolean?
;;   Dispatch an event synchronously and report whether no listener canceled it.
(define (event-target-dispatch! target event)
  (unless (external? event)
    (raise-argument-error 'event-target-dispatch! "external?" event))
  (define raw (event-target-raw 'event-target-dispatch! target))
  (define method (js-ref/extern raw "dispatchEvent"))
  (unless (and (external? method)
               (string=? (js-typeof method) "function"))
    (raise-arguments-error 'event-target-dispatch!
                           "EventTarget.dispatchEvent is not available"))
  (js-send/boolean raw "dispatchEvent" (vector event)))
