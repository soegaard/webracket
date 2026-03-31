#lang webracket

;;;
;;; WebSocket wrappers
;;;

;; This library wraps the low-level `js-websocket-*` FFI bindings with
;; validated `websocket-*` helpers for browser-side code loaded via
;; `include-lib`.

;; websocket-constructor-present? : -> boolean?
;;   Check whether the host environment exposes a WebSocket constructor.
(define (websocket-constructor-present?)
  (string=? (js-typeof (js-var "WebSocket"))
            "function"))

;; procedure->external-cache : hash?
;;   Cache JS callback wrappers so the same procedure maps to the same external.
(define procedure->external-cache (make-hasheq))

;; websocket? : any/c -> boolean?
;;   Check whether x is a WebSocket value.
(define (websocket? x)
  (and (external? x)
       (websocket-constructor-present?)
       (js-instanceof x (js-var "WebSocket"))))

;; check-websocket : symbol? any/c -> void?
;;   Ensure x is a WebSocket value.
(define (check-websocket who x)
  (unless (websocket? x)
    (raise-argument-error who "websocket?" x)))

;; check-websocket-handler : symbol? any/c -> void?
;;   Ensure handler is a procedure, an external callback, or #f.
(define (check-websocket-handler who handler)
  (unless (or (procedure? handler)
              (external? handler)
              (eq? handler #f))
    (raise-argument-error who "(or/c #f procedure? external?)" handler)))

;; check-websocket-listener : symbol? any/c -> void?
;;   Ensure listener is an external JavaScript callback.
(define (check-websocket-listener who listener)
  (unless (external? listener)
    (raise-argument-error who "external?" listener)))

;; check-websocket-event-name : symbol? any/c -> void?
;;   Ensure event-name is a string or symbol.
(define (check-websocket-event-name who event-name)
  (unless (or (string? event-name)
              (symbol? event-name))
    (raise-argument-error who "(or/c string? symbol?)" event-name)))

;; normalize-websocket-event-name : symbol? any/c -> string?
;;   Convert an event name to the string used by add/removeEventListener.
(define (normalize-websocket-event-name who event-name)
  (check-websocket-event-name who event-name)
  (if (symbol? event-name)
      (symbol->string event-name)
      event-name))

;; check-websocket-protocols : symbol? any/c -> void?
;;   Ensure protocols is omitted, a string, or a string sequence.
(define (check-websocket-protocols who protocols)
  (define (string-sequence? xs)
    (cond
      [(null? xs) #t]
      [(pair? xs)
       (and (string? (car xs))
            (string-sequence? (cdr xs)))]
      [else #f]))
  (define (vector-of-strings? v)
    (let loop ([i 0])
      (cond
        [(= i (vector-length v)) #t]
        [(string? (vector-ref v i)) (loop (add1 i))]
        [else #f])))
  (unless (or (void? protocols)
              (string? protocols)
              (and (list? protocols)
                   (string-sequence? protocols))
              (and (vector? protocols)
                   (vector-of-strings? protocols)))
    (raise-argument-error
     who
     "(or/c void? string? (listof string?) (vectorof string?))"
     protocols)))

;; check-websocket-close-code : symbol? any/c -> void?
;;   Ensure code is omitted or an exact integer.
(define (check-websocket-close-code who code)
  (unless (or (void? code)
              (exact-integer? code))
    (raise-argument-error who "(or/c void? exact-integer?)" code)))

;; check-websocket-close-reason : symbol? any/c -> void?
;;   Ensure reason is omitted or a string.
(define (check-websocket-close-reason who reason)
  (unless (or (void? reason)
              (string? reason))
    (raise-argument-error who "(or/c void? string?)" reason)))

;; websocket-handler->external : symbol? any/c -> any/c
;;   Convert a handler to an external callback or JS null.
(define (websocket-handler->external who handler)
  (check-websocket-handler who handler)
  (cond
    [(eq? handler #f) (js-null)]
    [(external? handler) handler]
    [else (procedure->stable-external handler)]))

;; websocket-listener->external : symbol? any/c -> external?
;;   Convert a listener procedure to a JS callback value.
(define (websocket-listener->external who listener)
  (cond
    [(external? listener) listener]
    [(procedure? listener) (procedure->stable-external listener)]
    [else
     (raise-argument-error who "(or/c procedure? external?)" listener)]))

;; procedure->stable-external : procedure? -> external?
;;   Reuse a cached callback wrapper for a procedure when possible.
(define (procedure->stable-external proc)
  (define cached (hash-ref procedure->external-cache proc #f))
  (cond
    [cached cached]
    [else
     (define external (procedure->external proc))
     (hash-set! procedure->external-cache proc external)
     external]))

;; websocket-set-handler! : symbol? external? string? any/c -> void?
;;   Install or clear a WebSocket event handler property.
(define (websocket-set-handler! who ws prop handler)
  (check-websocket who ws)
  (js-set! ws prop (websocket-handler->external who handler))
  (void))

;; websocket-new : string? [protocols (or/c void? string? (listof string?) (vectorof string?))] -> extern/raw
;;   Create a new WebSocket connection.
(define (websocket-new url [protocols (void)])
  (unless (string? url)
    (raise-argument-error 'websocket-new "string?" url))
  (check-websocket-protocols 'websocket-new protocols)
  (js-websocket-new url protocols))

;; websocket-send : websocket? any/c -> void?
;;   Send data through a WebSocket connection.
(define (websocket-send ws data)
  (check-websocket 'websocket-send ws)
  (js-websocket-send ws data))

;; websocket-close : websocket? [code (or/c void? exact-integer?)] [reason (or/c void? string?)] -> void?
;;   Close a WebSocket connection.
(define (websocket-close ws [code (void)] [reason (void)])
  (check-websocket 'websocket-close ws)
  (check-websocket-close-code 'websocket-close code)
  (check-websocket-close-reason 'websocket-close reason)
  (js-websocket-close ws code reason))

;; websocket-url : websocket? -> string?
;;   Read the WebSocket URL.
(define (websocket-url ws)
  (check-websocket 'websocket-url ws)
  (js-websocket-url ws))

;; websocket-ready-state : websocket? -> u32?
;;   Read the readyState value.
(define (websocket-ready-state ws)
  (check-websocket 'websocket-ready-state ws)
  (js-websocket-ready-state ws))

;; websocket-buffered-amount : websocket? -> u32?
;;   Read the bufferedAmount value.
(define (websocket-buffered-amount ws)
  (check-websocket 'websocket-buffered-amount ws)
  (js-websocket-buffered-amount ws))

;; websocket-protocol : websocket? -> string?
;;   Read the negotiated protocol string.
(define (websocket-protocol ws)
  (check-websocket 'websocket-protocol ws)
  (js-websocket-protocol ws))

;; websocket-extensions : websocket? -> string?
;;   Read the negotiated extensions string.
(define (websocket-extensions ws)
  (check-websocket 'websocket-extensions ws)
  (js-websocket-extensions ws))

;; websocket-onopen! : websocket? (or/c #f procedure? external?) -> void?
;;   Install or clear the onopen handler.
(define (websocket-onopen! ws handler)
  (websocket-set-handler! 'websocket-onopen! ws "onopen" handler))

;; websocket-onmessage! : websocket? (or/c #f procedure? external?) -> void?
;;   Install or clear the onmessage handler.
(define (websocket-onmessage! ws handler)
  (websocket-set-handler! 'websocket-onmessage! ws "onmessage" handler))

;; websocket-onclose! : websocket? (or/c #f procedure? external?) -> void?
;;   Install or clear the onclose handler.
(define (websocket-onclose! ws handler)
  (websocket-set-handler! 'websocket-onclose! ws "onclose" handler))

;; websocket-onerror! : websocket? (or/c #f procedure? external?) -> void?
;;   Install or clear the onerror handler.
(define (websocket-onerror! ws handler)
  (websocket-set-handler! 'websocket-onerror! ws "onerror" handler))

;; websocket-add-event-listener! : websocket? (or/c string? symbol?) (or/c procedure? external?) [any/c] -> external?
;;   Add a WebSocket event listener and return the installed listener.
(define (websocket-add-event-listener! ws event-name listener [options (void)])
  (check-websocket 'websocket-add-event-listener! ws)
  (define event-name* (normalize-websocket-event-name 'websocket-add-event-listener! event-name))
  (define listener* (websocket-listener->external 'websocket-add-event-listener! listener))
  (js-send/extern/nullish ws "addEventListener" (vector event-name* listener* options))
  listener*)

;; websocket-remove-event-listener! : websocket? (or/c string? symbol?) (or/c procedure? external?) [any/c] -> void?
;;   Remove a previously installed WebSocket event listener.
(define (websocket-remove-event-listener! ws event-name listener [options (void)])
  (check-websocket 'websocket-remove-event-listener! ws)
  (define event-name* (normalize-websocket-event-name 'websocket-remove-event-listener! event-name))
  (define listener* (websocket-listener->external 'websocket-remove-event-listener! listener))
  (js-send/extern/nullish ws "removeEventListener" (vector event-name* listener* options))
  (void))
