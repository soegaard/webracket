#lang webracket

;;;
;;; WebSocket wrappers
;;;

;; This library wraps the low-level `js-websocket-*` FFI bindings with
;; validated `websocket-*` helpers for browser-side code loaded via
;; `include-lib`.

;; websocket : external/raw -> websocket?
;;   Wrap a browser WebSocket object.
(struct websocket (raw) #:transparent)

;; websocket-unwrap : websocket? -> external/raw
;;   Extract the raw browser WebSocket object.
(define (websocket-unwrap ws)
  (websocket-raw ws))

;; websocket-constructor-present? : -> boolean?
;;   Check whether the host environment exposes a WebSocket constructor.
(define (websocket-constructor-present?)
  (string=? (js-typeof (js-var "WebSocket"))
            "function"))

;; procedure->external-cache : hash?
;;   Cache JS callback wrappers so the same procedure maps to the same external.
(define procedure->external-cache (make-hasheq))

;; check-websocket : symbol? any/c -> void?
;;   Ensure x is a wrapped WebSocket value.
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

;; check-websocket-protocol : symbol? any/c -> void?
;;   Ensure a subprotocol name is a string or symbol.
(define (check-websocket-protocol who protocol)
  (unless (or (string? protocol)
              (symbol? protocol))
    (raise-argument-error who "(or/c string? symbol?)" protocol)))

;; normalize-websocket-protocol : symbol? any/c -> string?
;;   Convert a subprotocol name to a string.
(define (normalize-websocket-protocol who protocol)
  (check-websocket-protocol who protocol)
  (if (symbol? protocol)
      (symbol->string protocol)
      protocol))

;; normalize-websocket-protocols : symbol? (listof (or/c string? symbol?)) -> (or/c void? string? (vectorof string?))
;;   Convert variadic subprotocol arguments to a low-level WebSocket argument.
(define (normalize-websocket-protocols who protocols)
  (cond
    [(null? protocols)
     (void)]
    [(null? (cdr protocols))
     (normalize-websocket-protocol who (car protocols))]
    [else
     (list->vector
      (map (lambda (protocol)
             (normalize-websocket-protocol who protocol))
           protocols))]))

;; ready-state-number->symbol : exact-integer? -> symbol?
;;   Convert a browser readyState number to a symbolic label.
(define (ready-state-number->symbol state)
  (case state
    [(0) 'connecting]
    [(1) 'open]
    [(2) 'closing]
    [(3) 'closed]
    [else
     (error 'websocket-ready-state-number
            "unexpected readyState value: ~a"
            state)]))

;; check-websocket-close-code : symbol? any/c -> void?
;;   Ensure code is an exact integer.
(define (check-websocket-close-code who code)
  (unless (exact-integer? code)
    (raise-argument-error who "exact-integer?" code)))

;; check-websocket-close-reason : symbol? any/c -> void?
;;   Ensure reason is absent or a string.
(define (check-websocket-close-reason who reason)
  (unless (or (eq? reason #f)
              (string? reason)
              (symbol? reason))
    (raise-argument-error who "(or/c #f string? symbol?)" reason)))

;; normalize-websocket-stringish : symbol? any/c -> string?
;;   Convert a string-like WebSocket value to a string.
(define (normalize-websocket-stringish who value)
  (unless (or (string? value)
              (symbol? value))
    (raise-argument-error who "(or/c string? symbol?)" value))
  (if (symbol? value)
      (symbol->string value)
      value))

;; check-websocket-send-data : symbol? (or/c string? symbol? bytes? external?) -> void?
;;   Ensure data is a browser-acceptable WebSocket message body.
(define (check-websocket-send-data who data)
  (unless (or (string? data)
              (symbol? data)
              (bytes? data)
              (external? data))
    (raise-argument-error who "(or/c string? symbol? bytes? external?)" data)))

;; check-websocket-listener-option : symbol? (or/c boolean? external?) -> void?
;;   Ensure an add/removeEventListener option is a boolean or JS object.
(define (check-websocket-listener-option who option)
  (unless (or (boolean? option)
              (external? option))
    (raise-argument-error who "(or/c boolean? external?)" option)))

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
  (js-set! (websocket-unwrap ws) prop (websocket-handler->external who handler))
  (void))

;; websocket-new : (or/c string? symbol?) (or/c string? symbol?) ... -> extern/raw
;;   Create a new WebSocket connection from a URL and optional subprotocol names.
(define (websocket-new url . protocols)
  (define url* (normalize-websocket-stringish 'websocket-new url))
  (define low-level-protocols
    (normalize-websocket-protocols 'websocket-new protocols))
  (websocket (js-websocket-new url* low-level-protocols)))

;; websocket-send : websocket? (or/c string? symbol? bytes? external?) -> void?
;;   Send data through a WebSocket connection.
(define (websocket-send ws data)
  (check-websocket 'websocket-send ws)
  (check-websocket-send-data 'websocket-send data)
  (js-websocket-send (websocket-unwrap ws)
                     (if (symbol? data) (symbol->string data) data)))

;; websocket-close : websocket? [code exact-integer? 1000] [reason (or/c #f string? symbol?) #f] -> void?
;;   Close a WebSocket connection.
(define (websocket-close ws [code 1000] [reason #f])
  (check-websocket 'websocket-close ws)
  (check-websocket-close-code 'websocket-close code)
  (check-websocket-close-reason 'websocket-close reason)
  (js-websocket-close (websocket-unwrap ws)
                      code
                      (if reason
                          (normalize-websocket-stringish 'websocket-close reason)
                          (void))))

;; websocket-url : websocket? -> string?
;;   Read the WebSocket URL.
(define (websocket-url ws)
  (check-websocket 'websocket-url ws)
  (js-websocket-url (websocket-unwrap ws)))

;; websocket-ready-state-number : websocket? -> u32?
;;   Read the raw readyState number.
(define (websocket-ready-state-number ws)
  (check-websocket 'websocket-ready-state-number ws)
  (js-websocket-ready-state (websocket-unwrap ws)))

;; websocket-ready-state : websocket? -> symbol?
;;   Read the readyState value as a symbolic label.
(define (websocket-ready-state ws)
  (ready-state-number->symbol (websocket-ready-state-number ws)))

;; websocket-buffered-amount : websocket? -> u32?
;;   Read the bufferedAmount value.
(define (websocket-buffered-amount ws)
  (check-websocket 'websocket-buffered-amount ws)
  (js-websocket-buffered-amount (websocket-unwrap ws)))

;; websocket-protocol : websocket? -> string?
;;   Read the negotiated protocol string.
(define (websocket-protocol ws)
  (check-websocket 'websocket-protocol ws)
  (js-websocket-protocol (websocket-unwrap ws)))

;; websocket-extensions : websocket? -> string?
;;   Read the negotiated extensions string.
(define (websocket-extensions ws)
  (check-websocket 'websocket-extensions ws)
  (js-websocket-extensions (websocket-unwrap ws)))

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

;; websocket-add-event-listener! : websocket? (or/c string? symbol?) (or/c procedure? external?) (or/c boolean? external?) ... -> external?
;;   Add a WebSocket event listener and return the installed listener.
(define (websocket-add-event-listener! ws event-name listener . options)
  (check-websocket 'websocket-add-event-listener! ws)
  (define event-name* (normalize-websocket-event-name 'websocket-add-event-listener! event-name))
  (define listener* (websocket-listener->external 'websocket-add-event-listener! listener))
  (for-each (lambda (option)
              (check-websocket-listener-option 'websocket-add-event-listener! option))
            options)
  (define args
    (if (null? options)
        (vector event-name* listener*)
        (list->vector (list* event-name* listener* options))))
  (js-send/extern/nullish (websocket-unwrap ws) "addEventListener" args)
  listener*)

;; websocket-remove-event-listener! : websocket? (or/c string? symbol?) (or/c procedure? external?) (or/c boolean? external?) ... -> void?
;;   Remove a previously installed WebSocket event listener.
(define (websocket-remove-event-listener! ws event-name listener . options)
  (check-websocket 'websocket-remove-event-listener! ws)
  (define event-name* (normalize-websocket-event-name 'websocket-remove-event-listener! event-name))
  (define listener* (websocket-listener->external 'websocket-remove-event-listener! listener))
  (for-each (lambda (option)
              (check-websocket-listener-option 'websocket-remove-event-listener! option))
            options)
  (define args
    (if (null? options)
        (vector event-name* listener*)
        (list->vector (list* event-name* listener* options))))
  (js-send/extern/nullish (websocket-unwrap ws) "removeEventListener" args)
  (void))
