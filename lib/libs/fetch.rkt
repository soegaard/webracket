#lang webracket

;;;
;;; Fetch wrappers
;;;

;; This library exposes a small, Rackety Fetch surface on top of the
;; browser's `fetch`, `Request`, `Response`, and `Headers` globals.

;; fetch-stringish->string : symbol? any/c -> string?
;;   Normalize a request or header label to a browser string.
(define (fetch-stringish->string who v)
  (cond
    [(string? v) v]
    [(symbol? v) (symbol->string v)]
    [else
     (raise-argument-error who "(or/c string? symbol?)" v)]))

;; fetch-resolve-optional : any/c -> any/c
;;   Treat #f as omitted and force thunks when a literal value is needed.
(define (fetch-resolve-optional value)
  (cond
    [(eq? value #f) (void)]
    [(procedure? value) (value)]
    [else value]))

;; fetch-constructor-present? : string? -> boolean?
;;   Check whether the host environment exposes a named constructor.
(define (fetch-constructor-present? name)
  (string=? (js-typeof (js-var name)) "function"))

;; fetch-headers-info : external/raw -> fetch-headers-info?
;;   Wrap a browser Headers object.
(struct fetch-headers-info (raw) #:transparent)

;; fetch-request-info : external/raw -> fetch-request-info?
;;   Wrap a browser Request object.
(struct fetch-request-info (raw) #:transparent)

;; fetch-response-info : external/raw -> fetch-response-info?
;;   Wrap a browser Response object.
(struct fetch-response-info (raw) #:transparent)

;; fetch-headers : external/raw -> fetch-headers-info?
;;   Wrap a browser Headers object.
(define (fetch-headers raw)
  (fetch-headers-info raw))

;; fetch-headers-raw : fetch-headers-info? -> external/raw
;;   Extract the raw Headers object.
(define (fetch-headers-raw headers)
  (fetch-headers-info-raw headers))

;; fetch-request : external/raw -> fetch-request-info?
;;   Wrap a browser Request object.
(define (fetch-request raw)
  (fetch-request-info raw))

;; fetch-request-raw : fetch-request-info? -> external/raw
;;   Extract the raw Request object.
(define (fetch-request-raw request)
  (fetch-request-info-raw request))

;; fetch-response : external/raw -> fetch-response-info?
;;   Wrap a browser Response object.
(define (fetch-response raw)
  (fetch-response-info raw))

;; fetch-response-raw : fetch-response-info? -> external/raw
;;   Extract the raw Response object.
(define (fetch-response-raw response)
  (fetch-response-info-raw response))

;; fetch-headers? : any/c -> boolean?
;;   Report whether v is a wrapped Headers value.
(define (fetch-headers? v)
  (and (fetch-headers-info? v)
       (fetch-constructor-present? "Headers")
       (external? (fetch-headers-raw v))
       (js-instanceof (fetch-headers-raw v) (js-var "Headers"))))

;; fetch-request? : any/c -> boolean?
;;   Report whether v is a wrapped Request value.
(define (fetch-request? v)
  (and (fetch-request-info? v)
       (fetch-constructor-present? "Request")
       (external? (fetch-request-raw v))
       (js-instanceof (fetch-request-raw v) (js-var "Request"))))

;; fetch-response? : any/c -> boolean?
;;   Report whether v is a wrapped Response value.
(define (fetch-response? v)
  (and (fetch-response-info? v)
       (fetch-constructor-present? "Response")
       (external? (fetch-response-raw v))
       (js-instanceof (fetch-response-raw v) (js-var "Response"))))

;; make-fetch-headers : [any/c #f] -> fetch-headers-info?
;;   Create a new Headers object from an optional initializer.
(define (make-fetch-headers [init #f])
  (unless (fetch-constructor-present? "Headers")
    (error 'make-fetch-headers "Headers constructor is not available"))
  (define init* (fetch-resolve-optional init))
  (fetch-headers (js-new (js-var "Headers") (vector init*))))

;; make-fetch-request : any/c [any/c #f] -> fetch-request-info?
;;   Create a new Request object from an input and optional initializer.
(define (make-fetch-request input [init #f])
  (unless (fetch-constructor-present? "Request")
    (error 'make-fetch-request "Request constructor is not available"))
  (define input* (if (symbol? input) (symbol->string input) input))
  (define init* (fetch-resolve-optional init))
  (fetch-request (js-new (js-var "Request") (vector input* init*))))

;; fetch : any/c [any/c #f] -> external/raw
;;   Start a fetch request.
(define (fetch request [init #f])
  (define request* (if (fetch-request? request)
                       (fetch-request-raw request)
                       request))
  (js-window-fetch request* (fetch-resolve-optional init)))

;; fetch-request-method : fetch-request-info? -> any/c
;;   Read the request method.
(define (fetch-request-method request)
  (unless (fetch-request? request)
    (raise-argument-error 'fetch-request-method "fetch-request-info?" request))
  (js-ref (fetch-request-raw request) "method"))

;; fetch-request-url : fetch-request-info? -> any/c
;;   Read the request URL.
(define (fetch-request-url request)
  (unless (fetch-request? request)
    (raise-argument-error 'fetch-request-url "fetch-request-info?" request))
  (js-ref (fetch-request-raw request) "url"))

;; fetch-request-body-used? : fetch-request-info? -> boolean?
;;   Report whether the request body has already been consumed.
(define (fetch-request-body-used? request)
  (unless (fetch-request? request)
    (raise-argument-error 'fetch-request-body-used? "fetch-request-info?" request))
  (js-ref (fetch-request-raw request) "bodyUsed"))

;; fetch-request-headers : fetch-request-info? -> fetch-headers-info?
;;   Read the request headers wrapper.
(define (fetch-request-headers request)
  (unless (fetch-request? request)
    (raise-argument-error 'fetch-request-headers "fetch-request-info?" request))
  (fetch-headers (js-ref (fetch-request-raw request) "headers")))

;; fetch-request-clone : fetch-request-info? -> fetch-request-info?
;;   Clone a request.
(define (fetch-request-clone request)
  (unless (fetch-request? request)
    (raise-argument-error 'fetch-request-clone "fetch-request-info?" request))
  (fetch-request (js-send/extern (fetch-request-raw request) "clone" (vector))))

;; fetch-response-ok? : fetch-response-info? -> boolean?
;;   Report whether the response status is in the successful range.
(define (fetch-response-ok? response)
  (unless (fetch-response? response)
    (raise-argument-error 'fetch-response-ok? "fetch-response-info?" response))
  (js-ref (fetch-response-raw response) "ok"))

;; fetch-response-status : fetch-response-info? -> exact-nonnegative-integer?
;;   Read the response status code.
(define (fetch-response-status response)
  (unless (fetch-response? response)
    (raise-argument-error 'fetch-response-status "fetch-response-info?" response))
  (js-ref (fetch-response-raw response) "status"))

;; fetch-response-status-text : fetch-response-info? -> string?
;;   Read the response status text.
(define (fetch-response-status-text response)
  (unless (fetch-response? response)
    (raise-argument-error 'fetch-response-status-text "fetch-response-info?" response))
  (js-ref (fetch-response-raw response) "statusText"))

;; fetch-response-url : fetch-response-info? -> string?
;;   Read the final response URL.
(define (fetch-response-url response)
  (unless (fetch-response? response)
    (raise-argument-error 'fetch-response-url "fetch-response-info?" response))
  (js-ref (fetch-response-raw response) "url"))

;; fetch-response-type : fetch-response-info? -> string?
;;   Read the response type.
(define (fetch-response-type response)
  (unless (fetch-response? response)
    (raise-argument-error 'fetch-response-type "fetch-response-info?" response))
  (js-ref (fetch-response-raw response) "type"))

;; fetch-response-body-used? : fetch-response-info? -> boolean?
;;   Report whether the response body has already been consumed.
(define (fetch-response-body-used? response)
  (unless (fetch-response? response)
    (raise-argument-error 'fetch-response-body-used? "fetch-response-info?" response))
  (js-ref (fetch-response-raw response) "bodyUsed"))

;; fetch-response-headers : fetch-response-info? -> fetch-headers-info?
;;   Read the response headers wrapper.
(define (fetch-response-headers response)
  (unless (fetch-response? response)
    (raise-argument-error 'fetch-response-headers "fetch-response-info?" response))
  (fetch-headers (js-ref (fetch-response-raw response) "headers")))

;; fetch-response-clone : fetch-response-info? -> fetch-response-info?
;;   Clone a response.
(define (fetch-response-clone response)
  (unless (fetch-response? response)
    (raise-argument-error 'fetch-response-clone "fetch-response-info?" response))
  (fetch-response (js-send/extern (fetch-response-raw response) "clone" (vector))))

;; fetch-headers-get : fetch-headers-info? (or/c string? symbol?) -> (or/c #f string?)
;;   Read a header value by name.
(define (fetch-headers-get headers name)
  (unless (fetch-headers? headers)
    (raise-argument-error 'fetch-headers-get "fetch-headers-info?" headers))
  (define name* (fetch-stringish->string 'fetch-headers-get name))
  (js-send/value (fetch-headers-raw headers) "get" (vector name*)))

;; fetch-headers-has? : fetch-headers-info? (or/c string? symbol?) -> boolean?
;;   Check whether a header is present.
(define (fetch-headers-has? headers name)
  (unless (fetch-headers? headers)
    (raise-argument-error 'fetch-headers-has? "fetch-headers-info?" headers))
  (define name* (fetch-stringish->string 'fetch-headers-has? name))
  (js-send/boolean (fetch-headers-raw headers) "has" (vector name*)))

;; fetch-headers-set! : fetch-headers-info? (or/c string? symbol?) (or/c string? symbol?) -> void?
;;   Set a header value.
(define (fetch-headers-set! headers name value)
  (unless (fetch-headers? headers)
    (raise-argument-error 'fetch-headers-set! "fetch-headers-info?" headers))
  (define name* (fetch-stringish->string 'fetch-headers-set! name))
  (define value* (fetch-stringish->string 'fetch-headers-set! value))
  (js-send/extern/nullish (fetch-headers-raw headers) "set" (vector name* value*))
  (void))

;; fetch-headers-append! : fetch-headers-info? (or/c string? symbol?) (or/c string? symbol?) -> void?
;;   Append a header value.
(define (fetch-headers-append! headers name value)
  (unless (fetch-headers? headers)
    (raise-argument-error 'fetch-headers-append! "fetch-headers-info?" headers))
  (define name* (fetch-stringish->string 'fetch-headers-append! name))
  (define value* (fetch-stringish->string 'fetch-headers-append! value))
  (js-send/extern/nullish (fetch-headers-raw headers) "append" (vector name* value*))
  (void))

;; fetch-headers-delete! : fetch-headers-info? (or/c string? symbol?) -> void?
;;   Delete a header.
(define (fetch-headers-delete! headers name)
  (unless (fetch-headers? headers)
    (raise-argument-error 'fetch-headers-delete! "fetch-headers-info?" headers))
  (define name* (fetch-stringish->string 'fetch-headers-delete! name))
  (js-send/extern/nullish (fetch-headers-raw headers) "delete" (vector name*))
  (void))

;; fetch-headers-callback-cache : hash?
;;   Cache JS callback wrappers for Headers iteration.
(define fetch-headers-callback-cache (make-hasheq))

;; fetch-headers->external : any/c -> external?
;;   Convert a Headers callback to a stable external value.
(define (fetch-headers->external proc)
  (cond
    [(external? proc) proc]
    [(procedure? proc)
     (define cached (hash-ref fetch-headers-callback-cache proc #f))
     (cond
       [cached cached]
       [else
        (define external (procedure->external proc))
        (hash-set! fetch-headers-callback-cache proc external)
        external])]
    [else
     (raise-argument-error 'fetch-headers-for-each "(or/c procedure? external?)" proc)]))

;; fetch-headers-for-each : fetch-headers-info? (or/c procedure? external?) [any/c] -> void?
;;   Iterate over headers with a Map-style callback.
(define (fetch-headers-for-each headers proc [this-arg #f])
  (unless (fetch-headers? headers)
    (raise-argument-error 'fetch-headers-for-each "fetch-headers-info?" headers))
  (define proc* (fetch-headers->external proc))
  (define args
    (if (eq? this-arg #f)
        (vector proc*)
        (vector proc* this-arg)))
  (js-send/extern/nullish (fetch-headers-raw headers) "forEach" args)
  (void))
