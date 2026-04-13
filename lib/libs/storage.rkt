#lang webracket

;;;
;;; Web Storage wrappers
;;;

;; This library exposes direct `localStorage` and `sessionStorage`
;; wrappers on top of the browser's `Storage` object.

;; storage-stringish->string : symbol? any/c -> string?
;;   Normalize a storage key or value to a browser string.
(define (storage-stringish->string who v)
  (cond
    [(string? v) v]
    [(symbol? v) (symbol->string v)]
    [else
     (raise-argument-error who "(or/c string? symbol?)" v)]))

;; storage-info : external/raw -> storage-info?
;;   Wrap a browser Storage object.
(struct storage-info (raw) #:transparent)

;; storage : external/raw -> storage-info?
;;   Wrap a browser Storage object.
(define (storage raw)
  (storage-info raw))

;; storage-raw : storage-info? -> external/raw
;;   Extract the raw Storage object.
(define (storage-raw value)
  (storage-info-raw value))

;; storage? : any/c -> boolean?
;;   Report whether v is a wrapped Storage value.
(define (storage? v)
  (storage-info? v))

;; local-storage : -> storage-info?
;;   Read the browser localStorage object.
(define (local-storage)
  (storage (js-window-local-storage)))

;; session-storage : -> storage-info?
;;   Read the browser sessionStorage object.
(define (session-storage)
  (storage (js-window-session-storage)))

;; storage-length : storage-info? -> exact-nonnegative-integer?
;;   Read the number of stored entries.
(define (storage-length value)
  (unless (storage? value)
    (raise-argument-error 'storage-length "storage-info?" value))
  (js-ref (storage-raw value) "length"))

;; storage-key : storage-info? exact-nonnegative-integer? -> (or/c #f string?)
;;   Read a storage key by numeric position.
(define (storage-key value index)
  (unless (storage? value)
    (raise-argument-error 'storage-key "storage-info?" value))
  (js-send/value (storage-raw value) "key" (vector index)))

;; storage-get-item : storage-info? (or/c string? symbol?) -> (or/c #f string?)
;;   Read a stored value by key.
(define (storage-get-item value key)
  (unless (storage? value)
    (raise-argument-error 'storage-get-item "storage-info?" value))
  (define key* (storage-stringish->string 'storage-get-item key))
  (js-send/value (storage-raw value) "getItem" (vector key*)))

;; storage-set-item! : storage-info? (or/c string? symbol?) (or/c string? symbol?) -> void?
;;   Write a stored value by key.
(define (storage-set-item! value key data)
  (unless (storage? value)
    (raise-argument-error 'storage-set-item! "storage-info?" value))
  (define key* (storage-stringish->string 'storage-set-item! key))
  (define data* (storage-stringish->string 'storage-set-item! data))
  (js-send/extern/nullish (storage-raw value) "setItem" (vector key* data*))
  (void))

;; storage-remove-item! : storage-info? (or/c string? symbol?) -> void?
;;   Remove a stored value by key.
(define (storage-remove-item! value key)
  (unless (storage? value)
    (raise-argument-error 'storage-remove-item! "storage-info?" value))
  (define key* (storage-stringish->string 'storage-remove-item! key))
  (js-send/extern/nullish (storage-raw value) "removeItem" (vector key*))
  (void))

;; storage-clear! : storage-info? -> void?
;;   Remove all stored values.
(define (storage-clear! value)
  (unless (storage? value)
    (raise-argument-error 'storage-clear! "storage-info?" value))
  (js-send/extern/nullish (storage-raw value) "clear" (vector))
  (void))
