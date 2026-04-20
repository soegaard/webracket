;;;
;;; Web Pict Bitmaps
;;;

;; Browser-side bitmap caching and image loading helpers for `web-pict`.

(define loaded-bitmaps (make-hash))

    ;; bitmap-ref : string? [any/c] -> any/c
    ;;   Return a previously loaded bitmap, or default when absent.
;; bitmap-ref : string? [any/c] -> any/c
;;   Return a previously loaded bitmap, or default when absent.
(define (bitmap-ref url [default #f])
  (hash-ref loaded-bitmaps url (lambda () default)))

    ;; register-bitmap! : string? any/c -> void?
    ;;   Store an image value in the bitmap cache.
;; register-bitmap! : string? any/c -> void?
;;   Store an image value in the bitmap cache.
(define (register-bitmap! url image)
  (hash-set! loaded-bitmaps url image))

    ;; call-bitmap-ready : any/c string? any/c -> void?
    ;;   Invoke an optional ready handler with the URL and image.
;; call-bitmap-ready : any/c string? any/c -> void?
;;   Invoke an optional ready handler with the URL and image.
(define (call-bitmap-ready on-ready url image)
  (when on-ready
    (on-ready url image)))

    ;; call-bitmap-error : any/c string? -> void?
    ;;   Invoke an optional error handler with the failing URL.
;; call-bitmap-error : any/c string? -> void?
;;   Invoke an optional error handler with the failing URL.
(define (call-bitmap-error on-error url)
  (if on-error
      (on-error url)
      (js-log (format "Failed to load bitmap: ~a" url))))

    ;; load-bitmap : string? [procedure? #f] [procedure? #f] -> external?
    ;;   Start loading an image, cache it on success, and return the DOM image node.
;; load-bitmap : string? [procedure? #f] [procedure? #f] -> external?
;;   Start loading an image, cache it on success, and return the DOM image node.
(define (load-bitmap url [on-ready #f] [on-error #f])
  (when on-ready
    (unless (procedure? on-ready)
      (raise-argument-error 'load-bitmap "(or/c #f procedure?)" on-ready)))
  (when on-error
    (unless (procedure? on-error)
      (raise-argument-error 'load-bitmap "(or/c #f procedure?)" on-error)))
  (define image (js-create-element "img"))
  (define ready-callback
    (procedure->external
     (lambda _
       (register-bitmap! url image)
       (call-bitmap-ready on-ready url image))))
  (define error-callback
    (procedure->external
     (lambda _
       (call-bitmap-error on-error url))))
  (js-set-attribute! image "crossorigin" "anonymous")
  (js-set-attribute! image "style" "display:none")
  (js-add-event-listener! image "load" ready-callback)
  (js-add-event-listener! image "error" error-callback)
  (js-set-attribute! image "src" url)
  (js-append-child! (js-document-body) image)
  image)
