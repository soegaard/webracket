#lang webracket

;;;
;;; Smoke Capsule: Link
;;;

;; Isolated smoke capsule for link rendering and attributes in smoke-all.
;;
;; Exports:
;;   link-make-page   Build and mount the link page under root.
;;   link-run-test    Execute capsule-local setup checks.
;;   link-cleanup     Destroy mounted renderer state for this capsule.

(define-values (link-make-page link-run-test link-cleanup)
  (let ()
    ;; Constants for link capsule state.
    (define link-renderer #f) ; Mounted renderer for this capsule.

    ;; link-make-page : any/c -> void?
    ;;   Build and mount the link page under root.
    (define (link-make-page root)
      (set! link-renderer
            (render
             (window
              (vpanel
               (link "Open docs" "https://example.com/docs" #f "_blank")
               (link "Download spec" "https://example.com/spec.txt" #t #f)))))
      (mount-renderer! link-renderer root)
      (void))

    ;; link-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (link-run-test _root)
      (and link-renderer #t))

    ;; link-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (link-cleanup _root)
      (when link-renderer
        (renderer-destroy link-renderer)
        (set! link-renderer #f))
      (void))

    (values link-make-page link-run-test link-cleanup)))
