#lang webracket

;;;
;;; Smoke Capsule: Divider
;;;

;; Isolated smoke capsule for horizontal and vertical divider rendering in smoke-all.
;;
;; Exports:
;;   divider-make-page   Build and mount the divider page under root.
;;   divider-run-test    Execute capsule-local setup checks.
;;   divider-cleanup     Destroy mounted renderer state for this capsule.

(define-values (divider-make-page divider-run-test divider-cleanup)
  (let ()
    ;; Constants for divider capsule state.
    (define divider-renderer #f) ; Mounted renderer for this capsule.

    ;; divider-make-page : any/c -> void?
    ;;   Build and mount the divider page under root.
    (define (divider-make-page root)
      (set! divider-renderer
            (render
             (window
              (vpanel
               (text "top")
               (divider #:orientation 'horizontal)
               (text "bottom")
               (hpanel
                (text "left")
                (divider #:orientation 'vertical)
                (text "right"))))))
      (mount-renderer! divider-renderer root)
      (void))

    ;; divider-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (divider-run-test _root)
      (and divider-renderer #t))

    ;; divider-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (divider-cleanup _root)
      (when divider-renderer
        (renderer-destroy divider-renderer)
        (set! divider-renderer #f))
      (void))

    (values divider-make-page divider-run-test divider-cleanup)))
