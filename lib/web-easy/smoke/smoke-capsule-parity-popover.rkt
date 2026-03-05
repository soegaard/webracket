#lang webracket

;;;
;;; Smoke Capsule: Parity Popover
;;;

;; Isolated parity capsule for popover behavior in parity-all.
;;
;; Exports:
;;   parity-popover-make-page   Build and mount the parity popover page under root.
;;   parity-popover-run-test    Execute capsule-local setup checks.
;;   parity-popover-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-popover-make-page parity-popover-run-test parity-popover-cleanup)
  (let ()
    ;; Constants for parity popover capsule state.
    (define parity-popover-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-popover-make-page : any/c -> void?
    ;;   Build and mount the parity popover page under root.
    (define (parity-popover-make-page root)
      (define @message (@ "Preview release artifacts"))
      (set! parity-popover-renderer
            (render
             (window
              (vpanel
               (button "retitle"
                       (lambda ()
                         (:= @message "Preview release artifacts (updated)")))
               (popover "preview"
                        (text @message)
                        (button "run" (lambda () (void))))))))
      (mount-renderer! parity-popover-renderer root)
      (void))

    ;; parity-popover-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-popover-run-test _root)
      (and parity-popover-renderer #t))

    ;; parity-popover-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-popover-cleanup _root)
      (when parity-popover-renderer
        (renderer-destroy parity-popover-renderer)
        (set! parity-popover-renderer #f))
      (void))

    (values parity-popover-make-page parity-popover-run-test parity-popover-cleanup)))
