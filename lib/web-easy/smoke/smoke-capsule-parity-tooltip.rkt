#lang webracket

;;;
;;; Smoke Capsule: Parity Tooltip
;;;

;; Isolated parity capsule for tooltip behavior in parity-all.
;;
;; Exports:
;;   parity-tooltip-make-page   Build and mount the parity tooltip page under root.
;;   parity-tooltip-run-test    Execute capsule-local setup checks.
;;   parity-tooltip-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-tooltip-make-page parity-tooltip-run-test parity-tooltip-cleanup)
  (let ()
    ;; Constants for parity tooltip capsule state.
    (define parity-tooltip-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-tooltip-make-page : any/c -> void?
    ;;   Build and mount the parity tooltip page under root.
    (define (parity-tooltip-make-page root)
      (define @hint (@ "Open release notes"))
      (set! parity-tooltip-renderer
            (render
             (window
              (vpanel
               (button "retitle"
                       (lambda ()
                         (:= @hint "Open release notes (new)")))
               (tooltip
                         @hint
                         (button "details"
                                 (lambda ()
                                   (void)))
                         #:placement 'top)))))
      (mount-renderer! parity-tooltip-renderer root)
      (void))

    ;; parity-tooltip-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-tooltip-run-test _root)
      (and parity-tooltip-renderer #t))

    ;; parity-tooltip-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-tooltip-cleanup _root)
      (when parity-tooltip-renderer
        (renderer-destroy parity-tooltip-renderer)
        (set! parity-tooltip-renderer #f))
      (void))

    (values parity-tooltip-make-page parity-tooltip-run-test parity-tooltip-cleanup)))
