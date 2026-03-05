#lang webracket

;;;
;;; Smoke Capsule: Parity Badge
;;;

;; Isolated parity capsule for badge severity and text updates in parity-all.
;;
;; Exports:
;;   parity-badge-make-page   Build and mount the parity page under root.
;;   parity-badge-run-test    Execute capsule-local setup checks.
;;   parity-badge-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-badge-make-page parity-badge-run-test parity-badge-cleanup)
  (let ()
    ;; Constants for parity badge capsule state.
    (define parity-badge-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-badge-make-page : any/c -> void?
    ;;   Build and mount the parity badge page under root.
    (define (parity-badge-make-page root)
      (define @message (@ "preview"))
      (define @level (@ 'info))
      (set! parity-badge-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "set-warn"
                        (lambda ()
                          (:= @level 'warn)
                          (:= @message "needs-attention")))
                (button "set-success"
                        (lambda ()
                          (:= @level 'success)
                          (:= @message "ready")))
                (button "set-error"
                        (lambda ()
                          (:= @level 'error)
                          (:= @message "broken"))))
               (badge @message @level)))))
      (mount-renderer! parity-badge-renderer root)
      (void))

    ;; parity-badge-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-badge-run-test _root)
      (and parity-badge-renderer #t))

    ;; parity-badge-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-badge-cleanup _root)
      (when parity-badge-renderer
        (renderer-destroy parity-badge-renderer)
        (set! parity-badge-renderer #f))
      (void))

    (values parity-badge-make-page parity-badge-run-test parity-badge-cleanup)))
