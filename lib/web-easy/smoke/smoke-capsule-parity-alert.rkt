#lang webracket

;;;
;;; Smoke Capsule: Parity Alert
;;;

;; Isolated parity capsule for alert severity and text updates in parity-all.
;;
;; Exports:
;;   parity-alert-make-page   Build and mount the parity page under root.
;;   parity-alert-run-test    Execute capsule-local setup checks.
;;   parity-alert-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-alert-make-page parity-alert-run-test parity-alert-cleanup)
  (let ()
    ;; Constants for parity alert capsule state.
    (define parity-alert-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-alert-make-page : any/c -> void?
    ;;   Build and mount the parity alert page under root.
    (define (parity-alert-make-page root)
      (define @message (@ "Parity saved"))
      (define @level (@ 'success))
      (set! parity-alert-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "set-info"
                        (lambda ()
                          (:= @level 'info)
                          (:= @message "Parity informational update")))
                (button "set-warning"
                        (lambda ()
                          (:= @level 'warning)
                          (:= @message "Parity warning")))
                (button "set-danger"
                        (lambda ()
                          (:= @level 'danger)
                          (:= @message "Parity failed"))))
               (alert @message @level)))))
      (mount-renderer! parity-alert-renderer root)
      (void))

    ;; parity-alert-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-alert-run-test _root)
      (and parity-alert-renderer #t))

    ;; parity-alert-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-alert-cleanup _root)
      (when parity-alert-renderer
        (renderer-destroy parity-alert-renderer)
        (set! parity-alert-renderer #f))
      (void))

    (values parity-alert-make-page parity-alert-run-test parity-alert-cleanup)))
