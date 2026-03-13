#lang webracket

;;;
;;; Smoke Capsule: Badge
;;;

;; Isolated smoke capsule for badge severity and text updates in smoke-all.
;;
;; Exports:
;;   badge-make-page   Build and mount the badge page under root.
;;   badge-run-test    Execute capsule-local setup checks.
;;   badge-cleanup     Destroy mounted renderer state for this capsule.

(define-values (badge-make-page badge-run-test badge-cleanup)
  (let ()
    ;; Constants for badge capsule state.
    (define badge-renderer #f) ; Mounted renderer for this capsule.

    ;; badge-make-page : any/c -> void?
    ;;   Build and mount the badge page under root.
    (define (badge-make-page root)
      (define @message (@ "beta"))
      (define @level   (@ 'info))
      (set! badge-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "set-info"
                        (lambda ()
                          (:= @level 'info)
                          (:= @message "beta")))
                (button "set-warning"
                        (lambda ()
                          (:= @level 'warning)
                          (:= @message "degraded")))
                (button "set-success"
                        (lambda ()
                          (:= @level 'success)
                          (:= @message "stable")))
                (button "set-danger"
                        (lambda ()
                          (:= @level 'danger)
                          (:= @message "failed"))))
               (badge @message @level)))))
      (mount-renderer! badge-renderer root)
      (void))

    ;; badge-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (badge-run-test _root)
      (and badge-renderer #t))

    ;; badge-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (badge-cleanup _root)
      (when badge-renderer
        (renderer-destroy badge-renderer)
        (set! badge-renderer #f))
      (void))

    (values badge-make-page badge-run-test badge-cleanup)))
