#lang webracket

;;;
;;; Smoke Capsule: Alert
;;;

;; Isolated smoke capsule for alert severity and text updates in smoke-all.
;;
;; Exports:
;;   alert-make-page   Build and mount the alert page under root.
;;   alert-run-test    Execute capsule-local setup checks.
;;   alert-cleanup     Destroy mounted renderer state for this capsule.

(define-values (alert-make-page alert-run-test alert-cleanup)
  (let ()
    ;; Constants for alert capsule state.
    (define alert-renderer #f) ; Mounted renderer for this capsule.

    ;; alert-make-page : any/c -> void?
    ;;   Build and mount the alert page under root.
    (define (alert-make-page root)
      (define @message (@ "Saved successfully"))
      (define @level   (@ 'success))

      ;; Constants for rich alert option coverage.
      (define @rich-body   (@ "Change a few things up"))
      (define @rich-title  (@ "Oh snap!"))
      (define @rich-link   (@ "Change a few things up"))
      (define @rich-level  (@ 'info))
      (define @rich-layout (@ 'stack))
      (define @rich-scale  (@ 'normal))
      (define @rich-tone   (@ 'secondary))

      (set! alert-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "set-info"
                        (lambda ()
                          (:= @level 'info)
                          (:= @message "Informational update")))
                (button "set-warn"
                        (lambda ()
                          (:= @level 'warn)
                          (:= @message "Disk space low")))
                (button "set-error"
                        (lambda ()
                          (:= @level 'error)
                          (:= @message "Save failed")))
                (button "set-success"
                        (lambda ()
                          (:= @level 'success)
                          (:= @message "Saved successfully"))))
               (alert @message @level)
               (hpanel
                (button "rich-inline"       (lambda () (:= @rich-layout 'inline)))
                (button "rich-stack"        (lambda () (:= @rich-layout 'stack)))
                (button "rich-major"        (lambda () (:= @rich-scale 'major)))
                (button "rich-normal"       (lambda () (:= @rich-scale 'normal)))
                (button "rich-tone-info"    (lambda () (:= @rich-tone 'info)))
                (button "rich-tone-primary" (lambda () (:= @rich-tone 'primary)))
                (button "rich-tone-light"   (lambda () (:= @rich-tone 'light))))
               (alert-rich
                         @rich-body
                         @rich-title
                         @rich-link
                         "#"
                         #:id "alert-rich-target"
                         #:level @rich-level
                         #:layout @rich-layout
                         #:scale @rich-scale
                         #:tone @rich-tone
                         #:dismiss-action (lambda () (void)))))))
      (mount-renderer! alert-renderer root)
      (void))

    ;; alert-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (alert-run-test _root)
      (and alert-renderer #t))

    ;; alert-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (alert-cleanup _root)
      (when alert-renderer
        (renderer-destroy alert-renderer)
        (set! alert-renderer #f))
      (void))

    (values alert-make-page alert-run-test alert-cleanup)))
