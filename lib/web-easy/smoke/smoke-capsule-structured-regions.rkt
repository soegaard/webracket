#lang webracket

;;;
;;; Smoke Capsule: Structured Regions
;;;

;; Isolated smoke capsule for structured alert/card/dialog/modal region hooks.
;;
;; Exports:
;;   structured-regions-make-page   Build and mount the structured regions page under root.
;;   structured-regions-run-test    Execute capsule-local setup checks.
;;   structured-regions-cleanup     Destroy mounted renderer state for this capsule.

(define-values (structured-regions-make-page structured-regions-run-test structured-regions-cleanup)
  (let ()
    ;; Constants for structured-regions capsule state.
    (define structured-regions-renderer #f) ; Mounted renderer for this capsule.

    ;; structured-regions-make-page : any/c -> void?
    ;;   Build and mount the structured regions page under root.
    (define (structured-regions-make-page root)
      ;; Constants for observable state.
      (define @alert-visible? (@ #t))
      (define @dialog-open?   (@ #t))
      (define @modal-open?    (@ #t))

      ;; dismiss-alert! : -> void?
      ;;   Hide the alert-rich fixture.
      (define (dismiss-alert!)
        (:= @alert-visible? #f))

      (set! structured-regions-renderer
            (render
             (window
              (vpanel
               (if-view @alert-visible?
                        (alert-rich
                                  "Disk almost full."
                                  "Warning"
                                  "Read details"
                                  "/alerts"
                                  #:level 'warning
                                  #:dismiss-action dismiss-alert!
                                  #:dismiss-label "Close warning")
                        (text "alert-dismissed"))
               (card
                         "Card header"
                         "Card footer"
                         (text "Card body")
                         #:subtitle "Card subtitle"
                         #:media (text "Card media")
                         #:actions (list (button "save" (lambda () (void)))
                                         (button "cancel" (lambda () (void)))))
               (dialog
                         @dialog-open?
                         (lambda () (:= @dialog-open? #f))
                         (text "Dialog body")
                         #:size 'md
                         #:title "Dialog title"
                         #:description "Dialog description"
                         #:show-close? #t
                         #:footer (button "dialog-footer-action" (lambda () (void))))
               (modal
                         @modal-open?
                         (lambda () (:= @modal-open? #f))
                         (text "Modal body")
                         #:size 'md
                         #:title "Modal title"
                         #:description "Modal description"
                         #:show-close? #t
                         #:footer (button "modal-footer-action" (lambda () (void))))))))
      (mount-renderer! structured-regions-renderer root)
      (void))

    ;; structured-regions-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (structured-regions-run-test _root)
      (and structured-regions-renderer #t))

    ;; structured-regions-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (structured-regions-cleanup _root)
      (when structured-regions-renderer
        (renderer-destroy structured-regions-renderer)
        (set! structured-regions-renderer #f))
      (void))

    (values structured-regions-make-page structured-regions-run-test structured-regions-cleanup)))
