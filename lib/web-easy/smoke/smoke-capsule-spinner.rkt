#lang webracket

;;;
;;; Smoke Capsule: Spinner
;;;

;; Isolated smoke capsule for spinner label updates in smoke-all.
;;
;; Exports:
;;   spinner-make-page   Build and mount the spinner page under root.
;;   spinner-run-test    Execute capsule-local setup checks.
;;   spinner-cleanup     Destroy mounted renderer state for this capsule.

(define-values (spinner-make-page spinner-run-test spinner-cleanup)
  (let ()
    ;; Constants for spinner capsule state.
    (define spinner-renderer #f) ; Mounted renderer for this capsule.

    ;; spinner-make-page : any/c -> void?
    ;;   Build and mount the spinner page under root.
    (define (spinner-make-page root)
      (define @label (@ "Loading..."))
      (set! spinner-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "set-syncing"
                        (lambda ()
                          (:= @label "Syncing")))
                (button "set-done"
                        (lambda ()
                          (:= @label "Done"))))
               (spinner @label)))))
      (mount-renderer! spinner-renderer root)
      (void))

    ;; spinner-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (spinner-run-test _root)
      (and spinner-renderer #t))

    ;; spinner-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (spinner-cleanup _root)
      (when spinner-renderer
        (renderer-destroy spinner-renderer)
        (set! spinner-renderer #f))
      (void))

    (values spinner-make-page spinner-run-test spinner-cleanup)))
