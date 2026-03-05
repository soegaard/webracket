#lang webracket

;;;
;;; Smoke Capsule: Parity Spinner
;;;

;; Isolated parity capsule for spinner label updates in parity-all.
;;
;; Exports:
;;   parity-spinner-make-page   Build and mount the parity page under root.
;;   parity-spinner-run-test    Execute capsule-local setup checks.
;;   parity-spinner-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-spinner-make-page parity-spinner-run-test parity-spinner-cleanup)
  (let ()
    ;; Constants for parity spinner capsule state.
    (define parity-spinner-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-spinner-make-page : any/c -> void?
    ;;   Build and mount the parity spinner page under root.
    (define (parity-spinner-make-page root)
      (define @label (@ "Parity loading..."))
      (set! parity-spinner-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "set-syncing"
                        (lambda ()
                          (:= @label "Parity syncing")))
                (button "set-done"
                        (lambda ()
                          (:= @label "Parity done"))))
               (spinner @label)))))
      (mount-renderer! parity-spinner-renderer root)
      (void))

    ;; parity-spinner-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-spinner-run-test _root)
      (and parity-spinner-renderer #t))

    ;; parity-spinner-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-spinner-cleanup _root)
      (when parity-spinner-renderer
        (renderer-destroy parity-spinner-renderer)
        (set! parity-spinner-renderer #f))
      (void))

    (values parity-spinner-make-page parity-spinner-run-test parity-spinner-cleanup)))
