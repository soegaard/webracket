#lang webracket

;;;
;;; Smoke Capsule: Parity Dialog No Desc
;;;

;; Isolated parity capsule for dialog without first-text description.
;;
;; Exports:
;;   parity-dialog-no-desc-make-page   Build and mount parity dialog-no-desc page under root.
;;   parity-dialog-no-desc-run-test    Execute capsule-local setup checks.
;;   parity-dialog-no-desc-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-dialog-no-desc-make-page
                parity-dialog-no-desc-run-test
                parity-dialog-no-desc-cleanup)
  (let ()
    ;; Constants for parity dialog-no-desc capsule state.
    (define parity-dialog-no-desc-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-dialog-no-desc-make-page : any/c -> void?
    ;;   Build and mount parity dialog-no-desc page under root.
    (define (parity-dialog-no-desc-make-page root)
      (define @open? (@ #f))
      (define @status (@ "idle"))
      (set! parity-dialog-no-desc-renderer
            (render
             (window
              (vpanel
               (button "open-dialog"
                       (lambda ()
                         (:= @open? #t)
                         (:= @status "open")))
               (dialog @open?
                       (lambda ()
                         (:= @open? #f)
                         (:= @status "esc"))
                       (button "cancel"
                               (lambda ()
                                 (:= @open? #f)
                                 (:= @status "cancel")))
                       (button "confirm"
                               (lambda ()
                                 (:= @open? #f)
                                 (:= @status "confirm"))))
               (text (~> @status (lambda (s) (~a "parity-status:" s))))))))
      (mount-renderer! parity-dialog-no-desc-renderer root)
      (void))

    ;; parity-dialog-no-desc-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-dialog-no-desc-run-test _root)
      (and parity-dialog-no-desc-renderer #t))

    ;; parity-dialog-no-desc-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-dialog-no-desc-cleanup _root)
      (when parity-dialog-no-desc-renderer
        (renderer-destroy parity-dialog-no-desc-renderer)
        (set! parity-dialog-no-desc-renderer #f))
      (void))

    (values parity-dialog-no-desc-make-page
            parity-dialog-no-desc-run-test
            parity-dialog-no-desc-cleanup)))
