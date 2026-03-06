#lang webracket

;;;
;;; Smoke Capsule: Dialog No Desc
;;;

;; Isolated smoke capsule for dialog without first-text description.
;;
;; Exports:
;;   dialog-no-desc-make-page   Build and mount dialog-no-desc page under root.
;;   dialog-no-desc-run-test    Execute capsule-local setup checks.
;;   dialog-no-desc-cleanup     Destroy mounted renderer state for this capsule.

(define-values (dialog-no-desc-make-page dialog-no-desc-run-test dialog-no-desc-cleanup)
  (let ()
    ;; Constants for dialog-no-desc capsule state.
    (define dialog-no-desc-renderer #f) ; Mounted renderer for this capsule.

    ;; dialog-no-desc-make-page : any/c -> void?
    ;;   Build and mount dialog-no-desc page under root.
    (define (dialog-no-desc-make-page root)
      (define @open? (@ #f))
      (define @status (@ "idle"))
      (set! dialog-no-desc-renderer
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
               (text (~> @status (lambda (s) (~a "status:" s))))))))
      (mount-renderer! dialog-no-desc-renderer root)
      (void))

    ;; dialog-no-desc-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (dialog-no-desc-run-test _root)
      (and dialog-no-desc-renderer #t))

    ;; dialog-no-desc-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (dialog-no-desc-cleanup _root)
      (when dialog-no-desc-renderer
        (renderer-destroy dialog-no-desc-renderer)
        (set! dialog-no-desc-renderer #f))
      (void))

    (values dialog-no-desc-make-page dialog-no-desc-run-test dialog-no-desc-cleanup)))
