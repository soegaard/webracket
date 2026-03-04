#lang webracket

;;;
;;; Smoke Capsule: Parity dialog
;;;

;; Isolated parity capsule for parity-dialog.
;;
;; Exports:
;;   parity-dialog-make-page   Build and mount the parity page under root.
;;   parity-dialog-run-test    Execute capsule-local setup checks.
;;   parity-dialog-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-dialog-make-page parity-dialog-run-test parity-dialog-cleanup)
  (let ()
    ;; Constants for parity-dialog capsule state.
    (define parity-dialog-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-dialog-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-dialog-make-page root)
      ;; Constants for observable state.
      (define @open   (@ #f))
      (define @status (@ "idle"))

      ;; open-dialog! : -> void?
      ;;   Open dialog and track state.
      (define (open-dialog!)
        (:= @open #t)
        (:= @status "open"))

      ;; close-escape! : -> void?
      ;;   Close dialog from Escape key handling.
      (define (close-escape!)
        (:= @open #f)
        (:= @status "escape"))

      ;; close-cancel! : -> void?
      ;;   Close dialog from cancel action.
      (define (close-cancel!)
        (:= @open #f)
        (:= @status "cancel"))

      ;; close-confirm! : -> void?
      ;;   Close dialog from confirm action.
      (define (close-confirm!)
        (:= @open #f)
        (:= @status "confirm"))

      (define app-renderer
        (render
         (window
          (vpanel
           (button "open-dialog" open-dialog!)
           (dialog @open
                   close-escape!
                   (vpanel
                    (text "Delete project?")
                    (hpanel
                     (button "cancel" close-cancel!)
                     (button "confirm" close-confirm!))))
           (text (~> @status
                     (lambda (status)
                       (string-append "status:" status))))))))

      (set! parity-dialog-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-dialog-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-dialog-run-test _root)
      (and parity-dialog-renderer #t))

    ;; parity-dialog-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-dialog-cleanup _root)
      (when parity-dialog-renderer
        (renderer-destroy parity-dialog-renderer)
        (set! parity-dialog-renderer #f))
      (void))

    (values parity-dialog-make-page parity-dialog-run-test parity-dialog-cleanup)))
