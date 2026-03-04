#lang webracket

;;;
;;; Smoke Capsule: Dialog
;;;

;; Isolated smoke capsule for dialog open/close behavior in smoke-all.
;;
;; Exports:
;;   dialog-make-page   Build and mount the dialog page under root.
;;   dialog-run-test    Execute capsule-local setup checks.
;;   dialog-cleanup     Destroy mounted renderer state for this capsule.

(define-values (dialog-make-page dialog-run-test dialog-cleanup)
  (let ()
    ;; Constants for dialog capsule state.
    (define dialog-renderer #f) ; Mounted renderer for this capsule.

    ;; dialog-make-page : any/c -> void?
    ;;   Build and mount the dialog page under root.
    (define (dialog-make-page root)
      ;; Constants for observable state.
      (define @open   (@ #f))
      (define @status (@ "idle"))

      ;; open-dialog! : -> void?
      ;;   Show the dialog and mark status as open.
      (define (open-dialog!)
        (:= @open #t)
        (:= @status "open"))

      ;; close-escape! : -> void?
      ;;   Close dialog from Escape key handling.
      (define (close-escape!)
        (:= @open #f)
        (:= @status "escape"))

      ;; close-cancel! : -> void?
      ;;   Close dialog from cancel button.
      (define (close-cancel!)
        (:= @open #f)
        (:= @status "cancel"))

      (set! dialog-renderer
            (render
             (window
              (vpanel
               (button "open-dialog" open-dialog!)
               (dialog @open
                       close-escape!
                       (vpanel
                        (text "Delete project?")
                        (button "cancel" close-cancel!)))
               (text (~> @status
                         (lambda (status)
                           (string-append "status:" status))))))))
      (mount-renderer! dialog-renderer root)
      (void))

    ;; dialog-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (dialog-run-test _root)
      (and dialog-renderer #t))

    ;; dialog-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (dialog-cleanup _root)
      (when dialog-renderer
        (renderer-destroy dialog-renderer)
        (set! dialog-renderer #f))
      (void))

    (values dialog-make-page dialog-run-test dialog-cleanup)))
