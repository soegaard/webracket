#lang webracket

;;;
;;; Smoke Capsule: Modal
;;;

;; Isolated smoke capsule for modal open/close and close-reason behavior in smoke-all.
;;
;; Exports:
;;   modal-make-page   Build and mount the modal page under root.
;;   modal-run-test    Execute capsule-local setup checks.
;;   modal-cleanup     Destroy mounted renderer state for this capsule.

(define-values (modal-make-page modal-run-test modal-cleanup)
  (let ()
    ;; Constants for modal capsule state.
    (define modal-renderer #f) ; Mounted renderer for this capsule.

    ;; modal-make-page : any/c -> void?
    ;;   Build and mount the modal page under root.
    (define (modal-make-page root)
      ;; Constants for observable state.
      (define @open   (@ #f))
      (define @status (@ "idle"))

      ;; open-modal! : -> void?
      ;;   Show modal and set status.
      (define (open-modal!)
        (:= @open #t)
        (:= @status "open"))

      ;; close-escape! : -> void?
      ;;   Close modal from Escape handling.
      (define (close-escape!)
        (:= @open #f)
        (:= @status "escape"))

      ;; close-cancel! : -> void?
      ;;   Close modal from cancel action.
      (define (close-cancel!)
        (:= @open #f)
        (:= @status "cancel"))

      ;; close-confirm! : -> void?
      ;;   Close modal from confirm action.
      (define (close-confirm!)
        (:= @open #f)
        (:= @status "confirm"))

      (set! modal-renderer
            (render
             (window
              (vpanel
               (button "open-modal" open-modal!)
               (modal @open
                      close-escape!
                      (vpanel
                       (text "Delete project?")
                       (hpanel
                        (button "cancel" close-cancel!)
                        (button "confirm" close-confirm!))))
               (text (~> @status
                         (lambda (status)
                           (~a "status:" status))))))))
      (mount-renderer! modal-renderer root)
      (void))

    ;; modal-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (modal-run-test _root)
      (and modal-renderer #t))

    ;; modal-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (modal-cleanup _root)
      (when modal-renderer
        (renderer-destroy modal-renderer)
        (set! modal-renderer #f))
      (void))

    (values modal-make-page modal-run-test modal-cleanup)))
