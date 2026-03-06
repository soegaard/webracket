#lang webracket

;;;
;;; Smoke Capsule: Parity Modal
;;;

;; Isolated parity capsule for modal open/close and close-reason behavior in parity-all.
;;
;; Exports:
;;   parity-modal-make-page   Build and mount the parity page under root.
;;   parity-modal-run-test    Execute capsule-local setup checks.
;;   parity-modal-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-modal-make-page parity-modal-run-test parity-modal-cleanup)
  (let ()
    ;; Constants for parity modal capsule state.
    (define parity-modal-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-modal-make-page : any/c -> void?
    ;;   Build and mount the parity modal page under root.
    (define (parity-modal-make-page root)
      ;; Constants for observable state.
      (define @open   (@ #f))
      (define @status (@ "idle"))

      ;; open-modal! : -> void?
      ;;   Open modal and track state.
      (define (open-modal!)
        (:= @open #t)
        (:= @status "open"))

      ;; close-escape! : -> void?
      ;;   Close modal from Escape key handling.
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

      (set! parity-modal-renderer
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
                           (~a "parity-status:" status))))))))
      (mount-renderer! parity-modal-renderer root)
      (void))

    ;; parity-modal-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-modal-run-test _root)
      (and parity-modal-renderer #t))

    ;; parity-modal-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-modal-cleanup _root)
      (when parity-modal-renderer
        (renderer-destroy parity-modal-renderer)
        (set! parity-modal-renderer #f))
      (void))

    (values parity-modal-make-page parity-modal-run-test parity-modal-cleanup)))
