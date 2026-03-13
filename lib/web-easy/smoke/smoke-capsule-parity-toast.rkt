#lang webracket

;;;
;;; Smoke Capsule: Parity Toast
;;;

;; Isolated parity capsule for toast open/close and severity behavior in parity-all.
;;
;; Exports:
;;   parity-toast-make-page   Build and mount the parity page under root.
;;   parity-toast-run-test    Execute capsule-local setup checks.
;;   parity-toast-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-toast-make-page parity-toast-run-test parity-toast-cleanup)
  (let ()
    ;; Constants for parity toast capsule state.
    (define parity-toast-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-toast-make-page : any/c -> void?
    ;;   Build and mount the parity toast page under root.
    (define (parity-toast-make-page root)
      (define @open (@ #t))
      (define @message (@ "Parity build complete"))
      (define @title (@ "Parity Build"))
      (define @level (@ 'success))
      (define @dismissible (@ #t))
      (set! parity-toast-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "show-danger"
                        (lambda ()
                          (:= @title "Parity Build")
                          (:= @message "Parity build failed")
                          (:= @level 'danger)
                          (:= @dismissible #t)
                          (:= @open #t)))
                (button "show-locked"
                        (lambda ()
                          (:= @title "Parity Deploy")
                          (:= @message "Parity deploy locked")
                          (:= @level 'warning)
                          (:= @dismissible #f)
                          (:= @open #t))))
               (toast @open
                      (lambda () (:= @open #f))
                      @message
                      @level
                      @title
                      @dismissible)))))
      (mount-renderer! parity-toast-renderer root)
      (void))

    ;; parity-toast-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-toast-run-test _root)
      (and parity-toast-renderer #t))

    ;; parity-toast-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-toast-cleanup _root)
      (when parity-toast-renderer
        (renderer-destroy parity-toast-renderer)
        (set! parity-toast-renderer #f))
      (void))

    (values parity-toast-make-page parity-toast-run-test parity-toast-cleanup)))
