#lang webracket

;;;
;;; Smoke Capsule: Toast
;;;

;; Isolated smoke capsule for toast open/close and severity behavior in smoke-all.
;;
;; Exports:
;;   toast-make-page   Build and mount the toast page under root.
;;   toast-run-test    Execute capsule-local setup checks.
;;   toast-cleanup     Destroy mounted renderer state for this capsule.

(define-values (toast-make-page toast-run-test toast-cleanup)
  (let ()
    ;; Constants for toast capsule state.
    (define toast-renderer #f) ; Mounted renderer for this capsule.

    ;; toast-make-page : any/c -> void?
    ;;   Build and mount the toast page under root.
    (define (toast-make-page root)
      (define @open (@ #t))
      (define @message (@ "Build complete"))
      (define @title (@ "Build"))
      (define @level (@ 'success))
      (define @dismissible (@ #t))
      (set! toast-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "show-success"
                        (lambda ()
                          (:= @title "Build")
                          (:= @message "Build complete")
                          (:= @level 'success)
                          (:= @dismissible #t)
                          (:= @open #t)))
                (button "show-danger"
                        (lambda ()
                          (:= @title "Build")
                          (:= @message "Build failed")
                          (:= @level 'danger)
                          (:= @dismissible #t)
                          (:= @open #t)))
                (button "show-locked"
                        (lambda ()
                          (:= @title "Deploy")
                          (:= @message "Deploy in progress")
                          (:= @level 'warning)
                          (:= @dismissible #f)
                          (:= @open #t))))
               (toast @open
                      (lambda () (:= @open #f))
                      @message
                      @level
                      @title
                      @dismissible)))))
      (mount-renderer! toast-renderer root)
      (void))

    ;; toast-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (toast-run-test _root)
      (and toast-renderer #t))

    ;; toast-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (toast-cleanup _root)
      (when toast-renderer
        (renderer-destroy toast-renderer)
        (set! toast-renderer #f))
      (void))

    (values toast-make-page toast-run-test toast-cleanup)))
