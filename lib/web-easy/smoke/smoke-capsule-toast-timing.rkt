#lang webracket

;;;
;;; Smoke Capsule: Toast Timing
;;;

;; Isolated smoke capsule for toast duration and pause-on-hover behavior in smoke-all.
;;
;; Exports:
;;   toast-timing-make-page   Build and mount the page under root.
;;   toast-timing-run-test    Execute capsule-local setup checks.
;;   toast-timing-cleanup     Destroy mounted renderer state for this capsule.

(define-values (toast-timing-make-page toast-timing-run-test toast-timing-cleanup)
  (let ()
    ;; Constants for toast timing capsule state.
    (define toast-timing-renderer #f) ; Mounted renderer for this capsule.

    ;; toast-timing-make-page : any/c -> void?
    ;;   Build and mount the toast timing page under root.
    (define (toast-timing-make-page root)
      (define @open (@ #f))
      (define @mode (@ 'idle))
      (define @duration (@ 0))
      (define @pause (@ #f))

      ;; show-auto! : -> void?
      ;;   Show auto-dismissing toast without hover pause.
      (define (show-auto!)
        (:= @mode 'auto)
        (:= @duration 350)
        (:= @pause #f)
        (:= @open #t))

      ;; show-paused! : -> void?
      ;;   Show auto-dismissing toast with hover pause.
      (define (show-paused!)
        (:= @mode 'paused)
        (:= @duration 350)
        (:= @pause #t)
        (:= @open #t))

      (set! toast-timing-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "show-auto" show-auto!)
                (button "show-paused" show-paused!))
               (toast @open
                      (lambda ()
                        (:= @open #f))
                      "Build complete"
                      'success
                      "Build"
                      #t
                      @duration
                      @pause)
               (text (~> @open
                         (lambda (open?)
                           (~a "open:" open?))))
               (text (~> @mode
                         (lambda (mode)
                           (~a "mode:" mode))))))))
      (mount-renderer! toast-timing-renderer root)
      (void))

    ;; toast-timing-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (toast-timing-run-test _root)
      (and toast-timing-renderer #t))

    ;; toast-timing-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (toast-timing-cleanup _root)
      (when toast-timing-renderer
        (renderer-destroy toast-timing-renderer)
        (set! toast-timing-renderer #f))
      (void))

    (values toast-timing-make-page toast-timing-run-test toast-timing-cleanup)))
