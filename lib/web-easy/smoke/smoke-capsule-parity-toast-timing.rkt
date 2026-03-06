#lang webracket

;;;
;;; Smoke Capsule: Parity Toast Timing
;;;

;; Isolated parity capsule for toast duration and pause-on-hover behavior in parity-all.
;;
;; Exports:
;;   parity-toast-timing-make-page   Build and mount the parity page under root.
;;   parity-toast-timing-run-test    Execute capsule-local setup checks.
;;   parity-toast-timing-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-toast-timing-make-page
                parity-toast-timing-run-test
                parity-toast-timing-cleanup)
  (let ()
    ;; Constants for parity toast timing capsule state.
    (define parity-toast-timing-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-toast-timing-make-page : any/c -> void?
    ;;   Build and mount the parity toast timing page under root.
    (define (parity-toast-timing-make-page root)
      (define @open (@ #f))
      (define @mode (@ 'idle))
      (define @duration (@ 0))
      (define @pause (@ #f))

      ;; show-auto! : -> void?
      ;;   Show auto-dismissing parity toast without hover pause.
      (define (show-auto!)
        (:= @mode 'auto)
        (:= @duration 350)
        (:= @pause #f)
        (:= @open #t))

      ;; show-paused! : -> void?
      ;;   Show auto-dismissing parity toast with hover pause.
      (define (show-paused!)
        (:= @mode 'paused)
        (:= @duration 350)
        (:= @pause #t)
        (:= @open #t))

      (set! parity-toast-timing-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "show-auto" show-auto!)
                (button "show-paused" show-paused!))
               (toast @open
                      (lambda ()
                        (:= @open #f))
                      "Parity build complete"
                      'success
                      "Parity Build"
                      #t
                      @duration
                      @pause)
               (text (~> @open
                         (lambda (open?)
                           (~a "parity-open:" open?))))
               (text (~> @mode
                         (lambda (mode)
                           (~a "parity-mode:" mode))))))))
      (mount-renderer! parity-toast-timing-renderer root)
      (void))

    ;; parity-toast-timing-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-toast-timing-run-test _root)
      (and parity-toast-timing-renderer #t))

    ;; parity-toast-timing-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-toast-timing-cleanup _root)
      (when parity-toast-timing-renderer
        (renderer-destroy parity-toast-timing-renderer)
        (set! parity-toast-timing-renderer #f))
      (void))

    (values parity-toast-timing-make-page
            parity-toast-timing-run-test
            parity-toast-timing-cleanup)))
