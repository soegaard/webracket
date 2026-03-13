#lang webracket

;;;
;;; Smoke Capsule: Parity progress
;;;

;; Isolated parity capsule for parity-progress.
;;
;; Exports:
;;   parity-progress-make-page   Build and mount the parity page under root.
;;   parity-progress-run-test    Execute capsule-local setup checks.
;;   parity-progress-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-progress-make-page parity-progress-run-test parity-progress-cleanup)
  (let ()
    ;; Constants for parity-progress capsule state.
    (define parity-progress-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-progress-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-progress-make-page root)
      (define @value   (@ 30))
      (define @variant (@ 'info))

      (set! parity-progress-renderer
            (render
             (window
              (vpanel
               (text (~> @value (lambda (n) (~a "progress:" n))))
               (progress
                         @value
                         #:min 0
                         #:max 100
                         #:variant @variant)
               (hpanel
                (button "set-success"
                        (lambda ()
                          (:= @variant 'success)))
                (button "set-warning"
                        (lambda ()
                          (:= @variant 'warning)))
                (button "set-danger"
                        (lambda ()
                          (:= @variant 'danger)))
                (button "step"
                        (lambda ()
                          (<~ @value (lambda (n)
                                       (if (>= n 90)
                                           100
                                           (+ n 10)))))))))))

      (mount-renderer! parity-progress-renderer root)
      (void))

    ;; parity-progress-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-progress-run-test _root)
      (and parity-progress-renderer #t))

    ;; parity-progress-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-progress-cleanup _root)
      (when parity-progress-renderer
        (renderer-destroy parity-progress-renderer)
        (set! parity-progress-renderer #f))
      (void))

    (values parity-progress-make-page parity-progress-run-test parity-progress-cleanup)))
