#lang webracket

;;;
;;; Smoke Capsule: Progress
;;;

;; Isolated smoke capsule for progress value + variant behavior in smoke-all.
;;
;; Exports:
;;   progress-make-page   Build and mount the progress page under root.
;;   progress-run-test    Execute capsule-local setup checks.
;;   progress-cleanup     Destroy mounted renderer state for this capsule.

(define-values (progress-make-page progress-run-test progress-cleanup)
  (let ()
    ;; Constants for progress capsule state.
    (define progress-renderer #f) ; Mounted renderer for this capsule.

    ;; progress-make-page : any/c -> void?
    ;;   Build and mount the progress page under root.
    (define (progress-make-page root)
      (define @value   (@ 25))
      (define @variant (@ 'info))

      (set! progress-renderer
            (render
             (window
              (vpanel
               (text (~> @value (lambda (n) (~a "value:" n))))
               (text (~> @variant (lambda (v) (~a "variant:" v))))
               (progress
                         @value
                         #:min 0
                         #:max 100
                         #:variant @variant)
               (hpanel
                (button "info"
                        (lambda ()
                          (:= @variant 'info)))
                (button "success"
                        (lambda ()
                          (:= @variant 'success)))
                (button "warning"
                        (lambda ()
                          (:= @variant 'warning)))
                (button "danger"
                        (lambda ()
                          (:= @variant 'danger)))
                (button "inc"
                        (lambda ()
                          (<~ @value (lambda (n)
                                       (if (>= n 95)
                                           100
                                           (+ n 5)))))))))))
      (mount-renderer! progress-renderer root)
      (void))

    ;; progress-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (progress-run-test _root)
      (and progress-renderer #t))

    ;; progress-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (progress-cleanup _root)
      (when progress-renderer
        (renderer-destroy progress-renderer)
        (set! progress-renderer #f))
      (void))

    (values progress-make-page progress-run-test progress-cleanup)))
