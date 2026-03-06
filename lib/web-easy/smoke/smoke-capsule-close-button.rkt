#lang webracket

;;;
;;; Smoke Capsule: Close Button
;;;

;; Isolated smoke capsule for close-button action behavior in smoke-all.
;;
;; Exports:
;;   close-button-make-page   Build and mount the close-button page under root.
;;   close-button-run-test    Execute capsule-local setup checks.
;;   close-button-cleanup     Destroy mounted renderer state for this capsule.

(define-values (close-button-make-page close-button-run-test close-button-cleanup)
  (let ()
    ;; Constants for close-button capsule state.
    (define close-button-renderer #f) ; Mounted renderer for this capsule.

    ;; close-button-make-page : any/c -> void?
    ;;   Build and mount the close-button page under root.
    (define (close-button-make-page root)
      (define @count (@ 0))
      (set! close-button-renderer
            (render
             (window
              (vpanel
               (close-button (lambda ()
                               (:= @count (+ (obs-peek @count) 1)))
                             "Close panel")
               (text (~> @count
                         (lambda (n)
                           (~a "close-clicks:" n))))))))
      (mount-renderer! close-button-renderer root)
      (void))

    ;; close-button-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (close-button-run-test _root)
      (and close-button-renderer #t))

    ;; close-button-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (close-button-cleanup _root)
      (when close-button-renderer
        (renderer-destroy close-button-renderer)
        (set! close-button-renderer #f))
      (void))

    (values close-button-make-page close-button-run-test close-button-cleanup)))
