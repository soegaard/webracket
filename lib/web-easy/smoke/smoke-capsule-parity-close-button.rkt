#lang webracket

;;;
;;; Smoke Capsule: Parity Close Button
;;;

;; Isolated parity capsule for close-button action behavior in parity-all.
;;
;; Exports:
;;   parity-close-button-make-page   Build and mount the parity page under root.
;;   parity-close-button-run-test    Execute capsule-local setup checks.
;;   parity-close-button-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-close-button-make-page parity-close-button-run-test parity-close-button-cleanup)
  (let ()
    ;; Constants for parity close-button capsule state.
    (define parity-close-button-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-close-button-make-page : any/c -> void?
    ;;   Build and mount the parity close-button page under root.
    (define (parity-close-button-make-page root)
      (define @count (@ 0))
      (set! parity-close-button-renderer
            (render
             (window
              (vpanel
               (close-button (lambda ()
                               (:= @count (+ (obs-peek @count) 1)))
                             "Close parity panel")
               (text (~> @count
                         (lambda (n)
                           (~a "parity-close-clicks:" n))))))))
      (mount-renderer! parity-close-button-renderer root)
      (void))

    ;; parity-close-button-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-close-button-run-test _root)
      (and parity-close-button-renderer #t))

    ;; parity-close-button-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-close-button-cleanup _root)
      (when parity-close-button-renderer
        (renderer-destroy parity-close-button-renderer)
        (set! parity-close-button-renderer #f))
      (void))

    (values parity-close-button-make-page parity-close-button-run-test parity-close-button-cleanup)))
