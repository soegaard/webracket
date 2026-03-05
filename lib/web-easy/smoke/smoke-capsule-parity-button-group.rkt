#lang webracket

;;;
;;; Smoke Capsule: Parity Button Group
;;;

;; Isolated parity capsule for grouped button layout and actions in parity-all.
;;
;; Exports:
;;   parity-button-group-make-page   Build and mount the parity page under root.
;;   parity-button-group-run-test    Execute capsule-local setup checks.
;;   parity-button-group-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-button-group-make-page parity-button-group-run-test parity-button-group-cleanup)
  (let ()
    ;; Constants for parity button-group capsule state.
    (define parity-button-group-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-button-group-make-page : any/c -> void?
    ;;   Build and mount the parity button-group page under root.
    (define (parity-button-group-make-page root)
      (define @count (@ 5))
      (set! parity-button-group-renderer
            (render
             (window
              (vpanel
               (button-group
                (button "-" (lambda () (:= @count (- (obs-peek @count) 1))))
                (button "+" (lambda () (:= @count (+ (obs-peek @count) 1)))))
               (text (~> @count (lambda (n) (~a "parity-count:" n))))))))
      (mount-renderer! parity-button-group-renderer root)
      (void))

    ;; parity-button-group-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-button-group-run-test _root)
      (and parity-button-group-renderer #t))

    ;; parity-button-group-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-button-group-cleanup _root)
      (when parity-button-group-renderer
        (renderer-destroy parity-button-group-renderer)
        (set! parity-button-group-renderer #f))
      (void))

    (values parity-button-group-make-page parity-button-group-run-test parity-button-group-cleanup)))

