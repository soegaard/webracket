#lang webracket

;;;
;;; Smoke Capsule: Parity Placeholder
;;;

;; Isolated parity capsule for placeholder shape and width behavior in parity-all.
;;
;; Exports:
;;   parity-placeholder-make-page   Build and mount the parity page under root.
;;   parity-placeholder-run-test    Execute capsule-local setup checks.
;;   parity-placeholder-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-placeholder-make-page parity-placeholder-run-test parity-placeholder-cleanup)
  (let ()
    ;; Constants for parity placeholder capsule state.
    (define parity-placeholder-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-placeholder-make-page : any/c -> void?
    ;;   Build and mount the parity placeholder page under root.
    (define (parity-placeholder-make-page root)
      (define @shape (@ 'rect))
      (define @width (@ "8em"))
      (set! parity-placeholder-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "shape-text" (lambda () (:= @shape 'text)))
                (button "shape-circle" (lambda () (:= @shape 'circle)))
                (button "width-wide" (lambda () (:= @width "10em")))
                (button "width-narrow" (lambda () (:= @width "5em"))))
               (placeholder @shape @width)
               (text (~> @shape
                         (lambda (s)
                           (~a "parity-shape:" s))))
               (text (~> @width
                         (lambda (w)
                           (~a "parity-width:" w))))))))
      (mount-renderer! parity-placeholder-renderer root)
      (void))

    ;; parity-placeholder-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-placeholder-run-test _root)
      (and parity-placeholder-renderer #t))

    ;; parity-placeholder-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-placeholder-cleanup _root)
      (when parity-placeholder-renderer
        (renderer-destroy parity-placeholder-renderer)
        (set! parity-placeholder-renderer #f))
      (void))

    (values parity-placeholder-make-page parity-placeholder-run-test parity-placeholder-cleanup)))
