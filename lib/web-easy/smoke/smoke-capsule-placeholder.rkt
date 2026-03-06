#lang webracket

;;;
;;; Smoke Capsule: Placeholder
;;;

;; Isolated smoke capsule for placeholder shape and width behavior in smoke-all.
;;
;; Exports:
;;   placeholder-make-page   Build and mount the placeholder page under root.
;;   placeholder-run-test    Execute capsule-local setup checks.
;;   placeholder-cleanup     Destroy mounted renderer state for this capsule.

(define-values (placeholder-make-page placeholder-run-test placeholder-cleanup)
  (let ()
    ;; Constants for placeholder capsule state.
    (define placeholder-renderer #f) ; Mounted renderer for this capsule.

    ;; placeholder-make-page : any/c -> void?
    ;;   Build and mount the placeholder page under root.
    (define (placeholder-make-page root)
      (define @shape (@ 'text))
      (define @width (@ "12em"))
      (set! placeholder-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "shape-text" (lambda () (:= @shape 'text)))
                (button "shape-rect" (lambda () (:= @shape 'rect)))
                (button "shape-circle" (lambda () (:= @shape 'circle)))
                (button "width-wide" (lambda () (:= @width "12em")))
                (button "width-narrow" (lambda () (:= @width "6em"))))
               (placeholder @shape @width)
               (text (~> @shape
                         (lambda (s)
                           (~a "shape:" s))))
               (text (~> @width
                         (lambda (w)
                           (~a "width:" w))))))))
      (mount-renderer! placeholder-renderer root)
      (void))

    ;; placeholder-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (placeholder-run-test _root)
      (and placeholder-renderer #t))

    ;; placeholder-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (placeholder-cleanup _root)
      (when placeholder-renderer
        (renderer-destroy placeholder-renderer)
        (set! placeholder-renderer #f))
      (void))

    (values placeholder-make-page placeholder-run-test placeholder-cleanup)))
