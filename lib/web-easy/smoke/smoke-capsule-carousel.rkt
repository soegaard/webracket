#lang webracket

;;;
;;; Smoke Capsule: Carousel
;;;

;; Isolated smoke capsule for carousel index navigation in smoke-all.
;;
;; Exports:
;;   carousel-make-page   Build and mount the carousel page under root.
;;   carousel-run-test    Execute capsule-local setup checks.
;;   carousel-cleanup     Destroy mounted renderer state for this capsule.

(define-values (carousel-make-page carousel-run-test carousel-cleanup)
  (let ()
    ;; Constants for carousel capsule state.
    (define carousel-renderer #f) ; Mounted renderer for this capsule.

    ;; carousel-make-page : any/c -> void?
    ;;   Build and mount the carousel page under root.
    (define (carousel-make-page root)
      (define @index (@ 0))
      (set! carousel-renderer
            (render
             (window
              (vpanel
               (carousel (list (list 0 "alpha" (text "slide-alpha"))
                               (list 1 "beta"  (text "slide-beta"))
                               (list 2 "gamma" (text "slide-gamma")))
                         @index
                         (lambda (next-index)
                           (:= @index next-index)))
               (text (~> @index
                         (lambda (i)
                           (~a "index:" i))))))))
      (mount-renderer! carousel-renderer root)
      (void))

    ;; carousel-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (carousel-run-test _root)
      (and carousel-renderer #t))

    ;; carousel-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (carousel-cleanup _root)
      (when carousel-renderer
        (renderer-destroy carousel-renderer)
        (set! carousel-renderer #f))
      (void))

    (values carousel-make-page carousel-run-test carousel-cleanup)))
