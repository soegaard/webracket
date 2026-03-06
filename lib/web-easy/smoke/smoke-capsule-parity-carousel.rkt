#lang webracket

;;;
;;; Smoke Capsule: Parity Carousel
;;;

;; Isolated parity capsule for carousel index navigation in parity-all.
;;
;; Exports:
;;   parity-carousel-make-page   Build and mount the parity page under root.
;;   parity-carousel-run-test    Execute capsule-local setup checks.
;;   parity-carousel-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-carousel-make-page parity-carousel-run-test parity-carousel-cleanup)
  (let ()
    ;; Constants for parity carousel capsule state.
    (define parity-carousel-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-carousel-make-page : any/c -> void?
    ;;   Build and mount the parity carousel page under root.
    (define (parity-carousel-make-page root)
      (define @index (@ 1))
      (set! parity-carousel-renderer
            (render
             (window
              (vpanel
               (carousel (list (list 0 "first"  (text "parity-slide-first"))
                               (list 1 "second" (text "parity-slide-second"))
                               (list 2 "third"  (text "parity-slide-third")))
                         @index
                         (lambda (next-index)
                           (:= @index next-index)))
               (text (~> @index
                         (lambda (i)
                           (~a "parity-index:" i))))))))
      (mount-renderer! parity-carousel-renderer root)
      (void))

    ;; parity-carousel-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-carousel-run-test _root)
      (and parity-carousel-renderer #t))

    ;; parity-carousel-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-carousel-cleanup _root)
      (when parity-carousel-renderer
        (renderer-destroy parity-carousel-renderer)
        (set! parity-carousel-renderer #f))
      (void))

    (values parity-carousel-make-page parity-carousel-run-test parity-carousel-cleanup)))
