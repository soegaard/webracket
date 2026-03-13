#lang webracket

;;;
;;; Smoke Capsule: Parity Carousel Advanced
;;;

;; Isolated parity capsule for carousel wrap/keyboard/autoplay behavior in parity-all.
;;
;; Exports:
;;   parity-carousel-advanced-make-page   Build and mount the parity page under root.
;;   parity-carousel-advanced-run-test    Execute capsule-local setup checks.
;;   parity-carousel-advanced-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-carousel-advanced-make-page
                parity-carousel-advanced-run-test
                parity-carousel-advanced-cleanup)
  (let ()
    ;; Constants for parity advanced carousel capsule state.
    (define parity-carousel-advanced-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-carousel-advanced-make-page : any/c -> void?
    ;;   Build and mount the parity advanced carousel page under root.
    (define (parity-carousel-advanced-make-page root)
      (define @index (@ 0))
      (define @wrap (@ #t))
      (define @autoplay (@ #f))
      (set! parity-carousel-advanced-renderer
            (render
             (window
              (vpanel
               (hpanel
                (button "set-wrap-on" (lambda () (:= @wrap #t)))
                (button "set-wrap-off" (lambda () (:= @wrap #f)))
                (button "set-autoplay-on" (lambda () (:= @autoplay #t)))
                (button "set-autoplay-off" (lambda () (:= @autoplay #f)))
                (button "set-first" (lambda () (:= @index 0)))
                (button "set-last" (lambda () (:= @index 2))))
               (carousel
                         (list (list 0 "first"  (text "parity-slide-first"))
                               (list 1 "second" (text "parity-slide-second"))
                               (list 2 "third"  (text "parity-slide-third")))
                         @index
                         (lambda (next-index)
                           (:= @index next-index))
                         #:wrap? @wrap
                         #:autoplay? @autoplay)
               (text (~> @index
                         (lambda (i)
                           (~a "parity-index:" i))))
               (text (~> @wrap
                         (lambda (w)
                           (~a "parity-wrap:" (if w "on" "off")))))
               (text (~> @autoplay
                         (lambda (a)
                           (~a "parity-autoplay:" (if a "on" "off")))))))))
      (mount-renderer! parity-carousel-advanced-renderer root)
      (void))

    ;; parity-carousel-advanced-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-carousel-advanced-run-test _root)
      (and parity-carousel-advanced-renderer #t))

    ;; parity-carousel-advanced-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-carousel-advanced-cleanup _root)
      (when parity-carousel-advanced-renderer
        (renderer-destroy parity-carousel-advanced-renderer)
        (set! parity-carousel-advanced-renderer #f))
      (void))

    (values parity-carousel-advanced-make-page
            parity-carousel-advanced-run-test
            parity-carousel-advanced-cleanup)))
