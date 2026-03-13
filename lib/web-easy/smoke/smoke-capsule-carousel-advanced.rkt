#lang webracket

;;;
;;; Smoke Capsule: Carousel Advanced
;;;

;; Isolated smoke capsule for carousel wrap/keyboard/autoplay behavior in smoke-all.
;;
;; Exports:
;;   carousel-advanced-make-page   Build and mount the advanced carousel page under root.
;;   carousel-advanced-run-test    Execute capsule-local setup checks.
;;   carousel-advanced-cleanup     Destroy mounted renderer state for this capsule.

(define-values (carousel-advanced-make-page
                carousel-advanced-run-test
                carousel-advanced-cleanup)
  (let ()
    ;; Constants for advanced carousel capsule state.
    (define carousel-advanced-renderer #f) ; Mounted renderer for this capsule.

    ;; carousel-advanced-make-page : any/c -> void?
    ;;   Build and mount the advanced carousel page under root.
    (define (carousel-advanced-make-page root)
      (define @index (@ 0))
      (define @wrap (@ #t))
      (define @autoplay (@ #f))
      (set! carousel-advanced-renderer
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
                         (list (list 0 "alpha" (text "slide-alpha"))
                               (list 1 "beta"  (text "slide-beta"))
                               (list 2 "gamma" (text "slide-gamma")))
                         @index
                         (lambda (next-index)
                           (:= @index next-index))
                         #:wrap? @wrap
                         #:autoplay? @autoplay)
               (text (~> @index
                         (lambda (i)
                           (~a "index:" i))))
               (text (~> @wrap
                         (lambda (w)
                           (~a "wrap:" (if w "on" "off")))))
               (text (~> @autoplay
                         (lambda (a)
                           (~a "autoplay:" (if a "on" "off")))))))))
      (mount-renderer! carousel-advanced-renderer root)
      (void))

    ;; carousel-advanced-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (carousel-advanced-run-test _root)
      (and carousel-advanced-renderer #t))

    ;; carousel-advanced-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (carousel-advanced-cleanup _root)
      (when carousel-advanced-renderer
        (renderer-destroy carousel-advanced-renderer)
        (set! carousel-advanced-renderer #f))
      (void))

    (values carousel-advanced-make-page
            carousel-advanced-run-test
            carousel-advanced-cleanup)))
