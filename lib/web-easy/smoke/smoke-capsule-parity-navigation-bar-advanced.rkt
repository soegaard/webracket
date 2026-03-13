#lang webracket

;;;
;;; Smoke Capsule: Parity Navigation Bar Advanced
;;;

;; Isolated parity capsule for navigation-bar orientation/collapse behavior in parity-all.
;;
;; Exports:
;;   parity-navigation-bar-advanced-make-page   Build and mount the parity page under root.
;;   parity-navigation-bar-advanced-run-test    Execute capsule-local setup checks.
;;   parity-navigation-bar-advanced-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-navigation-bar-advanced-make-page
                parity-navigation-bar-advanced-run-test
                parity-navigation-bar-advanced-cleanup)
  (let ()
    ;; Constants for parity advanced navigation-bar capsule state.
    (define parity-navigation-bar-advanced-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-navigation-bar-advanced-make-page : any/c -> void?
    ;;   Build and mount the parity advanced navigation-bar page under root.
    (define (parity-navigation-bar-advanced-make-page root)
      (define @selected (@ 'overview))
      (define @orientation (@ 'vertical))
      (define @collapsed (@ #t))
      (set! parity-navigation-bar-advanced-renderer
            (render
             (window
              (vpanel
               (button "toggle-orientation"
                       (lambda ()
                         (:= @orientation
                             (case (obs-peek @orientation)
                               [(vertical)   'horizontal]
                               [else         'vertical]))))
               (navigation-bar
                         (button "overview"
                                 (lambda ()
                                   (:= @selected 'overview)))
                         (button "deploys"
                                 (lambda ()
                                   (:= @selected 'deploys)))
                         (button "settings"
                                 (lambda ()
                                   (:= @selected 'settings)))
                         #:orientation @orientation
                         #:collapsed? @collapsed
                         #:expand 'always)
               (text (~> @selected
                         (lambda (id)
                           (~a "parity-nav-adv:" id))))
               (text (~> @orientation
                         (lambda (o)
                           (~a "parity-orientation:" o))))
               (text (~> @collapsed
                         (lambda (c)
                           (~a "parity-collapsed:" (if c "yes" "no")))))))))
      (mount-renderer! parity-navigation-bar-advanced-renderer root)
      (void))

    ;; parity-navigation-bar-advanced-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-navigation-bar-advanced-run-test _root)
      (and parity-navigation-bar-advanced-renderer #t))

    ;; parity-navigation-bar-advanced-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-navigation-bar-advanced-cleanup _root)
      (when parity-navigation-bar-advanced-renderer
        (renderer-destroy parity-navigation-bar-advanced-renderer)
        (set! parity-navigation-bar-advanced-renderer #f))
      (void))

    (values parity-navigation-bar-advanced-make-page
            parity-navigation-bar-advanced-run-test
            parity-navigation-bar-advanced-cleanup)))
