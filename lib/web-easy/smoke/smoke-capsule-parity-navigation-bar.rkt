#lang webracket

;;;
;;; Smoke Capsule: Parity Navigation Bar
;;;

;; Isolated parity capsule for navigation-bar layout and action wiring in parity-all.
;;
;; Exports:
;;   parity-navigation-bar-make-page   Build and mount the parity page under root.
;;   parity-navigation-bar-run-test    Execute capsule-local setup checks.
;;   parity-navigation-bar-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-navigation-bar-make-page
                parity-navigation-bar-run-test
                parity-navigation-bar-cleanup)
  (let ()
    ;; Constants for parity navigation-bar capsule state.
    (define parity-navigation-bar-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-navigation-bar-make-page : any/c -> void?
    ;;   Build and mount the parity navigation-bar page under root.
    (define (parity-navigation-bar-make-page root)
      (define @selected (@ 'overview))
      (set! parity-navigation-bar-renderer
            (render
             (window
              (vpanel
               (navigation-bar
                (button "overview"
                        (lambda ()
                          (:= @selected 'overview)))
                (button "deploys"
                        (lambda ()
                          (:= @selected 'deploys)))
                (button "settings"
                        (lambda ()
                          (:= @selected 'settings))))
               (text (~> @selected
                         (lambda (id)
                           (~a "parity-nav:" id))))))))
      (mount-renderer! parity-navigation-bar-renderer root)
      (void))

    ;; parity-navigation-bar-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-navigation-bar-run-test _root)
      (and parity-navigation-bar-renderer #t))

    ;; parity-navigation-bar-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-navigation-bar-cleanup _root)
      (when parity-navigation-bar-renderer
        (renderer-destroy parity-navigation-bar-renderer)
        (set! parity-navigation-bar-renderer #f))
      (void))

    (values parity-navigation-bar-make-page
            parity-navigation-bar-run-test
            parity-navigation-bar-cleanup)))
