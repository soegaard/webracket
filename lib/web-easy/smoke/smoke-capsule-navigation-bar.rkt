#lang webracket

;;;
;;; Smoke Capsule: Navigation Bar
;;;

;; Isolated smoke capsule for navigation-bar layout and action wiring in smoke-all.
;;
;; Exports:
;;   navigation-bar-make-page   Build and mount the navigation-bar page under root.
;;   navigation-bar-run-test    Execute capsule-local setup checks.
;;   navigation-bar-cleanup     Destroy mounted renderer state for this capsule.

(define-values (navigation-bar-make-page navigation-bar-run-test navigation-bar-cleanup)
  (let ()
    ;; Constants for navigation-bar capsule state.
    (define navigation-bar-renderer #f) ; Mounted renderer for this capsule.

    ;; navigation-bar-make-page : any/c -> void?
    ;;   Build and mount the navigation-bar page under root.
    (define (navigation-bar-make-page root)
      (define @selected (@ 'home))
      (set! navigation-bar-renderer
            (render
             (window
              (vpanel
               (navigation-bar
                (button "home"
                        (lambda ()
                          (:= @selected 'home)))
                (button "docs"
                        (lambda ()
                          (:= @selected 'docs)))
                (button "about"
                        (lambda ()
                          (:= @selected 'about))))
               (text (~> @selected
                         (lambda (id)
                           (~a "nav:" id))))))))
      (mount-renderer! navigation-bar-renderer root)
      (void))

    ;; navigation-bar-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (navigation-bar-run-test _root)
      (and navigation-bar-renderer #t))

    ;; navigation-bar-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (navigation-bar-cleanup _root)
      (when navigation-bar-renderer
        (renderer-destroy navigation-bar-renderer)
        (set! navigation-bar-renderer #f))
      (void))

    (values navigation-bar-make-page navigation-bar-run-test navigation-bar-cleanup)))
