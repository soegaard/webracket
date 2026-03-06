#lang webracket

;;;
;;; Smoke Capsule: Navigation Bar Advanced
;;;

;; Isolated smoke capsule for navigation-bar orientation/collapse behavior in smoke-all.
;;
;; Exports:
;;   navigation-bar-advanced-make-page   Build and mount the page under root.
;;   navigation-bar-advanced-run-test    Execute capsule-local setup checks.
;;   navigation-bar-advanced-cleanup     Destroy mounted renderer state for this capsule.

(define-values (navigation-bar-advanced-make-page
                navigation-bar-advanced-run-test
                navigation-bar-advanced-cleanup)
  (let ()
    ;; Constants for advanced navigation-bar capsule state.
    (define navigation-bar-advanced-renderer #f) ; Mounted renderer for this capsule.

    ;; navigation-bar-advanced-make-page : any/c -> void?
    ;;   Build and mount the advanced navigation-bar page under root.
    (define (navigation-bar-advanced-make-page root)
      (define @selected (@ 'home))
      (define @orientation (@ 'vertical))
      (define @collapsed (@ #t))
      (set! navigation-bar-advanced-renderer
            (render
             (window
              (vpanel
               (button "toggle-orientation"
                       (lambda ()
                         (:= @orientation
                             (case (obs-peek @orientation)
                               [(vertical)   'horizontal]
                               [else         'vertical]))))
               (navigation-bar @orientation @collapsed 'always
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
                           (~a "nav-adv:" id))))
               (text (~> @orientation
                         (lambda (o)
                           (~a "orientation:" o))))
               (text (~> @collapsed
                         (lambda (c)
                           (~a "collapsed:" (if c "yes" "no")))))))))
      (mount-renderer! navigation-bar-advanced-renderer root)
      (void))

    ;; navigation-bar-advanced-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (navigation-bar-advanced-run-test _root)
      (and navigation-bar-advanced-renderer #t))

    ;; navigation-bar-advanced-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (navigation-bar-advanced-cleanup _root)
      (when navigation-bar-advanced-renderer
        (renderer-destroy navigation-bar-advanced-renderer)
        (set! navigation-bar-advanced-renderer #f))
      (void))

    (values navigation-bar-advanced-make-page
            navigation-bar-advanced-run-test
            navigation-bar-advanced-cleanup)))
