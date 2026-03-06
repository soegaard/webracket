#lang webracket

;;;
;;; Smoke Capsule: Scrollspy Docs
;;;

;; Real-world docs-page TOC style example for scrollspy.
;;
;; Exports:
;;   scrollspy-docs-make-page   Build and mount docs TOC page under root.
;;   scrollspy-docs-run-test    Execute capsule-local setup checks.
;;   scrollspy-docs-cleanup     Destroy mounted renderer state for this capsule.

(define-values (scrollspy-docs-make-page scrollspy-docs-run-test scrollspy-docs-cleanup)
  (let ()
    ;; Constants for docs scrollspy capsule state.
    (define scrollspy-docs-renderer #f) ; Mounted renderer for this capsule.

    ;; scrollspy-docs-make-page : any/c -> void?
    ;;   Build and mount docs TOC page under root.
    (define (scrollspy-docs-make-page root)
      (define @current (@ 'intro))
      (define intro-view
        (vpanel
         (text "Introduction")
         (text "What web-easy is")
         (text "When to use it")
         (text "Mental model")
         (text "Limitations")
         (text "Glossary")))
      (define install-view
        (vpanel
         (text "Installation")
         (text "1. Install Racket")
         (text "2. Link webracket")
         (text "3. Verify compiler")
         (text "4. Run smoke checks")
         (text "5. Open test dashboard")))
      (define widgets-view
        (vpanel
         (text "Widgets")
         (text "Layout: vpanel/hpanel/group")
         (text "Inputs: input/choice/slider")
         (text "Nav: tabs/menu/scrollspy")
         (text "Feedback: alert/toast/spinner")
         (text "Data: table/list-view")))
      (define contracts-view
        (vpanel
         (text "Contracts")
         (text "a11y contract")
         (text "keyboard contract")
         (text "style-hook contract")
         (text "theme-token contract")
         (text "parity contract suite")))
      (set! scrollspy-docs-renderer
            (render
             (window
              (vpanel
               (text "Docs TOC (real-world example)")
               (scrollspy (list (list 'intro "Intro" intro-view)
                                (list 'install "Install" install-view)
                                (list 'widgets "Widgets" widgets-view)
                                (list 'contracts "Contracts" contracts-view))
                          @current
                          (lambda (next-id)
                            (:= @current next-id)))
               (text (~> @current
                         (lambda (id)
                           (~a "docs-current:" id))))))))
      (mount-renderer! scrollspy-docs-renderer root)
      (void))

    ;; scrollspy-docs-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (scrollspy-docs-run-test _root)
      (and scrollspy-docs-renderer #t))

    ;; scrollspy-docs-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (scrollspy-docs-cleanup _root)
      (when scrollspy-docs-renderer
        (renderer-destroy scrollspy-docs-renderer)
        (set! scrollspy-docs-renderer #f))
      (void))

    (values scrollspy-docs-make-page scrollspy-docs-run-test scrollspy-docs-cleanup)))
