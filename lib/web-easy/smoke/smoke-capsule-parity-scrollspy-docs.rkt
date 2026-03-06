#lang webracket

;;;
;;; Smoke Capsule: Parity Scrollspy Docs
;;;

;; Parity docs-page TOC style example for scrollspy.
;;
;; Exports:
;;   parity-scrollspy-docs-make-page   Build and mount parity docs TOC page under root.
;;   parity-scrollspy-docs-run-test    Execute capsule-local setup checks.
;;   parity-scrollspy-docs-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-scrollspy-docs-make-page
                parity-scrollspy-docs-run-test
                parity-scrollspy-docs-cleanup)
  (let ()
    ;; Constants for parity docs scrollspy capsule state.
    (define parity-scrollspy-docs-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-scrollspy-docs-make-page : any/c -> void?
    ;;   Build and mount parity docs TOC page under root.
    (define (parity-scrollspy-docs-make-page root)
      (define @current (@ 'overview))
      (define overview-view
        (vpanel
         (text "Overview")
         (text "Project scope")
         (text "Audience")
         (text "Architecture summary")
         (text "Compatibility notes")
         (text "Support matrix")))
      (define quickstart-view
        (vpanel
         (text "Quickstart")
         (text "Bootstrap project")
         (text "Create first window")
         (text "Add observables")
         (text "Render and mount")
         (text "Run smoke checks")))
      (define migration-view
        (vpanel
         (text "Migration")
         (text "Port existing gui-easy apps")
         (text "Replace classes with structs")
         (text "Adopt include/reader layout")
         (text "Update tests for WebRacket")
         (text "Re-check browser behavior")))
      (define theming-view
        (vpanel
         (text "Theming")
         (text "Token-based theming")
         (text "Stable data-we-widget hooks")
         (text "External CSS override flow")
         (text "Light/dark example")
         (text "Contract checks")))
      (set! parity-scrollspy-docs-renderer
            (render
             (window
              (vpanel
               (text "Parity Docs TOC (real-world example)")
               (scrollspy (list (list 'overview "Overview" overview-view)
                                (list 'quickstart "Quickstart" quickstart-view)
                                (list 'migration "Migration" migration-view)
                                (list 'theming "Theming" theming-view))
                          @current
                          (lambda (next-id)
                            (:= @current next-id)))
               (text (~> @current
                         (lambda (id)
                           (~a "parity-docs-current:" id))))))))
      (mount-renderer! parity-scrollspy-docs-renderer root)
      (void))

    ;; parity-scrollspy-docs-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-scrollspy-docs-run-test _root)
      (and parity-scrollspy-docs-renderer #t))

    ;; parity-scrollspy-docs-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-scrollspy-docs-cleanup _root)
      (when parity-scrollspy-docs-renderer
        (renderer-destroy parity-scrollspy-docs-renderer)
        (set! parity-scrollspy-docs-renderer #f))
      (void))

    (values parity-scrollspy-docs-make-page
            parity-scrollspy-docs-run-test
            parity-scrollspy-docs-cleanup)))
