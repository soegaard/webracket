#lang webracket

;;;
;;; Smoke Capsule: Scrollspy
;;;

;; Isolated smoke capsule for scrollspy active-section behavior in smoke-all.
;;
;; Exports:
;;   scrollspy-make-page   Build and mount the scrollspy page under root.
;;   scrollspy-run-test    Execute capsule-local setup checks.
;;   scrollspy-cleanup     Destroy mounted renderer state for this capsule.

(define-values (scrollspy-make-page scrollspy-run-test scrollspy-cleanup)
  (let ()
    ;; Constants for scrollspy capsule state.
    (define scrollspy-renderer #f) ; Mounted renderer for this capsule.

    ;; scrollspy-make-page : any/c -> void?
    ;;   Build and mount the scrollspy page under root.
    (define (scrollspy-make-page root)
      (define @current (@ 'home))
      (define home-view
        (vpanel
         (text "Home section")
         (text "Welcome")
         (text "Overview")
         (text "Getting started")
         (text "Shortcuts")
         (text "Navigation")
         (text "Status")
         (text "Footer")))
      (define docs-view
        (vpanel
         (text "Docs section")
         (text "Install")
         (text "Configuration")
         (text "API reference")
         (text "Examples")
         (text "Troubleshooting")
         (text "FAQ")
         (text "Release notes")))
      (define api-view
        (vpanel
         (text "API section")
         (text "Core")
         (text "Widgets")
         (text "Events")
         (text "State")
         (text "Renderer")
         (text "Backend")
         (text "Limits")))
      (set! scrollspy-renderer
            (render
             (window
              (vpanel
               (scrollspy (list (list 'home "Home" home-view)
                                (list 'docs "Docs" docs-view)
                                (list 'api "API" api-view))
                          @current
                          (lambda (next-id)
                            (:= @current next-id)))
               (text (~> @current
                         (lambda (id)
                           (~a "current:" id))))))))
      (mount-renderer! scrollspy-renderer root)
      (void))

    ;; scrollspy-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (scrollspy-run-test _root)
      (and scrollspy-renderer #t))

    ;; scrollspy-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (scrollspy-cleanup _root)
      (when scrollspy-renderer
        (renderer-destroy scrollspy-renderer)
        (set! scrollspy-renderer #f))
      (void))

    (values scrollspy-make-page scrollspy-run-test scrollspy-cleanup)))
