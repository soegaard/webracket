#lang webracket

;;;
;;; Smoke Capsule: Parity Scrollspy
;;;

;; Isolated parity capsule for scrollspy active-section behavior in parity-all.
;;
;; Exports:
;;   parity-scrollspy-make-page   Build and mount the parity page under root.
;;   parity-scrollspy-run-test    Execute capsule-local setup checks.
;;   parity-scrollspy-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-scrollspy-make-page parity-scrollspy-run-test parity-scrollspy-cleanup)
  (let ()
    ;; Constants for parity scrollspy capsule state.
    (define parity-scrollspy-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-scrollspy-make-page : any/c -> void?
    ;;   Build and mount the parity scrollspy page under root.
    (define (parity-scrollspy-make-page root)
      (define @current (@ 'docs))
      (define home-view
        (vpanel
         (text "Parity home")
         (text "Intro")
         (text "Context")
         (text "Checklist")
         (text "Notes")
         (text "Examples")
         (text "Links")
         (text "Summary")))
      (define docs-view
        (vpanel
         (text "Parity docs")
         (text "Start")
         (text "Build")
         (text "Deploy")
         (text "Observe")
         (text "Verify")
         (text "Tune")
         (text "Ship")))
      (define faq-view
        (vpanel
         (text "Parity faq")
         (text "Q1")
         (text "Q2")
         (text "Q3")
         (text "Q4")
         (text "Q5")
         (text "Q6")
         (text "Q7")))
      (set! parity-scrollspy-renderer
            (render
             (window
              (vpanel
               (scrollspy (list (list 'home "Home" home-view)
                                (list 'docs "Docs" docs-view)
                                (list 'faq "FAQ" faq-view))
                          @current
                          (lambda (next-id)
                            (:= @current next-id)))
               (text (~> @current
                         (lambda (id)
                           (~a "parity-current:" id))))))))
      (mount-renderer! parity-scrollspy-renderer root)
      (void))

    ;; parity-scrollspy-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-scrollspy-run-test _root)
      (and parity-scrollspy-renderer #t))

    ;; parity-scrollspy-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-scrollspy-cleanup _root)
      (when parity-scrollspy-renderer
        (renderer-destroy parity-scrollspy-renderer)
        (set! parity-scrollspy-renderer #f))
      (void))

    (values parity-scrollspy-make-page parity-scrollspy-run-test parity-scrollspy-cleanup)))
