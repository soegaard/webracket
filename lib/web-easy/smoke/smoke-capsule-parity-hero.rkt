#lang webracket

;;;
;;; Smoke Capsule: Parity Hero
;;;

;; Isolated parity capsule for a realistic hero/overview composition in parity-all.
;;
;; Exports:
;;   parity-hero-make-page   Build and mount the parity page under root.
;;   parity-hero-run-test    Execute capsule-local setup checks.
;;   parity-hero-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-hero-make-page parity-hero-run-test parity-hero-cleanup)
  (let ()
    ;; Constants for parity hero capsule state.
    (define parity-hero-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-hero-make-page : any/c -> void?
    ;;   Build and mount the parity hero page under root.
    (define (parity-hero-make-page root)
      (define @status (@ "ready"))
      (set! parity-hero-renderer
            (render
             (window
              (vpanel
               (heading-with-subtitle 1 "Parity Workspace" "overview")
               (lead "A compact hero section composed only from web-easy primitives.")
               (hpanel
                (button "set-busy"
                        (lambda ()
                          (:= @status "busy")))
                (button "set-ready"
                        (lambda ()
                          (:= @status "ready"))))
               (card "Parity status" #f
                (text (~> @status
                          (lambda (s)
                            (~a "parity-status:" s)))))))))
      (mount-renderer! parity-hero-renderer root)
      (void))

    ;; parity-hero-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-hero-run-test _root)
      (and parity-hero-renderer #t))

    ;; parity-hero-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-hero-cleanup _root)
      (when parity-hero-renderer
        (renderer-destroy parity-hero-renderer)
        (set! parity-hero-renderer #f))
      (void))

    (values parity-hero-make-page parity-hero-run-test parity-hero-cleanup)))
