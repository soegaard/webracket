#lang webracket

;;;
;;; Smoke Capsule: Hero
;;;

;; Isolated smoke capsule for a realistic hero/overview composition in smoke-all.
;;
;; Exports:
;;   hero-make-page   Build and mount the hero page under root.
;;   hero-run-test    Execute capsule-local setup checks.
;;   hero-cleanup     Destroy mounted renderer state for this capsule.

(define-values (hero-make-page hero-run-test hero-cleanup)
  (let ()
    ;; Constants for hero capsule state.
    (define hero-renderer #f) ; Mounted renderer for this capsule.

    ;; hero-make-page : any/c -> void?
    ;;   Build and mount the hero page under root.
    (define (hero-make-page root)
      (define @status (@ "All systems nominal"))
      (set! hero-renderer
            (render
             (window
              (vpanel
               (heading-with-subtitle 1 "Operations Center" "live status")
               (lead "Monitor deployments, incidents, and release readiness from one view.")
               (hpanel
                (button "mark-warning"
                        (lambda ()
                          (:= @status "Investigating elevated danger rate")))
                (button "mark-ok"
                        (lambda ()
                          (:= @status "All systems nominal"))))
               (card "Status" #f
                (text (~> @status
                          (lambda (s)
                            (~a "status:" s)))))))))
      (mount-renderer! hero-renderer root)
      (void))

    ;; hero-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (hero-run-test _root)
      (and hero-renderer #t))

    ;; hero-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (hero-cleanup _root)
      (when hero-renderer
        (renderer-destroy hero-renderer)
        (set! hero-renderer #f))
      (void))

    (values hero-make-page hero-run-test hero-cleanup)))
