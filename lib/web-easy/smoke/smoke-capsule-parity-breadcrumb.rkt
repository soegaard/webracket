#lang webracket

;;;
;;; Smoke Capsule: Parity Breadcrumb
;;;

;; Isolated parity capsule for breadcrumb navigation behavior in parity-all.
;;
;; Exports:
;;   parity-breadcrumb-make-page   Build and mount the parity page under root.
;;   parity-breadcrumb-run-test    Execute capsule-local setup checks.
;;   parity-breadcrumb-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-breadcrumb-make-page parity-breadcrumb-run-test parity-breadcrumb-cleanup)
  (let ()
    ;; Constants for parity breadcrumb capsule state.
    (define parity-breadcrumb-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-breadcrumb-make-page : any/c -> void?
    ;;   Build and mount the parity breadcrumb page under root.
    (define (parity-breadcrumb-make-page root)
      (define @current (@ 'api))
      (set! parity-breadcrumb-renderer
            (render
             (window
              (vpanel
               (breadcrumb '((home "Home") (docs "Docs") (api "API"))
                           @current
                           (lambda (new-id)
                             (:= @current new-id)))
               (text (~> @current (lambda (id) (~a "parity-current:" id))))))))
      (mount-renderer! parity-breadcrumb-renderer root)
      (void))

    ;; parity-breadcrumb-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-breadcrumb-run-test _root)
      (and parity-breadcrumb-renderer #t))

    ;; parity-breadcrumb-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-breadcrumb-cleanup _root)
      (when parity-breadcrumb-renderer
        (renderer-destroy parity-breadcrumb-renderer)
        (set! parity-breadcrumb-renderer #f))
      (void))

    (values parity-breadcrumb-make-page parity-breadcrumb-run-test parity-breadcrumb-cleanup)))
