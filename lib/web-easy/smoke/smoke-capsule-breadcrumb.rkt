#lang webracket

;;;
;;; Smoke Capsule: Breadcrumb
;;;

;; Isolated smoke capsule for breadcrumb navigation behavior in smoke-all.
;;
;; Exports:
;;   breadcrumb-make-page   Build and mount the breadcrumb page under root.
;;   breadcrumb-run-test    Execute capsule-local setup checks.
;;   breadcrumb-cleanup     Destroy mounted renderer state for this capsule.

(define-values (breadcrumb-make-page breadcrumb-run-test breadcrumb-cleanup)
  (let ()
    ;; Constants for breadcrumb capsule state.
    (define breadcrumb-renderer #f) ; Mounted renderer for this capsule.

    ;; breadcrumb-make-page : any/c -> void?
    ;;   Build and mount the breadcrumb page under root.
    (define (breadcrumb-make-page root)
      (define @current (@ 'docs))
      (set! breadcrumb-renderer
            (render
             (window
              (vpanel
               (breadcrumb '((home "Home") (docs "Docs") (api "API"))
                           @current
                           (lambda (new-id)
                             (:= @current new-id)))
               (text (~> @current (lambda (id) (~a "current:" id))))))))
      (mount-renderer! breadcrumb-renderer root)
      (void))

    ;; breadcrumb-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (breadcrumb-run-test _root)
      (and breadcrumb-renderer #t))

    ;; breadcrumb-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (breadcrumb-cleanup _root)
      (when breadcrumb-renderer
        (renderer-destroy breadcrumb-renderer)
        (set! breadcrumb-renderer #f))
      (void))

    (values breadcrumb-make-page breadcrumb-run-test breadcrumb-cleanup)))
