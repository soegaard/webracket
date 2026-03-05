#lang webracket

;;;
;;; Smoke Capsule: Parity Pagination
;;;

;; Isolated parity capsule for pagination control behavior in parity-all.
;;
;; Exports:
;;   parity-pagination-make-page   Build and mount the parity page under root.
;;   parity-pagination-run-test    Execute capsule-local setup checks.
;;   parity-pagination-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-pagination-make-page parity-pagination-run-test parity-pagination-cleanup)
  (let ()
    ;; Constants for parity pagination capsule state.
    (define parity-pagination-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-pagination-make-page : any/c -> void?
    ;;   Build and mount the parity pagination page under root.
    (define (parity-pagination-make-page root)
      (define @current-page (@ 3))
      (set! parity-pagination-renderer
            (render
             (window
              (vpanel
               (pagination 5 @current-page
                           (lambda (new-page)
                             (:= @current-page new-page)))
               (text (~> @current-page
                         (lambda (n)
                           (~a "parity-page:" n))))))))
      (mount-renderer! parity-pagination-renderer root)
      (void))

    ;; parity-pagination-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-pagination-run-test _root)
      (and parity-pagination-renderer #t))

    ;; parity-pagination-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-pagination-cleanup _root)
      (when parity-pagination-renderer
        (renderer-destroy parity-pagination-renderer)
        (set! parity-pagination-renderer #f))
      (void))

    (values parity-pagination-make-page parity-pagination-run-test parity-pagination-cleanup)))

