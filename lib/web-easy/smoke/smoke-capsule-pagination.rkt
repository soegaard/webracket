#lang webracket

;;;
;;; Smoke Capsule: Pagination
;;;

;; Isolated smoke capsule for pagination control behavior in smoke-all.
;;
;; Exports:
;;   pagination-make-page   Build and mount the pagination page under root.
;;   pagination-run-test    Execute capsule-local setup checks.
;;   pagination-cleanup     Destroy mounted renderer state for this capsule.

(define-values (pagination-make-page pagination-run-test pagination-cleanup)
  (let ()
    ;; Constants for pagination capsule state.
    (define pagination-renderer #f) ; Mounted renderer for this capsule.

    ;; pagination-make-page : any/c -> void?
    ;;   Build and mount the pagination page under root.
    (define (pagination-make-page root)
      (define @current-page (@ 2))
      (set! pagination-renderer
            (render
             (window
              (vpanel
               (pagination 4 @current-page
                           (lambda (new-page)
                             (:= @current-page new-page)))
               (text (~> @current-page
                         (lambda (n)
                           (~a "page:" n))))))))
      (mount-renderer! pagination-renderer root)
      (void))

    ;; pagination-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (pagination-run-test _root)
      (and pagination-renderer #t))

    ;; pagination-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (pagination-cleanup _root)
      (when pagination-renderer
        (renderer-destroy pagination-renderer)
        (set! pagination-renderer #f))
      (void))

    (values pagination-make-page pagination-run-test pagination-cleanup)))
