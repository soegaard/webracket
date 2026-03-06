#lang webracket

;;;
;;; Smoke Capsule: List Group
;;;

;; Isolated smoke capsule for list-group selection behavior in smoke-all.
;;
;; Exports:
;;   list-group-make-page   Build and mount the list-group page under root.
;;   list-group-run-test    Execute capsule-local setup checks.
;;   list-group-cleanup     Destroy mounted renderer state for this capsule.

(define-values (list-group-make-page list-group-run-test list-group-cleanup)
  (let ()
    ;; Constants for list-group capsule state.
    (define list-group-renderer #f) ; Mounted renderer for this capsule.

    ;; list-group-make-page : any/c -> void?
    ;;   Build and mount the list-group page under root.
    (define (list-group-make-page root)
      (define @current (@ 'b))
      (set! list-group-renderer
            (render
             (window
              (vpanel
               (list-group '((a "Alpha") (b "Beta") (c "Gamma"))
                           @current
                           (lambda (new-id)
                             (:= @current new-id)))
               (text (~> @current (lambda (id) (~a "current:" id))))))))
      (mount-renderer! list-group-renderer root)
      (void))

    ;; list-group-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (list-group-run-test _root)
      (and list-group-renderer #t))

    ;; list-group-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (list-group-cleanup _root)
      (when list-group-renderer
        (renderer-destroy list-group-renderer)
        (set! list-group-renderer #f))
      (void))

    (values list-group-make-page list-group-run-test list-group-cleanup)))
