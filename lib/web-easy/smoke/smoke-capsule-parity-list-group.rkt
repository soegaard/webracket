#lang webracket

;;;
;;; Smoke Capsule: Parity List Group
;;;

;; Isolated parity capsule for list-group selection behavior in parity-all.
;;
;; Exports:
;;   parity-list-group-make-page   Build and mount the parity page under root.
;;   parity-list-group-run-test    Execute capsule-local setup checks.
;;   parity-list-group-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-list-group-make-page parity-list-group-run-test parity-list-group-cleanup)
  (let ()
    ;; Constants for parity list-group capsule state.
    (define parity-list-group-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-list-group-make-page : any/c -> void?
    ;;   Build and mount the parity list-group page under root.
    (define (parity-list-group-make-page root)
      (define @current (@ 'y))
      (set! parity-list-group-renderer
            (render
             (window
              (vpanel
               (list-group '((x "Xray") (y "Yankee") (z "Zulu"))
                           @current
                           (lambda (new-id)
                             (:= @current new-id)))
               (text (~> @current (lambda (id) (~a "parity-current:" id))))))))
      (mount-renderer! parity-list-group-renderer root)
      (void))

    ;; parity-list-group-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-list-group-run-test _root)
      (and parity-list-group-renderer #t))

    ;; parity-list-group-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-list-group-cleanup _root)
      (when parity-list-group-renderer
        (renderer-destroy parity-list-group-renderer)
        (set! parity-list-group-renderer #f))
      (void))

    (values parity-list-group-make-page parity-list-group-run-test parity-list-group-cleanup)))

