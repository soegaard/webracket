#lang webracket

;;;
;;; Smoke Capsule: Parity Dropdown
;;;

;; Isolated parity capsule for dropdown trigger/menu behavior in parity-all.
;;
;; Exports:
;;   parity-dropdown-make-page   Build and mount the parity page under root.
;;   parity-dropdown-run-test    Execute capsule-local setup checks.
;;   parity-dropdown-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-dropdown-make-page parity-dropdown-run-test parity-dropdown-cleanup)
  (let ()
    ;; Constants for parity dropdown capsule state.
    (define parity-dropdown-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-dropdown-make-page : any/c -> void?
    ;;   Build and mount the parity dropdown page under root.
    (define (parity-dropdown-make-page root)
      (define @selected (@ 'none))
      (set! parity-dropdown-renderer
            (render
             (window
              (vpanel
               (dropdown "Project"
                         '((build "Build")
                           (deploy "Deploy")
                           (clean "Clean"))
                         (lambda (new-id)
                           (:= @selected new-id)))
               (text (~> @selected (lambda (id) (~a "parity-selected:" id))))))))
      (mount-renderer! parity-dropdown-renderer root)
      (void))

    ;; parity-dropdown-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-dropdown-run-test _root)
      (and parity-dropdown-renderer #t))

    ;; parity-dropdown-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-dropdown-cleanup _root)
      (when parity-dropdown-renderer
        (renderer-destroy parity-dropdown-renderer)
        (set! parity-dropdown-renderer #f))
      (void))

    (values parity-dropdown-make-page parity-dropdown-run-test parity-dropdown-cleanup)))
