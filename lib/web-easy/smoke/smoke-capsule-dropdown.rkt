#lang webracket

;;;
;;; Smoke Capsule: Dropdown
;;;

;; Isolated smoke capsule for dropdown trigger/menu behavior in smoke-all.
;;
;; Exports:
;;   dropdown-make-page   Build and mount the dropdown page under root.
;;   dropdown-run-test    Execute capsule-local setup checks.
;;   dropdown-cleanup     Destroy mounted renderer state for this capsule.

(define-values (dropdown-make-page dropdown-run-test dropdown-cleanup)
  (let ()
    ;; Constants for dropdown capsule state.
    (define dropdown-renderer #f) ; Mounted renderer for this capsule.

    ;; dropdown-make-page : any/c -> void?
    ;;   Build and mount the dropdown page under root.
    (define (dropdown-make-page root)
      (define @selected (@ 'none))
      (set! dropdown-renderer
            (render
             (window
              (vpanel
               (dropdown "Actions"
                         '((open "Open") (save "Save") (close "Close"))
                         (lambda (new-id)
                           (:= @selected new-id)))
               (text (~> @selected (lambda (id) (~a "selected:" id))))))))
      (mount-renderer! dropdown-renderer root)
      (void))

    ;; dropdown-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (dropdown-run-test _root)
      (and dropdown-renderer #t))

    ;; dropdown-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (dropdown-cleanup _root)
      (when dropdown-renderer
        (renderer-destroy dropdown-renderer)
        (set! dropdown-renderer #f))
      (void))

    (values dropdown-make-page dropdown-run-test dropdown-cleanup)))
