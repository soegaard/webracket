#lang webracket

;;;
;;; Smoke Capsule: Dropdown Labeled
;;;

;; Isolated smoke capsule for labeled dropdown option decoding in smoke-all.
;;
;; Exports:
;;   dropdown-labeled-make-page   Build and mount the dropdown-labeled page under root.
;;   dropdown-labeled-run-test    Execute capsule-local setup checks.
;;   dropdown-labeled-cleanup     Destroy mounted renderer state for this capsule.

(define-values
  (dropdown-labeled-make-page dropdown-labeled-run-test dropdown-labeled-cleanup)
  (let ()
    ;; Constants for dropdown-labeled capsule state.
    (define dropdown-labeled-renderer #f) ; Mounted renderer for this capsule.

    ;; dropdown-labeled-make-page : any/c -> void?
    ;;   Build and mount the dropdown-labeled page under root.
    (define (dropdown-labeled-make-page root)
      (define @selected (@ 'none))
      (set! dropdown-labeled-renderer
            (render
             (window
              (vpanel
               (dropdown "Priority"
                         '((low "Low") (med "Medium") (high "High"))
                         (lambda (new-id)
                           (:= @selected new-id)))
               (text (~> @selected (lambda (id) (~a "selected:" id))))))))
      (mount-renderer! dropdown-labeled-renderer root)
      (void))

    ;; dropdown-labeled-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (dropdown-labeled-run-test _root)
      (and dropdown-labeled-renderer #t))

    ;; dropdown-labeled-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (dropdown-labeled-cleanup _root)
      (when dropdown-labeled-renderer
        (renderer-destroy dropdown-labeled-renderer)
        (set! dropdown-labeled-renderer #f))
      (void))

    (values dropdown-labeled-make-page
            dropdown-labeled-run-test
            dropdown-labeled-cleanup)))
