#lang webracket

;;;
;;; Smoke Capsule: Group
;;;

;; Isolated smoke capsule for group/legend rendering in smoke-all.
;;
;; Exports:
;;   group-make-page   Build and mount the group page under root.
;;   group-run-test    Execute capsule-local setup checks.
;;   group-cleanup     Destroy mounted renderer state for this capsule.

(define-values (group-make-page group-run-test group-cleanup)
  (let ()
    ;; Constants for group capsule state.
    (define group-renderer #f) ; Mounted renderer for this capsule.

    ;; group-make-page : any/c -> void?
    ;;   Build and mount the group page under root.
    (define (group-make-page root)
      (define @title (@ "Visual Check"))
      (define @body (@ "inside"))
      (set! group-renderer
        (render
         (window
          (vpanel
           (group @title
                  (text @body))))))
      (mount-renderer! group-renderer root)
      (void))

    ;; group-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (group-run-test _root)
      (and group-renderer #t))

    ;; group-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (group-cleanup _root)
      (when group-renderer
        (renderer-destroy group-renderer)
        (set! group-renderer #f))
      (void))

    (values group-make-page group-run-test group-cleanup)))
