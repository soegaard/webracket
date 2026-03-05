#lang webracket

;;;
;;; Smoke Capsule: Accordion
;;;

;; Isolated smoke capsule for accordion section toggling in smoke-all.
;;
;; Exports:
;;   accordion-make-page   Build and mount the accordion page under root.
;;   accordion-run-test    Execute capsule-local setup checks.
;;   accordion-cleanup     Destroy mounted renderer state for this capsule.

(define-values (accordion-make-page accordion-run-test accordion-cleanup)
  (let ()
    ;; Constants for accordion capsule state.
    (define accordion-renderer #f) ; Mounted renderer for this capsule.

    ;; accordion-make-page : any/c -> void?
    ;;   Build and mount the accordion page under root.
    (define (accordion-make-page root)
      (define @selected (@ 'overview))
      (set! accordion-renderer
            (render
             (window
              (vpanel
               (accordion @selected
                          (list (list 'overview "Overview" (text "overview-body"))
                                (list 'details "Details" (text "details-body"))))
               (text (~> @selected
                         (lambda (id)
                           (~a "selected:" id))))))))
      (mount-renderer! accordion-renderer root)
      (void))

    ;; accordion-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (accordion-run-test _root)
      (and accordion-renderer #t))

    ;; accordion-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (accordion-cleanup _root)
      (when accordion-renderer
        (renderer-destroy accordion-renderer)
        (set! accordion-renderer #f))
      (void))

    (values accordion-make-page accordion-run-test accordion-cleanup)))
