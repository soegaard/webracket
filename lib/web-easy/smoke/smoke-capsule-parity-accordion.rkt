#lang webracket

;;;
;;; Smoke Capsule: Parity Accordion
;;;

;; Isolated parity capsule for accordion section toggling in parity-all.
;;
;; Exports:
;;   parity-accordion-make-page   Build and mount the parity page under root.
;;   parity-accordion-run-test    Execute capsule-local setup checks.
;;   parity-accordion-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-accordion-make-page parity-accordion-run-test parity-accordion-cleanup)
  (let ()
    ;; Constants for parity accordion capsule state.
    (define parity-accordion-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-accordion-make-page : any/c -> void?
    ;;   Build and mount the parity accordion page under root.
    (define (parity-accordion-make-page root)
      (define @selected (@ 'details))
      (set! parity-accordion-renderer
            (render
             (window
              (vpanel
               (accordion @selected
                          (list (list 'overview "Overview" (text "parity-overview-body"))
                                (list 'details "Details" (text "parity-details-body"))))
               (text (~> @selected
                         (lambda (id)
                           (~a "parity-selected:" id))))))))
      (mount-renderer! parity-accordion-renderer root)
      (void))

    ;; parity-accordion-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-accordion-run-test _root)
      (and parity-accordion-renderer #t))

    ;; parity-accordion-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-accordion-cleanup _root)
      (when parity-accordion-renderer
        (renderer-destroy parity-accordion-renderer)
        (set! parity-accordion-renderer #f))
      (void))

    (values parity-accordion-make-page parity-accordion-run-test parity-accordion-cleanup)))
