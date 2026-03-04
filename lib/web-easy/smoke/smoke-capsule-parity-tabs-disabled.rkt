#lang webracket

;;;
;;; Smoke Capsule: Parity tabs disabled
;;;

;; Isolated parity capsule for parity-tabs-disabled.
;;
;; Exports:
;;   parity-tabs-disabled-make-page      Build and mount the parity page under root.
;;   parity-tabs-disabled-run-test       Execute capsule-local setup checks.
;;   parity-tabs-disabled-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-tabs-disabled-make-page
                parity-tabs-disabled-run-test
                parity-tabs-disabled-cleanup)
  (let ()
    ;; Constants for parity-tabs-disabled capsule state.
    (define parity-tabs-disabled-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-tabs-disabled-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-tabs-disabled-make-page root)
      (define @tab (@ 'left))

      (set! parity-tabs-disabled-renderer
        (render
         (window
          (vpanel
           (tab-panel @tab
                      (list (list 'left (text "Left parity tab") #f)
                            (list 'middle (text "Middle parity tab") #t)
                            (list 'right (text "Right parity tab") #f)))))))
      (mount-renderer! parity-tabs-disabled-renderer root)
      (void))

    ;; parity-tabs-disabled-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-tabs-disabled-run-test _root)
      (and parity-tabs-disabled-renderer #t))

    ;; parity-tabs-disabled-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-tabs-disabled-cleanup _root)
      (when parity-tabs-disabled-renderer
        (renderer-destroy parity-tabs-disabled-renderer)
        (set! parity-tabs-disabled-renderer #f))
      (void))

    (values parity-tabs-disabled-make-page
            parity-tabs-disabled-run-test
            parity-tabs-disabled-cleanup)))
