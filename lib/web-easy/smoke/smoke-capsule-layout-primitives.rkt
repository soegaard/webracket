#lang webracket

;;;
;;; Smoke Capsule: Layout Primitives
;;;

;; Isolated smoke capsule for container/grid/stack/inline/spacer rendering in smoke-all.
;;
;; Exports:
;;   layout-primitives-make-page   Build and mount the layout-primitives page under root.
;;   layout-primitives-run-test    Execute capsule-local setup checks.
;;   layout-primitives-cleanup     Destroy mounted renderer state for this capsule.

(define-values (layout-primitives-make-page layout-primitives-run-test layout-primitives-cleanup)
  (let ()
    ;; Constants for layout-primitives capsule state.
    (define layout-primitives-renderer #f) ; Mounted renderer for this capsule.

    ;; layout-primitives-make-page : any/c -> void?
    ;;   Build and mount the layout-primitives page under root.
    (define (layout-primitives-make-page root)
      (set! layout-primitives-renderer
            (render
             (window
              (container
               (stack
                (inline
                 (text "left")
                 (spacer 2)
                 (text "right"))
                (grid 2
                      (card "A" #f (text "alpha"))
                      (card "B" #f (text "beta"))))))))
      (mount-renderer! layout-primitives-renderer root)
      (void))

    ;; layout-primitives-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (layout-primitives-run-test _root)
      (and layout-primitives-renderer #t))

    ;; layout-primitives-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (layout-primitives-cleanup _root)
      (when layout-primitives-renderer
        (renderer-destroy layout-primitives-renderer)
        (set! layout-primitives-renderer #f))
      (void))

    (values layout-primitives-make-page
            layout-primitives-run-test
            layout-primitives-cleanup)))
