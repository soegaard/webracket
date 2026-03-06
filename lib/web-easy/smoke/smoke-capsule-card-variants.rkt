#lang webracket

;;;
;;; Smoke Capsule: Card Variants
;;;

;; Isolated smoke capsule for card variant class rendering in smoke-all.
;;
;; Exports:
;;   card-variants-make-page   Build and mount the card-variants page under root.
;;   card-variants-run-test    Execute capsule-local setup checks.
;;   card-variants-cleanup     Destroy mounted renderer state for this capsule.

(define-values (card-variants-make-page card-variants-run-test card-variants-cleanup)
  (let ()
    ;; Constants for card-variants capsule state.
    (define card-variants-renderer #f) ; Mounted renderer for this capsule.

    ;; card-variants-make-page : any/c -> void?
    ;;   Build and mount the card-variants page under root.
    (define (card-variants-make-page root)
      (set! card-variants-renderer
            (render
             (window
              (vpanel
               (card "Default" #f (text "default"))
               (card "Compact" #f 'compact (text "compact"))
               (card "Flat" #f 'flat (text "flat"))
               (card #f #f 'headerless (text "headerless"))))))
      (mount-renderer! card-variants-renderer root)
      (void))

    ;; card-variants-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (card-variants-run-test _root)
      (and card-variants-renderer #t))

    ;; card-variants-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (card-variants-cleanup _root)
      (when card-variants-renderer
        (renderer-destroy card-variants-renderer)
        (set! card-variants-renderer #f))
      (void))

    (values card-variants-make-page card-variants-run-test card-variants-cleanup)))
