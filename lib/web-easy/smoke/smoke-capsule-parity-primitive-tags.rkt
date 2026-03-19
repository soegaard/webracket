#lang webracket

;;;
;;; Smoke Capsule: Parity Primitive Tags
;;;

;; Isolated parity capsule for primitive html-element initial tag rendering.
;;
;; Exports:
;;   parity-primitive-tags-make-page   Build and mount the parity page under root.
;;   parity-primitive-tags-run-test    Execute capsule-local setup checks.
;;   parity-primitive-tags-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-primitive-tags-make-page
                parity-primitive-tags-run-test
                parity-primitive-tags-cleanup)
  (let ()
    ;; Constants for parity primitive-tags capsule state.
    (define parity-primitive-tags-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-primitive-tags-make-page : any/c -> void?
    ;;   Build and mount the parity primitive-tags page under root.
    (define (parity-primitive-tags-make-page root)
      (set! parity-primitive-tags-renderer
            (render
             (window
              (vpanel
               (H1 "Parity Primitive H1" #:id "parity-prim-h1")
               (P "Parity Primitive P" #:id "parity-prim-p")
               (Span "Parity Primitive Span" #:id "parity-prim-span")
               (Img #:src "avatar-icon.png" #:alt "Parity Primitive Img" #:id "parity-prim-img")
               (Div #:id "parity-prim-div"
                    (Span "Parity Div child" #:id "parity-prim-div-child"))))))
      (mount-renderer! parity-primitive-tags-renderer root)
      (void))

    ;; parity-primitive-tags-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-primitive-tags-run-test _root)
      (and parity-primitive-tags-renderer #t))

    ;; parity-primitive-tags-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-primitive-tags-cleanup _root)
      (when parity-primitive-tags-renderer
        (renderer-destroy parity-primitive-tags-renderer)
        (set! parity-primitive-tags-renderer #f))
      (void))

    (values parity-primitive-tags-make-page
            parity-primitive-tags-run-test
            parity-primitive-tags-cleanup)))
