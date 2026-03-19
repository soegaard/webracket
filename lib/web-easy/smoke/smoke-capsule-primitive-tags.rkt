#lang webracket

;;;
;;; Smoke Capsule: Primitive Tags
;;;

;; Isolated smoke capsule for primitive html-element initial tag rendering.
;;
;; Exports:
;;   primitive-tags-make-page   Build and mount the primitive-tags page under root.
;;   primitive-tags-run-test    Execute capsule-local setup checks.
;;   primitive-tags-cleanup     Destroy mounted renderer state for this capsule.

(define-values (primitive-tags-make-page primitive-tags-run-test primitive-tags-cleanup)
  (let ()
    ;; Constants for primitive-tags capsule state.
    (define primitive-tags-renderer #f) ; Mounted renderer for this capsule.

    ;; primitive-tags-make-page : any/c -> void?
    ;;   Build and mount the primitive-tags page under root.
    (define (primitive-tags-make-page root)
      (set! primitive-tags-renderer
            (render
             (window
              (vpanel
               (H1 "Primitive H1" #:id "prim-h1")
               (P "Primitive P" #:id "prim-p")
               (Span "Primitive Span" #:id "prim-span")
               (Img #:src "avatar-icon.png" #:alt "Primitive Img" #:id "prim-img")
               (Div #:id "prim-div"
                    (Span "Div child" #:id "prim-div-child"))))))
      (mount-renderer! primitive-tags-renderer root)
      (void))

    ;; primitive-tags-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (primitive-tags-run-test _root)
      (and primitive-tags-renderer #t))

    ;; primitive-tags-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (primitive-tags-cleanup _root)
      (when primitive-tags-renderer
        (renderer-destroy primitive-tags-renderer)
        (set! primitive-tags-renderer #f))
      (void))

    (values primitive-tags-make-page
            primitive-tags-run-test
            primitive-tags-cleanup)))
