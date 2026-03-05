#lang webracket

;;;
;;; Smoke Capsule: Collapse
;;;

;; Isolated smoke capsule for collapse open/close rendering in smoke-all.
;;
;; Exports:
;;   collapse-make-page   Build and mount the collapse page under root.
;;   collapse-run-test    Execute capsule-local setup checks.
;;   collapse-cleanup     Destroy mounted renderer state for this capsule.

(define-values (collapse-make-page collapse-run-test collapse-cleanup)
  (let ()
    ;; Constants for collapse capsule state.
    (define collapse-renderer #f) ; Mounted renderer for this capsule.

    ;; collapse-make-page : any/c -> void?
    ;;   Build and mount the collapse page under root.
    (define (collapse-make-page root)
      (define @open (@ #f))
      (set! collapse-renderer
            (render
             (window
              (vpanel
               (button "toggle"
                       (lambda ()
                         (obs-update! @open not)))
               (collapse @open
                         (text "secret"))))))
      (mount-renderer! collapse-renderer root)
      (void))

    ;; collapse-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (collapse-run-test _root)
      (and collapse-renderer #t))

    ;; collapse-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (collapse-cleanup _root)
      (when collapse-renderer
        (renderer-destroy collapse-renderer)
        (set! collapse-renderer #f))
      (void))

    (values collapse-make-page collapse-run-test collapse-cleanup)))
