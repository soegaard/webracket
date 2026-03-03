#lang webracket

;;;
;;; Smoke Capsule: Input
;;;

;; Isolated smoke capsule for the input test page in the smoke-all driver.
;;
;; Exports:
;;   input-make-page   Build and mount the input page under root.
;;   input-run-test    Execute capsule-local setup checks.
;;   input-cleanup     Destroy mounted renderer state for this capsule.

(define-values (input-make-page input-run-test input-cleanup)
  (let ()
    ;; Constants for input capsule state.
    (define input-renderer #f) ; Mounted renderer for this capsule.

    ;; input-make-page : any/c -> void?
    ;;   Build and mount the input page under root.
    (define (input-make-page root)
      (define @name (@ "alice"))
      (set! input-renderer
        (render
         (window
          (vpanel
           (input @name
                  (lambda (new-value)
                    (:= @name new-value)))
           (text @name)))))
      (mount-renderer! input-renderer root)
      (void))

    ;; input-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (input-run-test _root)
      (and input-renderer #t))

    ;; input-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (input-cleanup _root)
      (when input-renderer
        (renderer-destroy input-renderer)
        (set! input-renderer #f))
      (void))

    (values input-make-page input-run-test input-cleanup)))
