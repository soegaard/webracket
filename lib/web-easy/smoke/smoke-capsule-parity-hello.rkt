#lang webracket

;;;
;;; Smoke Capsule: Parity hello
;;;

;; Isolated parity capsule for parity-hello.
;;
;; Exports:
;;   parity-hello-make-page      Build and mount the parity page under root.
;;   parity-hello-run-test       Execute capsule-local setup checks.
;;   parity-hello-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-hello-make-page parity-hello-run-test parity-hello-cleanup)
  (let ()
    ;; Constants for parity-hello capsule state.
    (define parity-hello-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-hello-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-hello-make-page root)
      ;;;
      ;;; web-easy Browser Parity Hello Example
      ;;;
      
      ;; Parity example: gui-easy quickstart 1.1-style hello world.
      
      
      (define app-renderer
        (render
         (window
          (vpanel
           (text "Hello, World!")))))
      
      (set! parity-hello-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-hello-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-hello-run-test _root)
      (and parity-hello-renderer #t))

    ;; parity-hello-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-hello-cleanup _root)
      (when parity-hello-renderer
        (renderer-destroy parity-hello-renderer)
        (set! parity-hello-renderer #f))
      (void))

    (values parity-hello-make-page parity-hello-run-test parity-hello-cleanup)))
