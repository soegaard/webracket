#lang webracket

;;;
;;; Smoke Capsule: Parity counter
;;;

;; Isolated parity capsule for parity-counter.
;;
;; Exports:
;;   parity-counter-make-page      Build and mount the parity page under root.
;;   parity-counter-run-test       Execute capsule-local setup checks.
;;   parity-counter-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-counter-make-page parity-counter-run-test parity-counter-cleanup)
  (let ()
    ;; Constants for parity-counter capsule state.
    (define parity-counter-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-counter-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-counter-make-page root)
      ;;;
      ;;; web-easy Browser Parity Counter Example
      ;;;
      
      ;; Parity example: gui-easy quickstart 1.2-style counter.
      
      
      (define @count (@ 0))
      
      (define app-renderer
        (render
         (window
          (vpanel
           (text (~> @count (lambda (n) (~a n))))
           (button "+" (lambda ()
                          (<~ @count add1)))))))
      
      (set! parity-counter-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-counter-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-counter-run-test _root)
      (and parity-counter-renderer #t))

    ;; parity-counter-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-counter-cleanup _root)
      (when parity-counter-renderer
        (renderer-destroy parity-counter-renderer)
        (set! parity-counter-renderer #f))
      (void))

    (values parity-counter-make-page parity-counter-run-test parity-counter-cleanup)))
