#lang webracket

;;;
;;; Smoke Capsule: Parity counters
;;;

;; Isolated parity capsule for parity-counters.
;;
;; Exports:
;;   parity-counters-make-page      Build and mount the parity page under root.
;;   parity-counters-run-test       Execute capsule-local setup checks.
;;   parity-counters-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-counters-make-page parity-counters-run-test parity-counters-cleanup)
  (let ()
    ;; Constants for parity-counters capsule state.
    (define parity-counters-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-counters-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-counters-make-page root)
      ;;;
      ;;; web-easy Browser Parity Counters Example
      ;;;
      
      ;; Parity example: gui-easy quickstart 1.3-style independent counters.
      
      
      ;; Constants for example-local observable state.
      (define @counter-1 (@ 0))
      (define @counter-2 (@ 0))
      
      ;; counter-view : string? observable? -> view?
      ;;   Build one labeled counter row with increment/decrement buttons.
      (define (counter-view label @count)
        (hpanel
         (text (~> @count
                   (lambda (n)
                     (~a label ":" n))))
         (button "+" (lambda ()
                       (<~ @count add1)))
         (button "-" (lambda ()
                       (<~ @count sub1)))))
      
      (define app-renderer
        (render
         (window
          (vpanel
           (counter-view "c1" @counter-1)
           (counter-view "c2" @counter-2)))))
      
      (set! parity-counters-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-counters-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-counters-run-test _root)
      (and parity-counters-renderer #t))

    ;; parity-counters-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-counters-cleanup _root)
      (when parity-counters-renderer
        (renderer-destroy parity-counters-renderer)
        (set! parity-counters-renderer #f))
      (void))

    (values parity-counters-make-page parity-counters-run-test parity-counters-cleanup)))
