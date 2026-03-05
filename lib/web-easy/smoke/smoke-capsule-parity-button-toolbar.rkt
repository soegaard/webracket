#lang webracket

;;;
;;; Smoke Capsule: Parity Button Toolbar
;;;

;; Isolated parity capsule for button-toolbar group actions in parity-all.
;;
;; Exports:
;;   parity-button-toolbar-make-page   Build and mount the parity page under root.
;;   parity-button-toolbar-run-test    Execute capsule-local setup checks.
;;   parity-button-toolbar-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-button-toolbar-make-page
                parity-button-toolbar-run-test
                parity-button-toolbar-cleanup)
  (let ()
    ;; Constants for parity button-toolbar capsule state.
    (define parity-button-toolbar-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-button-toolbar-make-page : any/c -> void?
    ;;   Build and mount the parity button-toolbar page under root.
    (define (parity-button-toolbar-make-page root)
      (define @left  (@ 2))
      (define @right (@ 20))
      (set! parity-button-toolbar-renderer
            (render
             (window
              (vpanel
               (button-toolbar
                (button-group
                 (button "L+" (lambda () (:= @left (+ (obs-peek @left) 1)))))
                (button-group
                 (button "R-" (lambda () (:= @right (- (obs-peek @right) 1))))))
               (text (~> @left (lambda (l) (~a "parity-L:" l))))
               (text (~> @right (lambda (r) (~a "parity-R:" r))))))))
      (mount-renderer! parity-button-toolbar-renderer root)
      (void))

    ;; parity-button-toolbar-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-button-toolbar-run-test _root)
      (and parity-button-toolbar-renderer #t))

    ;; parity-button-toolbar-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-button-toolbar-cleanup _root)
      (when parity-button-toolbar-renderer
        (renderer-destroy parity-button-toolbar-renderer)
        (set! parity-button-toolbar-renderer #f))
      (void))

    (values parity-button-toolbar-make-page
            parity-button-toolbar-run-test
            parity-button-toolbar-cleanup)))

