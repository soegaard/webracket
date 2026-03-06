#lang webracket

;;;
;;; Smoke Capsule: Button Toolbar
;;;

;; Isolated smoke capsule for button-toolbar group actions in smoke-all.
;;
;; Exports:
;;   button-toolbar-make-page   Build and mount the button-toolbar page under root.
;;   button-toolbar-run-test    Execute capsule-local setup checks.
;;   button-toolbar-cleanup     Destroy mounted renderer state for this capsule.

(define-values (button-toolbar-make-page button-toolbar-run-test button-toolbar-cleanup)
  (let ()
    ;; Constants for button-toolbar capsule state.
    (define button-toolbar-renderer #f) ; Mounted renderer for this capsule.

    ;; button-toolbar-make-page : any/c -> void?
    ;;   Build and mount the button-toolbar page under root.
    (define (button-toolbar-make-page root)
      (define @left  (@ 0))
      (define @right (@ 10))
      (set! button-toolbar-renderer
            (render
             (window
              (vpanel
               (button-toolbar
                (button-group
                 (button "L+" (lambda () (:= @left (+ (obs-peek @left) 1)))))
                (button-group
                 (button "R-" (lambda () (:= @right (- (obs-peek @right) 1))))))
               (text (~> @left (lambda (l) (~a "L:" l))))
               (text (~> @right (lambda (r) (~a "R:" r))))))))
      (mount-renderer! button-toolbar-renderer root)
      (void))

    ;; button-toolbar-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (button-toolbar-run-test _root)
      (and button-toolbar-renderer #t))

    ;; button-toolbar-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (button-toolbar-cleanup _root)
      (when button-toolbar-renderer
        (renderer-destroy button-toolbar-renderer)
        (set! button-toolbar-renderer #f))
      (void))

    (values button-toolbar-make-page button-toolbar-run-test button-toolbar-cleanup)))
