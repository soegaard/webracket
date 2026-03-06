#lang webracket

;;;
;;; Smoke Capsule: Button Group
;;;

;; Isolated smoke capsule for grouped button layout and actions in smoke-all.
;;
;; Exports:
;;   button-group-make-page   Build and mount the button-group page under root.
;;   button-group-run-test    Execute capsule-local setup checks.
;;   button-group-cleanup     Destroy mounted renderer state for this capsule.

(define-values (button-group-make-page button-group-run-test button-group-cleanup)
  (let ()
    ;; Constants for button-group capsule state.
    (define button-group-renderer #f) ; Mounted renderer for this capsule.

    ;; button-group-make-page : any/c -> void?
    ;;   Build and mount the button-group page under root.
    (define (button-group-make-page root)
      (define @count (@ 0))
      (set! button-group-renderer
            (render
             (window
              (vpanel
               (button-group
                (button "-" (lambda () (:= @count (- (obs-peek @count) 1))))
                (button "+" (lambda () (:= @count (+ (obs-peek @count) 1)))))
               (text (~> @count (lambda (n) (~a "count:" n))))))))
      (mount-renderer! button-group-renderer root)
      (void))

    ;; button-group-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (button-group-run-test _root)
      (and button-group-renderer #t))

    ;; button-group-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (button-group-cleanup _root)
      (when button-group-renderer
        (renderer-destroy button-group-renderer)
        (set! button-group-renderer #f))
      (void))

    (values button-group-make-page button-group-run-test button-group-cleanup)))
