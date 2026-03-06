#lang webracket

;;;
;;; Smoke Capsule: Button Icons
;;;

;; Isolated smoke capsule for button icon slot rendering in smoke-all.
;;
;; Exports:
;;   button-icons-make-page   Build and mount the button-icons page under root.
;;   button-icons-run-test    Execute capsule-local setup checks.
;;   button-icons-cleanup     Destroy mounted renderer state for this capsule.

(define-values (button-icons-make-page button-icons-run-test button-icons-cleanup)
  (let ()
    ;; Constants for button-icons capsule state.
    (define button-icons-renderer #f) ; Mounted renderer for this capsule.

    ;; button-icons-make-page : any/c -> void?
    ;;   Build and mount the button-icons page under root.
    (define (button-icons-make-page root)
      (define @count (@ 0))
      (define @plain (@ 0))
      (set! button-icons-renderer
            (render
             (window
              (vpanel
               (button "plain"
                       (lambda () (<~ @plain add1)))
               (button "apply"
                       (lambda () (<~ @count add1))
                       "✓"
                       "→")
               (text (~> @count (lambda (n) (~a "count:" n))))
               (text (~> @plain (lambda (n) (~a "plain:" n))))))))
      (mount-renderer! button-icons-renderer root)
      (void))

    ;; button-icons-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (button-icons-run-test _root)
      (and button-icons-renderer #t))

    ;; button-icons-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (button-icons-cleanup _root)
      (when button-icons-renderer
        (renderer-destroy button-icons-renderer)
        (set! button-icons-renderer #f))
      (void))

    (values button-icons-make-page button-icons-run-test button-icons-cleanup)))
