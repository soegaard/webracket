#lang webracket

;;;
;;; Smoke Capsule: Counter
;;;

;; Isolated smoke capsule for the counter test page in the smoke-all driver.
;;
;; Exports:
;;   smoke-make-page   Build and mount the counter page under root.
;;   smoke-run-test    Execute capsule-local setup checks.
;;   smoke-cleanup     Destroy mounted renderer state for this capsule.

(define-values (smoke-make-page smoke-run-test smoke-cleanup)
  (let ()
    ;; Constants for smoke counter capsule state.
    (define smoke-renderer #f) ; Mounted renderer for this capsule.

    ;; smoke-make-page : any/c -> void?
    ;;   Build and mount the counter page under root.
    (define (smoke-make-page root)
      (define @count (@ 0))
      (set! smoke-renderer
        (render
         (window
          (vpanel
           (text (~> @count (lambda (n) (~a "count:" n))))
           (button "inc"
                   (lambda ()
                     (<~ @count add1)))))))
      (mount-renderer! smoke-renderer root)
      (void))

    ;; smoke-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (smoke-run-test _root)
      (and smoke-renderer #t))

    ;; smoke-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (smoke-cleanup _root)
      (when smoke-renderer
        (renderer-destroy smoke-renderer)
        (set! smoke-renderer #f))
      (void))

    (values smoke-make-page smoke-run-test smoke-cleanup)))
