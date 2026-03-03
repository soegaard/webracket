#lang webracket

;;;
;;; Smoke Capsule: Destroy
;;;

;; Isolated smoke capsule for the renderer destroy lifecycle test in smoke-all.
;;
;; Exports:
;;   destroy-make-page   Build and mount the destroy page under root.
;;   destroy-run-test    Execute capsule-local setup checks.
;;   destroy-cleanup     Destroy mounted renderer state for this capsule.

(define-values (destroy-make-page destroy-run-test destroy-cleanup)
  (let ()
    ;; Constants for destroy capsule state.
    (define destroy-renderer #f) ; Mounted renderer for this capsule.

    ;; destroy-make-page : any/c -> void?
    ;;   Build and mount the destroy page under root.
    (define (destroy-make-page root)
      (define @count (@ 0))
      (set! destroy-renderer
        (render
         (window
          (vpanel
           (text (~> @count (lambda (n) (string-append "count:" (number->string n)))))
           (button "inc"
                   (lambda ()
                     (<~ @count add1)))
           (button "destroy"
                   (lambda ()
                     (renderer-destroy destroy-renderer)))))))
      (mount-renderer! destroy-renderer root)
      (void))

    ;; destroy-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (destroy-run-test _root)
      (and destroy-renderer #t))

    ;; destroy-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (destroy-cleanup _root)
      (when destroy-renderer
        (renderer-destroy destroy-renderer)
        (set! destroy-renderer #f))
      (void))

    (values destroy-make-page destroy-run-test destroy-cleanup)))
