#lang webracket

;;;
;;; Smoke Capsule: Operators
;;;

;; Isolated smoke capsule for ~#> and λ<~ behavior in smoke-all.
;;
;; Exports:
;;   operators-make-page   Build and mount the operators page under root.
;;   operators-run-test    Execute capsule-local setup checks.
;;   operators-cleanup     Destroy mounted renderer state for this capsule.

(define-values (operators-make-page operators-run-test operators-cleanup)
  (let ()
    ;; Constants for operators capsule state.
    (define operators-renderer #f) ; Mounted renderer for this capsule.

    ;; operators-make-page : any/c -> void?
    ;;   Build and mount the operators page under root.
    (define (operators-make-page root)
      (define @count (@ 0))
      (define inc! (λ<~ @count add1))
      (define @even (~#> @count even?))
      (set! operators-renderer
        (render
         (window
          (vpanel
           (text (~> @count (lambda (n)
                              (~a "count:" n))))
           (text (~> @even (lambda (n)
                             (~a "even:" n))))
           (button "inc" inc!)))))
      (mount-renderer! operators-renderer root)
      (void))

    ;; operators-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (operators-run-test _root)
      (and operators-renderer #t))

    ;; operators-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (operators-cleanup _root)
      (when operators-renderer
        (renderer-destroy operators-renderer)
        (set! operators-renderer #f))
      (void))

    (values operators-make-page operators-run-test operators-cleanup)))
