#lang webracket

;;;
;;; Smoke Capsule: Choice Decode
;;;

;; Isolated smoke capsule for choice value decoding edge cases in smoke-all.
;;
;; Exports:
;;   choice-decode-make-page   Build and mount the choice-decode page under root.
;;   choice-decode-run-test    Execute capsule-local setup checks.
;;   choice-decode-cleanup     Destroy mounted renderer state for this capsule.

(define-values (choice-decode-make-page choice-decode-run-test choice-decode-cleanup)
  (let ()
    ;; Constants for choice-decode capsule state.
    (define choice-decode-renderer #f) ; Mounted renderer for this capsule.

    ;; choice-decode-make-page : any/c -> void?
    ;;   Build and mount the choice-decode page under root.
    (define (choice-decode-make-page root)
      (define @selected (@ 'green))
      (set! choice-decode-renderer
            (render
             (window
              (vpanel
               (choice '((red "Red") (green "Green") (42 "Forty Two") (alpha "Alpha"))
                       @selected
                       (lambda (next)
                         (:= @selected next)))
               (text (~> @selected (lambda (v) (~a "selected:" v))))))))
      (mount-renderer! choice-decode-renderer root)
      (void))

    ;; choice-decode-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (choice-decode-run-test _root)
      (and choice-decode-renderer #t))

    ;; choice-decode-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (choice-decode-cleanup _root)
      (when choice-decode-renderer
        (renderer-destroy choice-decode-renderer)
        (set! choice-decode-renderer #f))
      (void))

    (values choice-decode-make-page choice-decode-run-test choice-decode-cleanup)))
