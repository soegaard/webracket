#lang webracket

;;;
;;; Smoke Capsule: Parity Choice Decode
;;;

;; Isolated parity capsule for choice value decoding edge cases in parity-all.
;;
;; Exports:
;;   parity-choice-decode-make-page   Build and mount parity choice-decode page under root.
;;   parity-choice-decode-run-test    Execute capsule-local setup checks.
;;   parity-choice-decode-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-choice-decode-make-page
                parity-choice-decode-run-test
                parity-choice-decode-cleanup)
  (let ()
    ;; Constants for parity choice-decode capsule state.
    (define parity-choice-decode-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-choice-decode-make-page : any/c -> void?
    ;;   Build and mount parity choice-decode page under root.
    (define (parity-choice-decode-make-page root)
      (define @selected (@ 'beta))
      (set! parity-choice-decode-renderer
            (render
             (window
              (vpanel
               (choice '((alpha "Alpha") (beta "Beta") (7 "Seven") (omega "Omega"))
                       @selected
                       (lambda (next)
                         (:= @selected next)))
               (text (~> @selected (lambda (v) (~a "parity-selected:" v))))))))
      (mount-renderer! parity-choice-decode-renderer root)
      (void))

    ;; parity-choice-decode-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-choice-decode-run-test _root)
      (and parity-choice-decode-renderer #t))

    ;; parity-choice-decode-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-choice-decode-cleanup _root)
      (when parity-choice-decode-renderer
        (renderer-destroy parity-choice-decode-renderer)
        (set! parity-choice-decode-renderer #f))
      (void))

    (values parity-choice-decode-make-page
            parity-choice-decode-run-test
            parity-choice-decode-cleanup)))
