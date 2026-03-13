#lang webracket

;;;
;;; Smoke Capsule: Parity Table Align
;;;

;; Isolated parity capsule for table column alignment in parity-all.
;;
;; Exports:
;;   parity-table-align-make-page   Build and mount parity table-align page under root.
;;   parity-table-align-run-test    Execute capsule-local setup checks.
;;   parity-table-align-cleanup     Destroy mounted renderer state for this capsule.

(define-values (parity-table-align-make-page
                parity-table-align-run-test
                parity-table-align-cleanup)
  (let ()
    ;; Constants for parity table-align capsule state.
    (define parity-table-align-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-table-align-make-page : any/c -> void?
    ;;   Build and mount parity table-align page under root.
    (define (parity-table-align-make-page root)
      (define @rows (@ '(("alpha" 21 "ok")
                         ("gamma" 1 "hold"))))
      (set! parity-table-align-renderer
            (render
             (window
              (vpanel
               (table
                         '(("name" left) ("count" right) ("state" center))
                         @rows
                         #:density 'normal)
               (text (~> @rows (lambda (rows) (~a "parity-rows:" (length rows)))))))))
      (mount-renderer! parity-table-align-renderer root)
      (void))

    ;; parity-table-align-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-table-align-run-test _root)
      (and parity-table-align-renderer #t))

    ;; parity-table-align-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-table-align-cleanup _root)
      (when parity-table-align-renderer
        (renderer-destroy parity-table-align-renderer)
        (set! parity-table-align-renderer #f))
      (void))

    (values parity-table-align-make-page
            parity-table-align-run-test
            parity-table-align-cleanup)))
