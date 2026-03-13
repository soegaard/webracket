#lang webracket

;;;
;;; Smoke Capsule: Table Align
;;;

;; Isolated smoke capsule for table column alignment in smoke-all.
;;
;; Exports:
;;   table-align-make-page   Build and mount the table-align page under root.
;;   table-align-run-test    Execute capsule-local setup checks.
;;   table-align-cleanup     Destroy mounted renderer state for this capsule.

(define-values (table-align-make-page table-align-run-test table-align-cleanup)
  (let ()
    ;; Constants for table-align capsule state.
    (define table-align-renderer #f) ; Mounted renderer for this capsule.

    ;; table-align-make-page : any/c -> void?
    ;;   Build and mount the table-align page under root.
    (define (table-align-make-page root)
      (define @rows (@ '(("alpha" 12 "ok")
                         ("beta" 3 "warning"))))
      (set! table-align-renderer
            (render
             (window
              (vpanel
               (table
                         '(("name" left) ("count" right) ("state" center))
                         @rows
                         #:density 'normal)
               (text (~> @rows (lambda (rows) (~a "rows:" (length rows)))))))))
      (mount-renderer! table-align-renderer root)
      (void))

    ;; table-align-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (table-align-run-test _root)
      (and table-align-renderer #t))

    ;; table-align-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (table-align-cleanup _root)
      (when table-align-renderer
        (renderer-destroy table-align-renderer)
        (set! table-align-renderer #f))
      (void))

    (values table-align-make-page table-align-run-test table-align-cleanup)))
