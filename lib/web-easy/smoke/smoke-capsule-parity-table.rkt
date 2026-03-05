#lang webracket

;;;
;;; Smoke Capsule: Parity table
;;;

;; Isolated parity capsule for parity-table.
;;
;; Exports:
;;   parity-table-make-page      Build and mount the parity page under root.
;;   parity-table-run-test       Execute capsule-local setup checks.
;;   parity-table-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-table-make-page parity-table-run-test parity-table-cleanup)
  (let ()
    ;; Constants for parity-table capsule state.
    (define parity-table-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-table-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-table-make-page root)
      ;;;
      ;;; web-easy Browser Parity Table Example
      ;;;
      
      ;; Parity example: multi-column table with menu actions and explicit cell layout.
      
      
      ;; Constants for initial row values.
      (define row/alice (list "alice" "admin" "active")) ; Alice row tuple.
      (define row/bob   (list "bob" "editor" "idle")) ; Bob row tuple.
      (define row/cara  (list "cara" "viewer" "pending")) ; Cara row tuple.
      
      ;; Constants for initial example state.
      (define initial-rows (list row/alice
                                 row/bob))
      
      ;; Constants for example-local observable state.
      (define @rows   (@ initial-rows))
      (define @status (@ "ready"))
      
      ;; row-name : list? -> string?
      ;;   Return name column from a table row.
      (define (row-name row)
        (list-ref row 0))
      
      ;; row-role : list? -> string?
      ;;   Return role column from a table row.
      (define (row-role row)
        (list-ref row 1))
      
      ;; row-state : list? -> string?
      ;;   Return status/state column from a table row.
      (define (row-state row)
        (list-ref row 2))
      
      ;; with-row-state : list? string? -> list?
      ;;   Create row with updated state/status value.
      (define (with-row-state row new-state)
        (list (row-name row)
              (row-role row)
              new-state))
      
      ;; update-bob-state! : string? -> void?
      ;;   Update Bob status field in rows.
      (define (update-bob-state! new-state)
        (:= @rows
            (map (lambda (row)
                   (if (string=? (row-name row) "bob")
                       (with-row-state row new-state)
                       row))
                 (obs-peek @rows))))
      
      ;; row-present? : list? string? -> boolean?
      ;;   Check whether any row has the given name.
      (define (row-present? rows target-name)
        (cond
          [(null? rows) #f]
          [(string=? (row-name (car rows)) target-name) #t]
          [else (row-present? (cdr rows) target-name)]))
      
      ;; activate-bob! : -> void?
      ;;   Set Bob status from idle to active.
      (define (activate-bob!)
        (update-bob-state! "active")
        (:= @status "bob-activated"))
      
      ;; add-cara! : -> void?
      ;;   Add Cara row once.
      (define (add-cara!)
        (define rows (obs-peek @rows))
        (if (row-present? rows "cara")
            (:= @status "cara-already-added")
            (begin
              (:= @rows (append rows (list row/cara)))
              (:= @status "cara-added"))))
      
      ;; reset-table! : -> void?
      ;;   Restore initial rows and status.
      (define (reset-table!)
        (:= @rows initial-rows)
        (:= @status "reset"))
      
      (define app-renderer
        (render
         (window
          (vpanel
           (group "Team"
                  (table '(name role status) @rows 'compact)
                  (text (~> @rows
                            (lambda (rows)
                              (~a "rows:" (length rows))))
                  )
                  (text (~> @status
                            (lambda (status)
                              (~a "status:" status)))))
           (menu-bar
            (menu "Actions"
                  (menu-item "Activate Bob" activate-bob!)
                  (menu-item "Add Cara" add-cara!)
                  (menu-item "Reset table" reset-table!)))))))
      
      (set! parity-table-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-table-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-table-run-test _root)
      (and parity-table-renderer #t))

    ;; parity-table-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-table-cleanup _root)
      (when parity-table-renderer
        (renderer-destroy parity-table-renderer)
        (set! parity-table-renderer #f))
      (void))

    (values parity-table-make-page parity-table-run-test parity-table-cleanup)))
