#lang webracket

;;;
;;; Smoke Capsule: Parity settings
;;;

;; Isolated parity capsule for parity-settings.
;;
;; Exports:
;;   parity-settings-make-page      Build and mount the parity page under root.
;;   parity-settings-run-test       Execute capsule-local setup checks.
;;   parity-settings-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-settings-make-page parity-settings-run-test parity-settings-cleanup)
  (let ()
    ;; Constants for parity-settings capsule state.
    (define parity-settings-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-settings-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-settings-make-page root)
      ;;;
      ;;; web-easy Browser Parity Settings Example
      ;;;
      
      ;; Parity example: settings/admin panel combining table and menu-bar actions.
      
      
      ;; Constants for initial row values.
      (define row/alice-viewer "Alice:viewer") ; Default Alice role row text.
      (define row/alice-admin  "Alice:admin")  ; Promoted Alice role row text.
      (define row/bob-editor   "Bob:editor")   ; Default Bob role row text.
      (define row/cara-viewer  "Cara:viewer")  ; Added Cara role row text.
      
      ;; Constants for initial settings state.
      (define initial-rows (list row/alice-viewer
                                 row/bob-editor))
      
      ;; Constants for example-local observable state.
      (define @rows   (@ initial-rows))
      (define @status (@ "ready"))
      
      ;; replace-row : list? string? string? -> list?
      ;;   Replace old-row with new-row when present.
      (define (replace-row rows old-row new-row)
        (map (lambda (row)
               (if (string=? row old-row)
                   new-row
                   row))
             rows))
      
      ;; row-present? : list? string? -> boolean?
      ;;   Check whether rows contains row-text.
      (define (row-present? rows row-text)
        (cond
          [(null? rows) #f]
          [(string=? (car rows) row-text) #t]
          [else (row-present? (cdr rows) row-text)]))
      
      ;; promote-alice! : -> void?
      ;;   Promote Alice to admin in table rows.
      (define (promote-alice!)
        (:= @rows (replace-row (obs-peek @rows)
                               row/alice-viewer
                               row/alice-admin))
        (:= @status "promoted"))
      
      ;; add-cara! : -> void?
      ;;   Append Cara row once.
      (define (add-cara!)
        (define rows (obs-peek @rows))
        (if (row-present? rows row/cara-viewer)
            (:= @status "already-added")
            (begin
              (:= @rows (append rows (list row/cara-viewer)))
              (:= @status "added-cara"))))
      
      ;; reset-settings! : -> void?
      ;;   Restore default table rows and status.
      (define (reset-settings!)
        (:= @rows initial-rows)
        (:= @status "reset"))
      
      (define app-renderer
        (render
         (window
          (vpanel
           (group "Settings"
                  (table '(members) @rows)
                  (text (~> @rows
                            (lambda (rows)
                              (string-append "count:" (number->string (length rows))))))
                  (text (~> @status
                            (lambda (status)
                              (string-append "status:" status)))))
           (menu-bar
            (menu "Actions"
                  (menu-item "Promote Alice" promote-alice!)
                  (menu-item "Add Cara" add-cara!)
                  (menu-item "Reset settings" reset-settings!)))))))
      
      (set! parity-settings-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-settings-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-settings-run-test _root)
      (and parity-settings-renderer #t))

    ;; parity-settings-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-settings-cleanup _root)
      (when parity-settings-renderer
        (renderer-destroy parity-settings-renderer)
        (set! parity-settings-renderer #f))
      (void))

    (values parity-settings-make-page parity-settings-run-test parity-settings-cleanup)))
