#lang webracket

;;;
;;; Smoke Capsule: Parity Release
;;;

;; Isolated parity capsule for parity-release.
;;
;; Exports:
;;   parity-release-make-page      Build and mount the parity page under root.
;;   parity-release-run-test       Execute capsule-local setup checks.
;;   parity-release-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-release-make-page parity-release-run-test parity-release-cleanup)
  (let ()
    ;; Constants for parity-release capsule state.
    (define parity-release-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-release-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-release-make-page root)
      (struct checklist-item (id done? owner title) #:transparent)

      ;; Constants for owner filter values.
      (define owner/all   "all")   ; No owner filtering.
      (define owner/alice "alice") ; Alice-owned rows.
      (define owner/bob   "bob")   ; Bob-owned rows.

      ;; Constants for initial checklist state.
      (define initial-items
        (list (checklist-item 201 #f owner/alice "docs")
              (checklist-item 202 #f owner/bob   "ci")
              (checklist-item 203 #t owner/alice "release-notes")))

      ;; Constants for example-local observable state.
      (define @items        (@ initial-items))
      (define @owner-filter (@ owner/all))
      (define @status       (@ "ready"))

      ;; item-visible? : checklist-item? string? -> boolean?
      ;;   Return #t when owner filter includes item.
      (define (item-visible? item filter-owner)
        (or (string=? filter-owner owner/all)
            (string=? (checklist-item-owner item) filter-owner)))

      ;; visible-items : -> list?
      ;;   Compute checklist items visible under current owner filter.
      (define (visible-items)
        (define filter-owner (obs-peek @owner-filter))
        (filter (lambda (item)
                  (item-visible? item filter-owner))
                (obs-peek @items)))

      ;; item->row : checklist-item? -> string?
      ;;   Convert checklist item to one summary row string.
      (define (item->row item)
        (~a (checklist-item-id item) ":"
            (checklist-item-owner item) ":"
            (if (checklist-item-done? item) "done" "todo") ":"
            (checklist-item-title item)))

      ;; visible-rows : -> list?
      ;;   Build table rows for visible checklist items.
      (define (visible-rows)
        (map item->row (visible-items)))

      ;; done-count : list? -> number?
      ;;   Count completed checklist items.
      (define (done-count items)
        (length (filter checklist-item-done? items)))

      ;; completion-pct : -> number?
      ;;   Compute completion percentage in range 0..100.
      (define (completion-pct)
        (define items (obs-peek @items))
        (cond
          [(null? items) 0]
          [else
           (inexact->exact
            (floor (* 100 (/ (done-count items) (length items)))))]))

      ;; summary-text : -> string?
      ;;   Build one-line summary for visible/total progress.
      (define (summary-text)
        (define items   (obs-peek @items))
        (define visible (visible-items))
        (~a "visible:" (length visible)
                       ";total:" (length items)
                       ";done:" (done-count items)
                       ";pct:" (completion-pct)))

      ;; update-item : number? (checklist-item? -> checklist-item?) -> void?
      ;;   Apply updater f to item with matching id.
      (define (update-item id f)
        (obs-update! @items
                     (lambda (items)
                       (map (lambda (item)
                              (if (= (checklist-item-id item) id)
                                  (f item)
                                  item))
                            items))))

      ;; toggle-visible! : -> void?
      ;;   Toggle done state of first visible checklist item.
      (define (toggle-visible!)
        (define visible (visible-items))
        (if (null? visible)
            (:= @status "no-visible")
            (let ()
              (define id (checklist-item-id (car visible)))
              (update-item
               id
               (lambda (item)
                 (checklist-item (checklist-item-id item)
                                 (not (checklist-item-done? item))
                                 (checklist-item-owner item)
                                 (checklist-item-title item))))
              (:= @status (~a "toggled:" id)))))

      ;; assign-visible-bob! : -> void?
      ;;   Assign first visible checklist item to bob.
      (define (assign-visible-bob!)
        (define visible (visible-items))
        (if (null? visible)
            (:= @status "no-visible")
            (let ()
              (define id (checklist-item-id (car visible)))
              (update-item
               id
               (lambda (item)
                 (checklist-item (checklist-item-id item)
                                 (checklist-item-done? item)
                                 owner/bob
                                 (checklist-item-title item))))
              (:= @status (~a "assigned-bob:" id)))))

      ;; reset-checklist! : -> void?
      ;;   Restore default checklist items and filter.
      (define (reset-checklist!)
        (:= @items initial-items)
        (:= @owner-filter owner/all)
        (:= @status "reset"))

      (define app-renderer
        (render
         (window
          (vpanel
           (group "Release Checklist"
                  (choice (list owner/all owner/alice owner/bob)
                          @owner-filter
                          (lambda (new-owner)
                            (:= @owner-filter new-owner)))
                  (observable-view @items
                                   (lambda (_)
                                     (observable-view @owner-filter
                                                      (lambda (_)
                                                        (vpanel
                                                         (text (summary-text))
                                                         (progress (completion-pct))
                                                         (table '(checklist) (visible-rows)))))))
                  (text (~> @status
                            (lambda (status)
                              (~a "status:" status)))))
           (menu-bar
            (menu "Actions"
                  (menu-item "Toggle Visible" toggle-visible!)
                  (menu-item "Assign Visible Bob" assign-visible-bob!)
                  (menu-item "Reset Checklist" reset-checklist!)))))))

      (set! parity-release-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-release-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-release-run-test _root)
      (and parity-release-renderer #t))

    ;; parity-release-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-release-cleanup _root)
      (when parity-release-renderer
        (renderer-destroy parity-release-renderer)
        (set! parity-release-renderer #f))
      (void))

    (values parity-release-make-page parity-release-run-test parity-release-cleanup)))
