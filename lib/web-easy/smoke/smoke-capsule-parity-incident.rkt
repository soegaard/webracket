#lang webracket

;;;
;;; Smoke Capsule: Parity Incident
;;;

;; Isolated parity capsule for parity-incident.
;;
;; Exports:
;;   parity-incident-make-page      Build and mount the parity page under root.
;;   parity-incident-run-test       Execute capsule-local setup checks.
;;   parity-incident-cleanup        Destroy mounted renderer state for this capsule.

(define-values (parity-incident-make-page parity-incident-run-test parity-incident-cleanup)
  (let ()
    ;; Constants for parity-incident capsule state.
    (define parity-incident-renderer #f) ; Mounted renderer for this capsule.

    ;; parity-incident-make-page : any/c -> void?
    ;;   Build and mount the parity page under root.
    (define (parity-incident-make-page root)
      ;; Constants for priority values.
      (define priority/all    "all")    ; No priority filtering.
      (define priority/high   "high")   ; High-priority incidents.
      (define priority/medium "medium") ; Medium-priority incidents.
      (define priority/low    "low")    ; Low-priority incidents.

      (struct incident (id priority state owner title) #:transparent)

      ;; Constants for initial incident values.
      (define initial-incidents
        (list (incident 101 priority/high   "open" "unassigned" "DB outage")
              (incident 102 priority/medium "open" "unassigned" "Login latency")
              (incident 103 priority/low    "open" "alice"      "UI typo")))

      ;; Constants for example-local observable state.
      (define @incidents (@ initial-incidents))
      (define @query     (@ ""))
      (define @priority  (@ priority/all))
      (define @status    (@ "ready"))

      ;; string-contains? : string? string? -> boolean?
      ;;   Return #t when needle appears in haystack.
      (define (string-contains? haystack needle)
        (define n-haystack (string-length haystack))
        (define n-needle   (string-length needle))
        (cond
          [(= n-needle 0) #t]
          [(> n-needle n-haystack) #f]
          [else
           (let loop ([i 0])
             (cond
               [(> i (- n-haystack n-needle)) #f]
               [(string=? (substring haystack i (+ i n-needle)) needle) #t]
               [else (loop (add1 i))]))]))

      ;; incident->row : incident? -> string?
      ;;   Convert incident record to one row summary string.
      (define (incident->row item)
        (string-append
         (number->string (incident-id item)) ":"
         (incident-priority item) ":"
         (incident-state item) ":"
         (incident-owner item) ":"
         (incident-title item)))

      ;; incident-matches? : incident? string? string? -> boolean?
      ;;   Return #t when incident passes priority and query filters.
      (define (incident-matches? item priority query)
        (and (or (string=? priority priority/all)
                 (string=? (incident-priority item) priority))
             (string-contains? (incident-title item) query)))

      ;; visible-incidents : -> list?
      ;;   Compute incidents visible under current filter/query state.
      (define (visible-incidents)
        (define priority (obs-peek @priority))
        (define query    (obs-peek @query))
        (filter (lambda (item)
                  (incident-matches? item priority query))
                (obs-peek @incidents)))

      ;; visible-rows : -> list?
      ;;   Compute table rows for visible incidents.
      (define (visible-rows)
        (map incident->row (visible-incidents)))

      ;; resolved-count : list? -> number?
      ;;   Count incidents with resolved state.
      (define (resolved-count items)
        (length (filter (lambda (item)
                          (string=? (incident-state item) "resolved"))
                        items)))

      ;; summary-text : -> string?
      ;;   Build aggregate summary text for visible and total incidents.
      (define (summary-text)
        (define all-items     (obs-peek @incidents))
        (define visible-items (visible-incidents))
        (string-append "visible:" (number->string (length visible-items))
                       ";total:" (number->string (length all-items))
                       ";resolved:" (number->string (resolved-count all-items))))

      ;; update-incident : number? (incident? -> incident?) -> void?
      ;;   Update incident matching id by applying f.
      (define (update-incident id f)
        (obs-update! @incidents
                     (lambda (items)
                       (map (lambda (item)
                              (if (= (incident-id item) id)
                                  (f item)
                                  item))
                            items))))

      ;; assign-visible! : -> void?
      ;;   Assign first visible incident to "me".
      (define (assign-visible!)
        (define visible (visible-incidents))
        (if (null? visible)
            (:= @status "no-visible")
            (let ()
              (define id (incident-id (car visible)))
              (update-incident
               id
               (lambda (item)
                 (incident (incident-id item)
                           (incident-priority item)
                           (incident-state item)
                           "me"
                           (incident-title item))))
              (:= @status (string-append "assigned:" (number->string id))))))

      ;; resolve-visible! : -> void?
      ;;   Resolve first visible incident.
      (define (resolve-visible!)
        (define visible (visible-incidents))
        (if (null? visible)
            (:= @status "no-visible")
            (let ()
              (define id (incident-id (car visible)))
              (update-incident
               id
               (lambda (item)
                 (incident (incident-id item)
                           (incident-priority item)
                           "resolved"
                           (incident-owner item)
                           (incident-title item))))
              (:= @status (string-append "resolved:" (number->string id))))))

      ;; reset-triage! : -> void?
      ;;   Restore initial incidents and clear filters.
      (define (reset-triage!)
        (:= @incidents initial-incidents)
        (:= @query "")
        (:= @priority priority/all)
        (:= @status "reset"))

      (define app-renderer
        (render
         (window
          (vpanel
           (group "Incident Triage"
                  (hpanel
                   (text "query:")
                   (input @query
                          (lambda (new-value)
                            (:= @query new-value)))
                   (choice (list priority/all priority/high priority/medium priority/low)
                           @priority
                           (lambda (new-value)
                             (:= @priority new-value))))
                  (observable-view @incidents
                                   (lambda (_)
                                     (observable-view @query
                                                      (lambda (_)
                                                        (observable-view @priority
                                                                         (lambda (_)
                                                                           (vpanel
                                                                            (text (summary-text))
                                                                            (table '(incidents) (visible-rows)))))))))
                  (text (~> @status
                            (lambda (status)
                              (string-append "status:" status)))))
           (menu-bar
            (menu "Actions"
                  (menu-item "Assign Visible" assign-visible!)
                  (menu-item "Resolve Visible" resolve-visible!)
                  (menu-item "Reset Triage" reset-triage!)))))))

      (set! parity-incident-renderer app-renderer)
      (mount-renderer! app-renderer root)
      (void))

    ;; parity-incident-run-test : any/c -> boolean?
    ;;   Return #t when capsule setup completed.
    (define (parity-incident-run-test _root)
      (and parity-incident-renderer #t))

    ;; parity-incident-cleanup : any/c -> void?
    ;;   Destroy the mounted renderer for this capsule.
    (define (parity-incident-cleanup _root)
      (when parity-incident-renderer
        (renderer-destroy parity-incident-renderer)
        (set! parity-incident-renderer #f))
      (void))

    (values parity-incident-make-page parity-incident-run-test parity-incident-cleanup)))
