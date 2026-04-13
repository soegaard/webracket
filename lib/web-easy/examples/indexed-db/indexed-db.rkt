;;;
;;; IndexedDB Demo
;;;

(include-lib web-easy)
(include-lib indexed-db)

(define @db-name (@ "webracket-demo-db"))
(define @status (@ "Ready."))
(define @request-summary (@ "No IndexedDB request yet."))

;; update-request-summary! : string? -> void?
;;   Record the most recent request description.
(define (update-request-summary! label)
  (obs-set! @request-summary label)
  (void))

;; open-db! : -> void?
;;   Issue an IndexedDB open request.
(define (open-db!)
  (define db (indexed-db))
  (define req (indexed-db-open db (obs-peek @db-name)))
  (update-request-summary! (format "Open request returned a ~a" (js-typeof req)))
  (obs-set! @status "Opened database request.")
  (void))

;; delete-db! : -> void?
;;   Issue an IndexedDB delete request.
(define (delete-db!)
  (define db (indexed-db))
  (define req (indexed-db-delete-database! db (obs-peek @db-name)))
  (update-request-summary! (format "Delete request returned a ~a" (js-typeof req)))
  (obs-set! @status "Issued database deletion request.")
  (void))

;; inspect-db! : -> void?
;;   Check the wrapped factory and show its status.
(define (inspect-db!)
  (define db (indexed-db))
  (update-request-summary!
   (format "Factory present? ~a; wrapped? ~a"
           (string=? (js-typeof (indexed-db-raw db)) "object")
           (indexed-db? db)))
  (obs-set! @status "Inspected IndexedDB factory.")
  (void))

(define indexed-db-demo-app
  (window
   (container
    (vpanel
     (h1 "IndexedDB Demo")
     (text "A small demo for the IndexedDB factory wrapper.")
     (text "Database name")
     (input @db-name
            (lambda (new-value)
              (:= @db-name new-value)))
     (hpanel
      (button "Open database" open-db!)
      (button "Delete database" delete-db!)
      (button "Inspect factory" inspect-db!))
     (P @status)
     (P @request-summary)))))

(define app-renderer
  (render indexed-db-demo-app))

(mount-renderer! app-renderer)
