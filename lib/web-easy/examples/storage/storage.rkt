;;;
;;; Storage Demo
;;;

(include-lib web-easy)
(include-lib storage)

(define @key (@ "webracket-demo"))
(define @local-value (@ "123"))
(define @session-value (@ "456"))
(define @status (@ "Ready."))
(define @snapshot (@ "No snapshot yet."))

;; refresh-snapshot! : -> void?
;;   Recompute the storage summary from the current browser state.
(define (refresh-snapshot!)
  (define ls (local-storage))
  (define ss (session-storage))
  (obs-set! @snapshot
            (format "localStorage length: ~a; sessionStorage length: ~a; local value: ~a; session value: ~a"
                    (storage-length ls)
                    (storage-length ss)
                    (storage-get-item ls (obs-peek @key))
                    (storage-get-item ss (obs-peek @key))))
  (void))

;; save-local! : -> void?
;;   Store the current value in localStorage.
(define (save-local!)
  (storage-set-item! (local-storage) (obs-peek @key) (obs-peek @local-value))
  (refresh-snapshot!)
  (obs-set! @status "Saved to localStorage.")
  (void))

;; save-session! : -> void?
;;   Store the current value in sessionStorage.
(define (save-session!)
  (storage-set-item! (session-storage) (obs-peek @key) (obs-peek @session-value))
  (refresh-snapshot!)
  (obs-set! @status "Saved to sessionStorage.")
  (void))

;; clear-storages! : -> void?
;;   Clear both storage areas.
(define (clear-storages!)
  (storage-clear! (local-storage))
  (storage-clear! (session-storage))
  (refresh-snapshot!)
  (obs-set! @status "Cleared both storage areas.")
  (void))

(define storage-demo-app
  (window
   (container
    (vpanel
     (h1 "Storage Demo")
     (text "A small demo for localStorage and sessionStorage.")
     (text "Storage key")
     (input @key
            (lambda (new-value)
              (:= @key new-value)))
     (text "Local value")
     (input @local-value
            (lambda (new-value)
              (:= @local-value new-value)))
     (text "Session value")
     (input @session-value
            (lambda (new-value)
              (:= @session-value new-value)))
     (hpanel
      (button "Save local" save-local!)
      (button "Save session" save-session!)
      (button "Clear both" clear-storages!)
      (button "Refresh" refresh-snapshot!))
     (P @status)
     (P @snapshot)))))

(define app-renderer
  (render storage-demo-app))

(mount-renderer! app-renderer)
