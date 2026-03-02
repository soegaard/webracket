;;;
;;; web-easy Browser Parity Hello Example
;;;

;; Parity example: gui-easy quickstart 1.1-style hello world.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define app-renderer
  (render
   (window
    (vpanel
     (text "Hello, World!")))))

(mount-renderer! app-renderer)
