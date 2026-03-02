;;;
;;; web-easy Browser Tab-Panel Smoke Example
;;;

;; Minimal browser app for tab-panel branch switching smoke tests.

(include/reader "../main-browser.rkt" read-syntax/skip-first-line)

(define @tab (@ 'info))

(define app-renderer
  (render
   (window
    (vpanel
     (tab-panel @tab
                (list (cons 'info (text "Info tab"))
                      (cons 'settings (text "Settings tab"))
                      (cons 'about (text "About tab"))))))))

(mount-renderer! app-renderer)
