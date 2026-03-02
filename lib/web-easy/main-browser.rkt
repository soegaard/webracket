;;;
;;; web-easy Browser Main
;;;

;; Browser entry point that assembles web-easy with the browser backend.

(include/reader "observable.rkt"      read-syntax/skip-first-line)
(include/reader "operator.rkt"        read-syntax/skip-first-line)
(include/reader "view.rkt"            read-syntax/skip-first-line)
(include/reader "backend-browser.rkt" read-syntax/skip-first-line)
(include/reader "renderer.rkt"        read-syntax/skip-first-line)
(include/reader "browser-host.rkt"    read-syntax/skip-first-line)
