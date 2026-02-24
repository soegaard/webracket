;;;
;;; Hello World
;;;

(define body (js-document-body))
(define h1   (js-create-element "h1"))
(define p    (js-create-element "p"))
(define h1-text (js-create-text-node "Hello, WebRacket!"))
(define p-text  (js-create-text-node "This page was compiled from Racket to WebAssembly."))

(js-append-child! h1 h1-text)
(js-append-child! p p-text)
(js-append-child! body h1)
(js-append-child! body p)
