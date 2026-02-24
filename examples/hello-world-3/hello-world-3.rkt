;;;
;;; Hello World 3
;;;
;;; This variant uses SXML to describe the DOM tree and then converts
;;; it with sxml->dom, avoiding direct js-create-element calls.

;; Build a dom node with a small hello message.
(define content
  (sxml->dom
   '(div
      (h1 "Hello, WebRacket!")
      (p "This page was compiled from Racket to WebAssembly."))))


;; Get the (empty) body node and our message.
(define body (js-document-body))
(js-append-child! body content)

;; Use the JavaScript Console for debugging.
(js-log "Hello!")
