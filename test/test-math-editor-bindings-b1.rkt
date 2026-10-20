#lang webracket/base

;; B1 browser-binding consumer. This intentionally imports a nested local
;; module and uses public browser libraries only; no console bridge is needed.

(require browser/document
         browser/element
         browser/event
         "math-editor-bindings/b1-listener.rktl")

(define assertion-count 0)

;; check-true : any/c string? -> void?
;;   Record a completed assertion or raise a labeled failure.
(define (check-true value label)
  (unless value
    (error 'test-math-editor-bindings-b1 label))
  (set! assertion-count (add1 assertion-count)))

;; check-equal : any/c any/c string? -> void?
;;   Record a completed equality assertion or raise a labeled failure.
(define (check-equal got expected label)
  (unless (equal? got expected)
    (error 'test-math-editor-bindings-b1 label))
  (set! assertion-count (add1 assertion-count)))

(define mathml-namespace "http://www.w3.org/1998/Math/MathML")
(define body (document-body))

(unless body
  (error 'test-math-editor-bindings-b1 "document.body is unavailable"))

(element-set-attribute! body 'data-math-editor-bindings-b1-state "started")

;; Construct native MathML without an HTML-parsing shortcut.
(define math (document-create-element-ns mathml-namespace 'math))
(define fraction (document-create-element-ns mathml-namespace 'mfrac))
(define numerator (document-create-element-ns mathml-namespace 'mi))
(define denominator (document-create-element-ns mathml-namespace 'mi))
(define root (document-create-element-ns mathml-namespace 'msup))
(define table (document-create-element-ns mathml-namespace 'mtable))
(define text-x (document-create-text-node "𝑥"))
(define text-y (document-create-text-node "y"))

(element-set-attribute! math 'data-binding-probe "mathml")
(append-child! numerator text-x)
(append-child! denominator text-y)
(append-child! fraction numerator)
(append-child! fraction denominator)
(append-child! math fraction)
(append-child! math root)
(append-child! math table)
(append-child! body math)

(check-equal (element-namespace-uri math) mathml-namespace "math namespace")
(check-equal (element-namespace-uri fraction) mathml-namespace "fraction namespace")
(check-equal (element-local-name fraction) "mfrac" "fraction local name")
(check-equal (element-get-attribute math 'data-binding-probe) "mathml" "literal attribute")
(check-equal (dom-node-type text-x) 3 "text node type")
(check-equal (dom-node-value text-x) "𝑥" "literal Unicode text node")
(check-equal (dom-node-text-content numerator) "𝑥" "text-node traversal")
(check-true (dom-node-contains? math denominator) "node containment")
(check-true (dom-node-is-connected? math) "connected MathML root")
(check-equal (node-list-length (dom-node-child-nodes math)) 3 "MathML child count")
(element-set-attribute! body 'data-math-editor-bindings-b1-state "mathml")

;; Build the native text input and test selection in the browser's UTF-16 domain.
(define textarea (document-create-element 'textarea))
(element-set-value! textarea "𝑥á")
(append-child! body textarea)
(element-focus! textarea #t)
(check-equal (element-id (document-active-element))
             (element-id textarea)
             "focused textarea")
(element-set-selection-range! textarea 0 4 'forward)
(check-equal (element-selection-start textarea) 0 "selection start UTF-16 offset")
(check-equal (element-selection-end textarea) 4 "selection end UTF-16 offset")
(check-equal (element-selection-direction textarea) "forward" "selection direction")
(element-set-read-only! textarea #t)
(check-true (element-read-only? textarea) "read-only state")
(element-set-disabled! textarea #f)
(check-true (not (element-disabled? textarea)) "disabled state")
(element-set-attribute! body 'data-math-editor-bindings-b1-state "selection")

;; The listener closure lives in the nested imported module. dispatchEvent's
;; result proves that preventDefault happened before dispatch returned.
(define veto-listener (install-synchronous-veto-listener! textarea))
(check-true (not (element-dispatch-event! textarea
                                          (event-new 'webracket-b1-cancel #t #t)))
            "synchronous cancellation result")
(element-remove-event-listener! textarea 'webracket-b1-cancel veto-listener)
(check-true (element-dispatch-event! textarea
                                     (event-new 'webracket-b1-cancel #t #t))
            "listener removal result")
(element-set-attribute! body 'data-math-editor-bindings-b1-state "events")

;; Detach one caller-owned subtree and retain the other for the independent
;; browser-side receipt below.
(dom-node-remove-child! body textarea)
(check-true (not (dom-node-is-connected? textarea)) "detached textarea subtree")
(check-true (dom-node-is-connected? math) "independent MathML subtree")

(element-set-attribute! body 'data-math-editor-bindings-b1-state "pass")
(element-set-attribute! body 'data-math-editor-bindings-b1-assertions
                        (number->string assertion-count))
