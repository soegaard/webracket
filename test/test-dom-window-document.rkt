;;;
;;; dom.ffi
;;;

;; Focused tests for the Window, Document, and Element wrapper libraries.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/dom.ffi -r test-dom-window-document.rkt

(include-lib window)
(include-lib document)
(include-lib element)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal label)))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(define (check-false got label)
  (when got
    (error 'check-false label)))

(define (expect-contract-error thunk)
  (with-handlers ([exn:fail:contract? (lambda (_e) #t)])
    (thunk)
    #f))

(define (install!)
  (js-eval
   "window.name = 'dom-suite';
    window.confirm = function (message) { window.lastConfirm = message; return 1; };
    window.open = function (url, target, features, replace) {
      window.lastOpen = [url, target, features, replace];
      return { kind: 'popup', url, target, features, replace };
    };
    window.setTimeout = function (callback, delay) {
      window.lastTimeout = [callback, delay];
      callback();
      return 17;
    };
    window.__domTest = window.__domTest || {};
    document.body.innerHTML = '';
    const host = document.createElement('div');
    host.id = 'dom-host';
    host.setAttribute('data-state', 'ready');
    const leaf = document.createElement('span');
    leaf.className = 'leaf';
    leaf.textContent = 'leaf';
    host.appendChild(leaf);
    document.body.appendChild(host);
    window.__domTest.host = host;
    window.__domTest.leaf = leaf;"))

(define (dom-test-fixture name)
  (js-ref (js-var "__domTest") name))

(list
 (list "Window wrappers"
       (let ()
         (install!)
         (define timeout-fired 0)
         (check-equal (window-name) "dom-suite" "window name")
         (window-set-name! "web-bracket")
         (check-equal (window-name) "web-bracket" "window set name")
         (check-true (window-confirm "proceed?") "window confirm")
         (check-equal (js-ref (js-var "window") "lastConfirm") "proceed?" "window confirm arg")
         (define opened (window-open "https://example.invalid" "tab" "noopener" #f))
         (check-true (external? opened) "window open result")
         (check-equal (js-ref opened "kind") "popup" "window open payload")
         (check-equal (window-set-timeout/delay (procedure->external (lambda () (set! timeout-fired (add1 timeout-fired))))
                                                10)
                      17
                      "window timeout handle")
         (check-equal timeout-fired 1 "window timeout callback")
         (check-equal (vector-ref (js-ref (js-var "window") "lastTimeout") 1) 10 "window timeout recorded")
         (check-true (expect-contract-error (lambda () (window-set-name! 1))) "window name validation")
         (check-true (expect-contract-error (lambda () (window-open 1))) "window open validation")
         (check-true (expect-contract-error (lambda () (window-confirm 1))) "window confirm validation")
         #t))
 (list "Document and element wrappers"
       (let ()
         (install!)
         (define body (document-body))
         (define host (document-create-element "div"))
         (define leaf (document-create-text-node "leaf"))
         (append-child! host leaf)
         (append-child! body host)
         (check-equal (get-attribute (dom-test-fixture "host") "data-state") "ready" "element attribute")
         (check-equal (get-attribute (document-get-element-by-id "dom-host") "data-state")
                      "ready"
                      "document lookup")
         (check-equal (get-attribute (document-query-selector "#dom-host") "id") "dom-host" "document query selector")
         (check-equal (js-ref (document-query-selector-all "#dom-host") "length") 1 "document query selector all")
         (check-true (toggle-attribute! host "data-flag") "element toggle on")
         (check-false (toggle-attribute! host "data-flag") "element toggle off")
         (replace-children! host (document-create-text-node "updated"))
         (check-equal (js-ref host "textContent") "updated" "element replace-children")
         (check-true (expect-contract-error (lambda () (document-create-element 1))) "document create-element validation")
         (check-true (expect-contract-error (lambda () (set-attribute! host 1 "value"))) "element set-attribute validation")
         #t)))
