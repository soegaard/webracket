;;;
;;; dom.ffi
;;;

;; Focused tests for the Window, Document, and Element wrapper libraries.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --ffi ../ffi/standard.ffi --ffi ../ffi/dom.ffi -r test-dom-window-document.rkt

(include-lib window)
(include-lib performance)
(include-lib document)
(include-lib element)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal (format "~a: got ~s want ~s" label got want))))

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
    window.__domTest.leaf = leaf;
    performance.clearMarks('webracket-performance-start');
    performance.clearMarks('webracket-performance-end');
    performance.clearMeasures('webracket-performance-span');"))

(define (dom-test-fixture name)
  (js-ref (js-var "__domTest") name))

(list
 (list "Window wrappers"
       (let ()
         (install!)
         (define timeout-fired 0)
         (define doc-info (window-document))
         (define loc-info (window-location))
         (check-true (window-document-info? doc-info) "window document wrapper")
         (check-true (external? (window-document-info-raw doc-info))
                     "window document wrapper raw")
         (check-true (window-location-info? loc-info) "window location wrapper")
         (check-true (external? (window-location-info-raw loc-info))
                     "window location wrapper raw")
         (check-equal (window-name) "dom-suite" "window name")
         (window-set-name! "web-bracket")
         (check-equal (window-name) "web-bracket" "window set name")
         (check-true (window-confirm "proceed?") "window confirm")
         (check-equal (js-ref (js-var "window") "lastConfirm") "proceed?" "window confirm arg")
         (define opened (window-open "https://example.invalid" "tab" "noopener" #f))
         (check-true (external? opened) "window open result")
         (check-equal (js-ref opened "kind") "popup" "window open payload")
         (check-equal (window-set-timeout/delay (procedure->external (lambda () (set! timeout-fired (add1 timeout-fired))))
                                                10.0)
                      17
                      "window timeout handle")
         (check-equal timeout-fired 1 "window timeout callback")
         (check-equal (vector-ref (js-ref (js-var "window") "lastTimeout") 1) 10 "window timeout recorded")
         (check-true (void? (window-scroll-to 0 240)) "window scroll default")
         (check-true (expect-contract-error (lambda () (window-set-name! 1))) "window name validation")
         (check-true (expect-contract-error (lambda () (window-open 1))) "window open validation")
         (check-true (expect-contract-error (lambda () (window-confirm 1))) "window confirm validation")
         #t))
 (list "Performance wrappers"
       (let ()
         (install!)
         (check-true (real? (performance-now)) "performance now")
         (check-true (real? (performance-time-origin)) "performance time origin")
         (check-true (exact-nonnegative-integer? (performance-interaction-count))
                     "performance interaction count")
         (define event-counts (performance-event-counts))
         (check-true (or (not event-counts)
                         (performance-event-count-map? event-counts))
                     "performance event counts")
         (when event-counts
           (check-true (external? (performance-event-count-map-raw event-counts))
                       "performance event count raw")
           (check-true (exact-nonnegative-integer? (performance-event-count-map-size event-counts))
                       "performance event count size")
           (check-true (iterator? (performance-event-count-map-entries event-counts))
                       "performance event count entries")
           (check-true (iterator? (performance-event-count-map-keys event-counts))
                       "performance event count keys")
           (check-true (iterator? (performance-event-count-map-values event-counts))
                       "performance event count values")
           (check-true (vector? (iterator->vector (performance-event-count-map-entries event-counts)))
                       "performance event count entries vector")
           (check-true (vector? (iterator->vector (performance-event-count-map-keys event-counts)))
                       "performance event count keys vector")
           (check-true (vector? (iterator->vector (performance-event-count-map-values event-counts)))
                       "performance event count values vector")
           (check-true (boolean? (performance-event-count-map-has? event-counts "click"))
                       "performance event count has")
           (check-true (or (not (performance-event-count-map-get event-counts "click"))
                           (exact-nonnegative-integer? (performance-event-count-map-get event-counts "click")))
                       "performance event count get")
           (check-true (void? (performance-event-count-map-for-each
                               event-counts
                               (lambda (count event-type map)
                                 (void count event-type map))))
                       "performance event count for-each")
           (check-true (external? (iterator-raw (performance-event-count-map-entries event-counts)))
                       "performance event count entries raw"))
         (define memory-info (performance-memory))
         (check-true (or (not memory-info)
                         (performance-memory-info? memory-info))
                     "performance memory")
         (when memory-info
           (check-true (exact-nonnegative-integer? (performance-memory-info-js-heap-size-limit memory-info))
                       "performance memory js heap size limit")
           (check-true (exact-nonnegative-integer? (performance-memory-info-total-js-heap-size memory-info))
                       "performance memory total js heap size")
           (check-true (exact-nonnegative-integer? (performance-memory-info-used-js-heap-size memory-info))
                       "performance memory used js heap size"))
         (performance-clear-marks "webracket-performance-start")
         (performance-clear-marks "webracket-performance-end")
         (performance-clear-measures "webracket-performance-span")
         (performance-clear-resource-timings)
         (check-true (void? (performance-clear-marks #f))
                     "performance clear marks default")
         (check-true (void? (performance-clear-measures #f))
                     "performance clear measures default")
         (performance-mark "webracket-performance-start")
         (performance-mark "webracket-performance-end")
         (performance-measure "webracket-performance-span"
                              "webracket-performance-start"
                              "webracket-performance-end")
         (define marks
           (performance-get-entries-by-name "webracket-performance-start"))
         (define measures
           (performance-get-entries-by-name "webracket-performance-span" "measure"))
         (define mark-entries
           (performance-get-entries-by-type "mark"))
         (define performance-json
           (performance-to-json))
         (check-true (>= (js-ref marks "length") 1) "performance marks by name")
         (check-true (>= (js-ref measures "length") 1) "performance measures by name")
         (check-true (>= (js-ref mark-entries "length") 1) "performance entries by type")
         (check-true (or (not performance-json)
                         (external? performance-json))
                     "performance to-json")
         (check-true (procedure? performance-measure-user-agent-specific-memory)
                     "performance memory estimator available")
         (performance-set-resource-timing-buffer-size 256)
         (check-true (expect-contract-error (lambda () (performance-clear-marks 1)))
                     "performance clear marks validation")
         (check-true (expect-contract-error (lambda () (performance-clear-measures 1)))
                     "performance clear measures validation")
         (check-true (expect-contract-error (lambda () (performance-get-entries-by-name 1)))
                     "performance get entries by name validation")
         (check-true (expect-contract-error (lambda () (performance-get-entries-by-type 1)))
                     "performance get entries by type validation")
         (check-true (expect-contract-error (lambda () (performance-event-count-map-size 1)))
                     "performance event count map size validation")
         (check-true (expect-contract-error (lambda () (performance-event-count-map-entries 1)))
                     "performance event count map entries validation")
         (check-true (expect-contract-error (lambda () (performance-event-count-map-keys 1)))
                     "performance event count map keys validation")
         (check-true (expect-contract-error (lambda () (performance-event-count-map-values 1)))
                     "performance event count map values validation")
         (check-true (expect-contract-error (lambda () (performance-event-count-map-get 1 "click")))
                     "performance event count map get validation")
         (check-true (expect-contract-error (lambda () (performance-event-count-map-get event-counts 1)))
                     "performance event count map get type validation")
         (check-true (expect-contract-error (lambda () (performance-event-count-map-has? 1 "click")))
                     "performance event count map has validation")
         (check-true (expect-contract-error (lambda () (performance-event-count-map-has? event-counts 1)))
                     "performance event count map has type validation")
         (check-true (expect-contract-error (lambda () (performance-event-count-map-for-each 1 void)))
                     "performance event count map for-each validation")
         (check-true (expect-contract-error (lambda () (performance-event-count-map-for-each event-counts 1)))
                     "performance event count map for-each proc validation")
         (check-true (expect-contract-error (lambda () (performance-memory-info-js-heap-size-limit 1)))
                     "performance memory info heap size validation")
         (check-true (expect-contract-error (lambda () (performance-memory-info-total-js-heap-size 1)))
                     "performance memory info total validation")
         (check-true (expect-contract-error (lambda () (performance-memory-info-used-js-heap-size 1)))
                     "performance memory info used validation")
         (check-true (expect-contract-error (lambda () (performance-mark 1)))
                     "performance mark validation")
         (check-true (expect-contract-error (lambda () (performance-measure 1)))
                     "performance measure validation")
         (check-true (expect-contract-error (lambda () (performance-set-resource-timing-buffer-size -1)))
                     "performance buffer size validation")
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
