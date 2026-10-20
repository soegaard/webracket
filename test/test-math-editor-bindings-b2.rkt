#lang webracket/base

;; B2 browser-binding consumer. It exercises the public geometry and lifecycle
;; adapters without embedding any math-editor policy or renderer algorithm.

(require browser/abort
         browser/document
         browser/element
         browser/event
         browser/event-target
         browser/font
         browser/range
         browser/segmenter
         browser/window
         js/promise
         resize-observer)

(define assertion-count 0)

;; check-true : any/c string? -> void?
;;   Record a completed assertion or raise a labeled failure.
(define (check-true value label)
  (unless value
    (error 'test-math-editor-bindings-b2 label))
  (set! assertion-count (add1 assertion-count)))

;; check-equal : any/c any/c string? -> void?
;;   Record a completed equality assertion or raise a labeled failure.
(define (check-equal got expected label)
  (unless (equal? got expected)
    (error 'test-math-editor-bindings-b2 label))
  (set! assertion-count (add1 assertion-count)))

(define body (document-body))
(unless body
  (error 'test-math-editor-bindings-b2 "document.body is unavailable"))

(element-set-attribute! body 'data-math-editor-bindings-b2-state "started")

;; Range, TreeWalker, Selection, and native UTF-16 boundaries.
(define host (document-create-element 'section))
(define text (document-create-text-node "𝑥á"))
(append-child! host text)
(append-child! body host)
(define range (document-create-range))
(range-set-start! range text 0)
(range-set-end! range text 4)
(check-equal (range-start-offset range) 0 "range start UTF-16 offset")
(check-equal (range-end-offset range) 4 "range end UTF-16 offset")
(check-true (not (range-collapsed? range)) "range is not collapsed")
(check-true (vector? (range-get-client-rects range)) "range client rectangles vector")
(check-true (dom-rect? (range-get-bounding-client-rect range)) "range bounding rectangle")
(define walker (document-create-tree-walker host node-filter-show-text))
(check-equal (dom-node-value (tree-walker-next-node walker)) "𝑥á" "tree walker text")
(check-equal (tree-walker-next-node walker) #f "tree walker exhaustion")
(define selection (document-get-selection))
(unless selection
  (error 'test-math-editor-bindings-b2 "document selection is unavailable"))
(selection-remove-all-ranges! selection)
(selection-add-range! selection range)
(check-equal (selection-anchor-offset selection) 0 "selection anchor offset")
(check-equal (selection-focus-offset selection) 4 "selection focus offset")
(check-equal (range-end-offset (selection-get-range selection 0)) 4 "selection range endpoint")
(selection-remove-all-ranges! selection)

;; Native grapheme segmentation exposes browser UTF-16 indexes rather than a
;; hidden Racket scalar conversion.
(define segmenter (make-grapheme-segmenter))
(define segments (intl-segmenter-segment segmenter "𝑥á"))
(define first-segment (intl-segments-containing segments 0))
(define second-segment (intl-segments-containing segments 2))
(check-equal (intl-segment-data-segment first-segment) "𝑥" "first grapheme")
(check-equal (intl-segment-data-index first-segment) 0 "first grapheme UTF-16 index")
(check-equal (intl-segment-data-segment second-segment) "á" "combined grapheme")
(check-equal (intl-segment-data-index second-segment) 2 "combined grapheme UTF-16 index")

;; A listener token owns exactly one native registration and synchronous veto.
(define event-count 0)
(define listener-token
  (event-target-add-listener!
   host
   'webracket-b2-veto
   (lambda (evt)
     (set! event-count (add1 event-count))
     (prevent-default! evt))
   (make-event-listener-options #f #f #t #f)))
(check-true (not (event-target-dispatch! host (event-new 'webracket-b2-veto #t #t)))
            "listener token synchronous cancellation")
(check-true (event-target-dispatch! host (event-new 'webracket-b2-veto #t #t))
            "native once listener removal")
(check-equal event-count 1 "listener token executes once")
(check-true (not (event-target-listener-active? listener-token))
            "once token becomes inactive")
(event-target-remove-listener! listener-token)

;; Abort preserves the native synchronous state transition.
(define controller (make-abort-controller))
(define signal (abort-controller-signal controller))
(check-true (not (abort-signal-aborted? signal)) "fresh abort signal")
(abort-controller-abort/with-reason! controller "b2-stop")
(check-true (abort-signal-aborted? signal) "abort signal transition")
(check-equal (abort-signal-reason signal) "b2-stop" "abort signal reason")

;; These callbacks settle on distinct browser scheduling queues. The terminal
;; receipt is not published until all asynchronous evidence has arrived.
(define promise-seen? #f)
(define microtask-seen? #f)
(define timer-seen? #f)
(define frame-seen? #f)
(define font-seen? #f)
(define resize-seen? #f)

;; finish-if-ready : -> void?
;;   Publish the terminal receipt only after every scheduled callback ran.
(define (finish-if-ready)
  (when (and promise-seen? microtask-seen? timer-seen? frame-seen? font-seen? resize-seen?)
    (element-set-attribute! body 'data-math-editor-bindings-b2-state "pass")
    (element-set-attribute! body
                            'data-math-editor-bindings-b2-assertions
                            (number->string assertion-count))))

(js-promise-then
 (js-promise-resolved "b2")
 (lambda (value)
   (check-equal value "b2" "promise fulfillment value")
   (set! promise-seen? #t)
   (finish-if-ready)))

(window-queue-microtask
 (lambda ()
   (set! microtask-seen? #t)
   (finish-if-ready)))

(window-set-timeout/delay
 (lambda ()
   (set! timer-seen? #t)
   (finish-if-ready))
 0)

(window-request-animation-frame
 (lambda (timestamp)
   (check-true (real? timestamp) "animation frame timestamp")
   (set! frame-seen? #t)
   (finish-if-ready)))

(define fonts (document-font-face-set))
(unless fonts
  (error 'test-math-editor-bindings-b2 "document.fonts is unavailable"))
(check-true (string? (font-face-set-status fonts)) "font-face-set status")
(js-promise-then
 (font-face-set-ready fonts)
 (lambda (value)
   (set! font-seen? #t)
   (finish-if-ready)))

(define observer
  (make-resize-observer
   (lambda (entries observed)
     (check-true (> (vector-length entries) 0) "resize observer initial entry")
     (resize-observer-disconnect! observed)
     (set! resize-seen? #t)
     (finish-if-ready))))
(resize-observer-observe! observer host)

(define viewport (window-visual-viewport))
(when viewport
  (check-true (>= (window-visual-viewport-width viewport) 0) "visual viewport width")
  (check-true (>= (window-visual-viewport-scale viewport) 0) "visual viewport scale"))
(check-true (string? (css-style-declaration-get-property-value
                      (window-get-computed-style host)
                      "display"))
            "computed style")
