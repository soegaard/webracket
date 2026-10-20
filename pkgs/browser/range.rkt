#lang racket/base

(require browser/element
         webracket/unsafe/document
         webracket/unsafe/standard
         (only-in webracket/primitives external?))

(provide (except-out (struct-out dom-range-info) dom-range-info?)
         (except-out (struct-out dom-tree-walker-info) dom-tree-walker-info?)
         (rename-out [dom-range-info/surface? dom-range?]
                     [dom-tree-walker-info/surface? dom-tree-walker?])
         node-filter-show-all
         node-filter-show-text
         document-create-range
         document-create-tree-walker
         range-start-container
         range-start-offset
         range-end-container
         range-end-offset
         range-collapsed?
         range-set-start!
         range-set-end!
         range-select-node-contents!
         range-collapse!
         range-compare-start-to-start
         range-get-client-rects
         range-get-bounding-client-rect
         tree-walker-next-node
         selection-anchor-offset
         selection-focus-offset
         selection-get-range
         selection-add-range!)

;; dom-range-info : external/raw -> dom-range-info?
;;   Wrap a native DOM Range without converting its UTF-16 offsets.
(struct dom-range-info (raw) #:transparent)

;; dom-tree-walker-info : external/raw -> dom-tree-walker-info?
;;   Wrap a native TreeWalker while retaining its traversal state.
(struct dom-tree-walker-info (raw) #:transparent)

;; node-filter-show-all : exact-nonnegative-integer?
;;   Native NodeFilter mask selecting every node type.
(define node-filter-show-all #xffffffff)

;; node-filter-show-text : exact-nonnegative-integer?
;;   Native NodeFilter mask selecting Text nodes only.
(define node-filter-show-text #x4)

;; range-raw-host-value? : any/c -> boolean?
;;   Report whether value is a non-nullish browser host object.
(define (range-raw-host-value? value)
  (and (external? value)
       (not (js-nullish? value))))

;; range-host-method? : external/raw string? -> boolean?
;;   Report whether raw provides a callable host method.
(define (range-host-method? raw method-name)
  (and (range-raw-host-value? raw)
       (let ([method (js-ref raw method-name)])
         (and (not (js-nullish? method))
              (string=? (js-typeof method) "function")))))

;; dom-range-info/surface? : any/c -> boolean?
;;   Report whether value wraps a usable DOM Range.
(define (dom-range-info/surface? value)
  (and (dom-range-info? value)
       (range-host-method? (dom-range-info-raw value) "setStart")
       (range-host-method? (dom-range-info-raw value) "getClientRects")))

;; dom-tree-walker-info/surface? : any/c -> boolean?
;;   Report whether value wraps a usable TreeWalker.
(define (dom-tree-walker-info/surface? value)
  (and (dom-tree-walker-info? value)
       (range-host-method? (dom-tree-walker-info-raw value) "nextNode")))

;; range-node-raw : symbol? dom-node? -> external/raw
;;   Unwrap and validate a Node accepted by Range and TreeWalker operations.
(define (range-node-raw who node)
  (define raw (dom-node-unwrap node))
  (unless (range-raw-host-value? raw)
    (raise-argument-error who "dom-node?" node))
  raw)

;; range-offset : symbol? any/c -> exact-nonnegative-integer?
;;   Validate a native UTF-16 or child-boundary offset without conversion.
(define (range-offset who value)
  (unless (exact-nonnegative-integer? value)
    (raise-argument-error who "exact-nonnegative-integer?" value))
  value)

;; range-check : symbol? any/c -> dom-range-info?
;;   Check a Range wrapper before an operation.
(define (range-check who value)
  (unless (dom-range-info/surface? value)
    (raise-argument-error who "dom-range?" value))
  value)

;; tree-walker-check : symbol? any/c -> dom-tree-walker-info?
;;   Check a TreeWalker wrapper before an operation.
(define (tree-walker-check who value)
  (unless (dom-tree-walker-info/surface? value)
    (raise-argument-error who "dom-tree-walker?" value))
  value)

;; document-create-range : -> dom-range?
;;   Create a native Range in the current document.
(define (document-create-range)
  (dom-range-info (js-create-range)))

;; document-create-tree-walker : dom-node? exact-nonnegative-integer? -> dom-tree-walker?
;;   Create a TreeWalker rooted at node; what-to-show is a native NodeFilter mask.
(define (document-create-tree-walker root what-to-show)
  (unless (exact-nonnegative-integer? what-to-show)
    (raise-argument-error 'document-create-tree-walker
                          "exact-nonnegative-integer?"
                          what-to-show))
  (dom-tree-walker-info
   (js-create-tree-walker (range-node-raw 'document-create-tree-walker root)
                          what-to-show
                          (js-null))))

;; range-node-property : symbol? dom-range? string? -> dom-node?
;;   Read and wrap a non-null Range endpoint node.
(define (range-node-property who range property-name)
  (define raw (dom-range-info-raw (range-check who range)))
  (define node (js-ref/extern raw property-name))
  (unless (range-raw-host-value? node)
    (raise-arguments-error who
                           (string-append "Range." property-name " is not available")))
  (dom-node-wrap node))

;; range-offset-property : symbol? dom-range? string? -> exact-nonnegative-integer?
;;   Read a native Range endpoint offset in UTF-16 code units for Text nodes.
(define (range-offset-property who range property-name)
  (define value (js-ref (dom-range-info-raw (range-check who range)) property-name))
  (unless (exact-nonnegative-integer? value)
    (raise-arguments-error who
                           (string-append "Range." property-name
                                          " did not return a nonnegative integer")))
  value)

;; range-start-container : dom-range? -> dom-node?
;;   Read the Range start container.
(define (range-start-container range)
  (range-node-property 'range-start-container range "startContainer"))

;; range-start-offset : dom-range? -> exact-nonnegative-integer?
;;   Read the native Range start offset in its unconverted browser domain.
(define (range-start-offset range)
  (range-offset-property 'range-start-offset range "startOffset"))

;; range-end-container : dom-range? -> dom-node?
;;   Read the Range end container.
(define (range-end-container range)
  (range-node-property 'range-end-container range "endContainer"))

;; range-end-offset : dom-range? -> exact-nonnegative-integer?
;;   Read the native Range end offset in its unconverted browser domain.
(define (range-end-offset range)
  (range-offset-property 'range-end-offset range "endOffset"))

;; range-collapsed? : dom-range? -> boolean?
;;   Report whether the Range endpoints are equal.
(define (range-collapsed? range)
  (define value (js-ref (dom-range-info-raw (range-check 'range-collapsed? range)) "collapsed"))
  (unless (boolean? value)
    (raise-arguments-error 'range-collapsed? "Range.collapsed did not return a boolean"))
  value)

;; range-set-start! : dom-range? dom-node? exact-nonnegative-integer? -> void?
;;   Set a Range start; Text offsets are native UTF-16 code units.
(define (range-set-start! range node offset)
  (define raw (dom-range-info-raw (range-check 'range-set-start! range)))
  (js-send/extern/nullish raw
                          "setStart"
                          (vector (range-node-raw 'range-set-start! node)
                                  (range-offset 'range-set-start! offset)))
  (void))

;; range-set-end! : dom-range? dom-node? exact-nonnegative-integer? -> void?
;;   Set a Range end; Text offsets are native UTF-16 code units.
(define (range-set-end! range node offset)
  (define raw (dom-range-info-raw (range-check 'range-set-end! range)))
  (js-send/extern/nullish raw
                          "setEnd"
                          (vector (range-node-raw 'range-set-end! node)
                                  (range-offset 'range-set-end! offset)))
  (void))

;; range-select-node-contents! : dom-range? dom-node? -> void?
;;   Set the Range to the complete contents of node.
(define (range-select-node-contents! range node)
  (js-send/extern/nullish (dom-range-info-raw (range-check 'range-select-node-contents! range))
                          "selectNodeContents"
                          (vector (range-node-raw 'range-select-node-contents! node)))
  (void))

;; range-collapse! : dom-range? [boolean?] -> void?
;;   Collapse to the end by default, or to the start when to-start? is true.
(define (range-collapse! range [to-start? #f])
  (unless (boolean? to-start?)
    (raise-argument-error 'range-collapse! "boolean?" to-start?))
  (js-send/extern/nullish (dom-range-info-raw (range-check 'range-collapse! range))
                          "collapse"
                          (vector to-start?))
  (void))

;; range-compare-start-to-start : dom-range? dom-range? -> exact-integer?
;;   Compare two Range starts with native Range.START_TO_START semantics.
(define (range-compare-start-to-start range other)
  (define value
    (js-send/value (dom-range-info-raw (range-check 'range-compare-start-to-start range))
                   "compareBoundaryPoints"
                   (vector 0 (dom-range-info-raw
                              (range-check 'range-compare-start-to-start other)))))
  (unless (exact-integer? value)
    (raise-arguments-error 'range-compare-start-to-start
                           "Range.compareBoundaryPoints did not return an integer"))
  value)

;; range-get-client-rects : dom-range? -> vector?
;;   Return native client rectangles in browser coordinate space.
(define (range-get-client-rects range)
  (array-like->vector
   'range-get-client-rects
   (js-send/extern (dom-range-info-raw (range-check 'range-get-client-rects range))
                   "getClientRects"
                   (vector))
   dom-rect-wrap))

;; range-get-bounding-client-rect : dom-range? -> dom-rect?
;;   Return the Range bounding rectangle in viewport coordinates.
(define (range-get-bounding-client-rect range)
  (dom-rect-wrap
   (js-send/extern (dom-range-info-raw (range-check 'range-get-bounding-client-rect range))
                   "getBoundingClientRect"
                   (vector))))

;; tree-walker-next-node : dom-tree-walker? -> (or/c #f dom-node?)
;;   Advance the walker and return the next matching node, if any.
(define (tree-walker-next-node walker)
  (dom-node-wrap
   (js-send/extern/null (dom-tree-walker-info-raw
                         (tree-walker-check 'tree-walker-next-node walker))
                        "nextNode"
                        (vector))))

;; selection-offset-property : symbol? dom-selection? string? -> exact-nonnegative-integer?
;;   Read a Selection endpoint in its native UTF-16 or child-boundary domain.
(define (selection-offset-property who selection property-name)
  (unless (dom-selection? selection)
    (raise-argument-error who "dom-selection?" selection))
  (define value (js-ref (dom-selection-unwrap selection) property-name))
  (unless (exact-nonnegative-integer? value)
    (raise-arguments-error who
                           (string-append "Selection." property-name
                                          " did not return a nonnegative integer")))
  value)

;; selection-anchor-offset : dom-selection? -> exact-nonnegative-integer?
;;   Read the selection anchor offset without scalar conversion.
(define (selection-anchor-offset selection)
  (selection-offset-property 'selection-anchor-offset selection "anchorOffset"))

;; selection-focus-offset : dom-selection? -> exact-nonnegative-integer?
;;   Read the selection focus offset without scalar conversion.
(define (selection-focus-offset selection)
  (selection-offset-property 'selection-focus-offset selection "focusOffset"))

;; selection-get-range : dom-selection? exact-nonnegative-integer? -> dom-range?
;;   Return the selected native Range at index.
(define (selection-get-range selection index)
  (unless (dom-selection? selection)
    (raise-argument-error 'selection-get-range "dom-selection?" selection))
  (dom-range-info
   (js-send/extern (dom-selection-unwrap selection)
                   "getRangeAt"
                   (vector (range-offset 'selection-get-range index)))))

;; selection-add-range! : dom-selection? dom-range? -> void?
;;   Add a native Range to the browser selection.
(define (selection-add-range! selection range)
  (unless (dom-selection? selection)
    (raise-argument-error 'selection-add-range! "dom-selection?" selection))
  (js-send/extern/nullish (dom-selection-unwrap selection)
                          "addRange"
                          (vector (dom-range-info-raw
                                   (range-check 'selection-add-range! range))))
  (void))
