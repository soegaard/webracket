#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for the split DOM wrapper Scribble pages.

(provide
  dom
  Window
  (struct-out window)
  (struct-out element)
  (struct-out text)
  (struct-out attr)
  element-id
  element-set-id!
  element-class-name
  element-set-class-name!
  element-has-attribute?
  element-has-attributes?
  element-remove-attribute!
  element-remove-attribute-ns!
  element-matches?
  element-closest
  element-get-attribute-names
  element-get-attribute-ns
  element-children
  element-child-element-count
  element-first-element-child
  element-last-element-child
  element-inner-html
  element-set-inner-html!
  element-outer-html
  element-set-outer-html!
  element-text-content
  element-set-text-content!
  element-append!
  element-prepend!
  element-before!
  element-after!
  element-get-elements-by-class-name
  element-get-elements-by-tag-name
  element-get-elements-by-tag-name-ns
  element-insert-adjacent-element!
  element-insert-adjacent-html!
  element-insert-adjacent-text!
  element-computed-style-map
  element-get-animations
  element-attach-shadow!
  element-animate
  element-get-attribute-node
  element-get-attribute-node-ns
  element-set-attribute-node!
  element-set-attribute-node-ns!
  element-remove-attribute-node!
  element-has-pointer-capture?
  element-set-pointer-capture!
  element-release-pointer-capture!
  document-create-element
  document-create-attribute
  document-create-attribute-ns
  document-create-text-node
  window-self
  window-document
  (struct-out window-document-info)
  window-name
  window-set-name!
  window-location
  (struct-out window-location-info)
  (struct-out window-scroll-options)
  window-set-location!
  window-open
  window-fetch
  window-confirm
  window-prompt
  window-alert
  window-print
  window-focus
  window-stop
  window-scroll-to
  window-scroll-by
  window-scroll
  window-resize-to
  window-resize-by
  window-move-to
  window-move-by
  window-set-timeout
  window-set-timeout/delay
  window-clear-timeout
  window-set-interval
  window-clear-interval
  window-request-animation-frame
  window-cancel-animation-frame
  window-request-idle-callback
  window-cancel-idle-callback
  window-get-selection
  window-match-media
  window-get-computed-style
  window-structured-clone
  Document
  (struct-out document)
  (struct-out attr)
  (struct-out dom-rect)
  (struct-out performance-event-count-map)
  performance-event-count-map-size
  performance-event-count-map-entries
  performance-event-count-map-keys
  performance-event-count-map-values
  performance-event-count-map-get
  performance-event-count-map-has?
  performance-event-count-map-for-each
  performance-event-counts
  performance-interaction-count
  (struct-out performance-memory-info)
  performance-memory-info-js-heap-size-limit
  performance-memory-info-total-js-heap-size
  performance-memory-info-used-js-heap-size
  performance-memory
  performance-time-origin
  performance-now
  performance-clear-marks
  performance-clear-measures
  performance-clear-resource-timings
  performance-get-entries
  performance-get-entries-by-name
  performance-get-entries-by-type
  performance-mark
  performance-measure
  performance-measure-user-agent-specific-memory
  performance-set-resource-timing-buffer-size
  performance-to-json
  document
  document-head
  document-body
  document-element
  document-create-element
  document-create-text-node
  document-get-element-by-id
  document-query-selector
  document-query-selector-all
  document-has-focus?
  document-get-selection
  document-close
  document-open
  document-element-from-point
  document-elements-from-point
  event?
  mouse-event?
  keyboard-event?
  pointer-event?
  focus-event?
  input-event?
  submit-event?
  touch-event?
  wheel-event?
  touch-list?
  touch?
  event-type
  event-target
  event-current-target
  prevent-default!
  stop-propagation!
  stop-immediate-propagation!
  mouse-event-offset-x
  mouse-event-offset-y
  mouse-event-client-x
  mouse-event-client-y
  keyboard-event-key
  keyboard-event-code
  touch-event-touches
  touch-event-target-touches
  touch-event-changed-touches
  touch-list-length
  touch-list-ref
  touch-identifier
  touch-client-x
  touch-client-y
  touch-page-x
  touch-page-y
  touch-screen-x
  touch-screen-y
  dom-rect-left
  dom-rect-top
  dom-rect-width
  dom-rect-height
  append-child!
  set-attribute!
  get-attribute
  get-bounding-client-rect
  get-client-rects
  query-selector
  query-selector-all
  remove!
  replace-children!
  replace-with!
  request-fullscreen
  request-pointer-lock
  scroll!
  scroll-by!
  scroll-into-view!
  scroll-to!
  set-attribute-ns!
  toggle-attribute!
  canvas-capture-stream
  canvas-get-context
  canvas-width
  canvas-set-width!
  canvas-height
  canvas-set-height!
  canvas-to-data-url
  canvas-to-blob
  canvas-transfer-control-to-offscreen
  canvas-2d-canvas
  canvas-2d-direction
  canvas-2d-set-direction!
  canvas-2d-fill-style
  canvas-2d-set-fill-style!
  canvas-2d-stroke-style
  canvas-2d-set-stroke-style!
  canvas-2d-fill-rect
  canvas-2d-stroke-rect
  canvas-2d-begin-path
  canvas-2d-close-path
  canvas-2d-move-to
  canvas-2d-line-to
  canvas-2d-arc
  canvas-2d-clear-rect
  canvas-2d-fill
  canvas-2d-stroke
  canvas-2d-save
  canvas-2d-restore
  canvas-2d-translate
  canvas-2d-scale
  canvas-2d-rotate
  canvas-2d-fill-text
  canvas-2d-stroke-text
  canvas-2d-measure-text
  canvas-2d-create-linear-gradient
  canvas-2d-create-radial-gradient
  media-current-time
  media-set-current-time!
  media-volume
  media-set-volume!
  media-muted
  media-set-muted!
  media-default-muted
  media-set-default-muted!
  media-default-playback-rate
  media-set-default-playback-rate!
  media-playback-rate
  media-set-playback-rate!
  media-controls?
  media-set-controls!
  media-loop?
  media-set-loop!
  media-preload
  media-set-preload!
  media-src
  media-set-src!
  media-play
  media-pause
  media-load!
  media-fast-seek!
  media-can-play-type
  media-set-sink-id!
  image-new
  image-alt
  image-set-alt!
  image-src
  image-set-src!
  image-width
  image-set-width!
  image-height
  image-set-height!
  image-current-src
  image-decoding
  image-set-decoding!
  image-loading
  image-set-loading!
  image-complete?
  image-cross-origin
  image-set-cross-origin!
  (for-label (all-defined-out)))

(define dom any/c)
(define Window any/c)
(struct window (raw) #:transparent)
(struct element (raw) #:transparent)
(define window-self any/c)
(define window-document any/c)
(struct window-document-info (raw) #:transparent)
(define window-name any/c)
(define window-set-name! any/c)
(define window-location any/c)
(struct window-location-info (raw) #:transparent)
(struct window-scroll-options (top left behavior) #:transparent)
(define window-set-location! any/c)
(define window-open any/c)
(define window-fetch any/c)
(define window-confirm any/c)
(define window-prompt any/c)
(define window-alert any/c)
(define window-print any/c)
(define window-focus any/c)
(define window-stop any/c)
(define window-scroll-to any/c)
(define window-scroll-by any/c)
(define window-scroll any/c)
(define window-resize-to any/c)
(define window-resize-by any/c)
(define window-move-to any/c)
(define window-move-by any/c)
(define window-set-timeout any/c)
(define window-set-timeout/delay any/c)
(define window-clear-timeout any/c)
(define window-set-interval any/c)
(define window-clear-interval any/c)
(define window-request-animation-frame any/c)
(define window-cancel-animation-frame any/c)
(define window-request-idle-callback any/c)
(define window-cancel-idle-callback any/c)
(define window-get-selection any/c)
(define window-match-media any/c)
(define window-get-computed-style any/c)
(define window-structured-clone any/c)
(define Document any/c)
(struct document (raw) #:transparent)
(struct text (raw) #:transparent)
(struct attr (raw) #:transparent)
(struct dom-rect (raw) #:transparent)
(define element-id any/c)
(define element-set-id! any/c)
(define element-class-name any/c)
(define element-set-class-name! any/c)
(define element-has-attribute? any/c)
(define element-has-attributes? any/c)
(define element-remove-attribute! any/c)
(define element-remove-attribute-ns! any/c)
(define element-matches? any/c)
(define element-closest any/c)
(define element-get-attribute-names any/c)
(define element-get-attribute-ns any/c)
(define element-children any/c)
(define element-child-element-count any/c)
(define element-first-element-child any/c)
(define element-last-element-child any/c)
(define element-inner-html any/c)
(define element-set-inner-html! any/c)
(define element-outer-html any/c)
(define element-set-outer-html! any/c)
(define element-text-content any/c)
(define element-set-text-content! any/c)
(define element-append! any/c)
(define element-prepend! any/c)
(define element-before! any/c)
(define element-after! any/c)
(define element-get-elements-by-class-name any/c)
(define element-get-elements-by-tag-name any/c)
(define element-get-elements-by-tag-name-ns any/c)
(define element-insert-adjacent-element! any/c)
(define element-insert-adjacent-html! any/c)
(define element-insert-adjacent-text! any/c)
(define element-computed-style-map any/c)
(define element-get-animations any/c)
(define element-attach-shadow! any/c)
(define element-animate any/c)
(define element-get-attribute-node any/c)
(define element-get-attribute-node-ns any/c)
(define element-set-attribute-node! any/c)
(define element-set-attribute-node-ns! any/c)
(define element-remove-attribute-node! any/c)
(define element-has-pointer-capture? any/c)
(define element-set-pointer-capture! any/c)
(define element-release-pointer-capture! any/c)
(define document-create-element any/c)
(define document-create-attribute any/c)
(define document-create-attribute-ns any/c)
(define document-create-text-node any/c)

(struct iterator (raw) #:transparent #:constructor-name make-iterator)
(struct performance-event-count-map (raw) #:transparent)
(struct performance-memory-info (raw) #:transparent)

(define performance-event-count-map-size any/c)
(define performance-event-count-map-entries any/c)
(define performance-event-count-map-keys any/c)
(define performance-event-count-map-values any/c)
(define performance-event-count-map-get any/c)
(define performance-event-count-map-has? any/c)
(define performance-event-count-map-for-each any/c)
(define performance-event-counts any/c)
(define performance-interaction-count any/c)
(define performance-memory-info-js-heap-size-limit any/c)
(define performance-memory-info-total-js-heap-size any/c)
(define performance-memory-info-used-js-heap-size any/c)
(define performance-memory any/c)
(define performance-time-origin any/c)
(define performance-now any/c)
(define performance-clear-marks any/c)
(define performance-clear-measures any/c)
(define performance-clear-resource-timings any/c)
(define performance-get-entries any/c)
(define performance-get-entries-by-name any/c)
(define performance-get-entries-by-type any/c)
(define performance-mark any/c)
(define performance-measure any/c)
(define performance-measure-user-agent-specific-memory any/c)
(define performance-set-resource-timing-buffer-size any/c)
(define performance-to-json any/c)
(define document-open any/c)
(define document-head any/c)
(define document-body any/c)
(define document-element any/c)
(define document-get-element-by-id any/c)
(define document-query-selector any/c)
(define document-query-selector-all any/c)
(define document-has-focus? any/c)
(define document-get-selection any/c)
(define document-close any/c)
(define document-element-from-point any/c)
(define document-elements-from-point any/c)
(define event? any/c)
(define mouse-event? any/c)
(define keyboard-event? any/c)
(define pointer-event? any/c)
(define focus-event? any/c)
(define input-event? any/c)
(define submit-event? any/c)
(define touch-event? any/c)
(define wheel-event? any/c)
(define touch-list? any/c)
(define touch? any/c)
(define event-type any/c)
(define event-target any/c)
(define event-current-target any/c)
(define prevent-default! any/c)
(define stop-propagation! any/c)
(define stop-immediate-propagation! any/c)
(define mouse-event-offset-x any/c)
(define mouse-event-offset-y any/c)
(define mouse-event-client-x any/c)
(define mouse-event-client-y any/c)
(define keyboard-event-key any/c)
(define keyboard-event-code any/c)
(define touch-event-touches any/c)
(define touch-event-target-touches any/c)
(define touch-event-changed-touches any/c)
(define touch-list-length any/c)
(define touch-list-ref any/c)
(define touch-identifier any/c)
(define touch-client-x any/c)
(define touch-client-y any/c)
(define touch-page-x any/c)
(define touch-page-y any/c)
(define touch-screen-x any/c)
(define touch-screen-y any/c)
(define dom-rect-left any/c)
(define dom-rect-top any/c)
(define dom-rect-width any/c)
(define dom-rect-height any/c)
(define append-child! any/c)
(define set-attribute! any/c)
(define get-attribute any/c)
(define get-bounding-client-rect any/c)
(define get-client-rects any/c)
(define query-selector any/c)
(define query-selector-all any/c)
(define remove! any/c)
(define replace-children! any/c)
(define replace-with! any/c)
(define request-fullscreen any/c)
(define request-pointer-lock any/c)
(define scroll! any/c)
(define scroll-by! any/c)
(define scroll-into-view! any/c)
(define scroll-to! any/c)
(define set-attribute-ns! any/c)
(define toggle-attribute! any/c)
(define canvas-capture-stream any/c)
(define canvas-get-context any/c)
(define canvas-width any/c)
(define canvas-set-width! any/c)
(define canvas-height any/c)
(define canvas-set-height! any/c)
(define canvas-to-data-url any/c)
(define canvas-to-blob any/c)
(define canvas-transfer-control-to-offscreen any/c)
(define canvas-2d-canvas any/c)
(define canvas-2d-direction any/c)
(define canvas-2d-set-direction! any/c)
(define canvas-2d-fill-style any/c)
(define canvas-2d-set-fill-style! any/c)
(define canvas-2d-stroke-style any/c)
(define canvas-2d-set-stroke-style! any/c)
(define canvas-2d-fill-rect any/c)
(define canvas-2d-stroke-rect any/c)
(define canvas-2d-begin-path any/c)
(define canvas-2d-close-path any/c)
(define canvas-2d-move-to any/c)
(define canvas-2d-line-to any/c)
(define canvas-2d-arc any/c)
(define canvas-2d-clear-rect any/c)
(define canvas-2d-fill any/c)
(define canvas-2d-stroke any/c)
(define canvas-2d-save any/c)
(define canvas-2d-restore any/c)
(define canvas-2d-translate any/c)
(define canvas-2d-scale any/c)
(define canvas-2d-rotate any/c)
(define canvas-2d-fill-text any/c)
(define canvas-2d-stroke-text any/c)
(define canvas-2d-measure-text any/c)
(define canvas-2d-create-linear-gradient any/c)
(define canvas-2d-create-radial-gradient any/c)
(define media-current-time any/c)
(define media-set-current-time! any/c)
(define media-volume any/c)
(define media-set-volume! any/c)
(define media-muted any/c)
(define media-set-muted! any/c)
(define media-default-muted any/c)
(define media-set-default-muted! any/c)
(define media-default-playback-rate any/c)
(define media-set-default-playback-rate! any/c)
(define media-playback-rate any/c)
(define media-set-playback-rate! any/c)
(define media-controls? any/c)
(define media-set-controls! any/c)
(define media-loop? any/c)
(define media-set-loop! any/c)
(define media-preload any/c)
(define media-set-preload! any/c)
(define media-src any/c)
(define media-set-src! any/c)
(define media-play any/c)
(define media-pause any/c)
(define media-load! any/c)
(define media-fast-seek! any/c)
(define media-can-play-type any/c)
(define media-set-sink-id! any/c)
(define image-new any/c)
(define image-alt any/c)
(define image-set-alt! any/c)
(define image-src any/c)
(define image-set-src! any/c)
(define image-width any/c)
(define image-set-width! any/c)
(define image-height any/c)
(define image-set-height! any/c)
(define image-current-src any/c)
(define image-decoding any/c)
(define image-set-decoding! any/c)
(define image-loading any/c)
(define image-set-loading! any/c)
(define image-complete? any/c)
(define image-cross-origin any/c)
(define image-set-cross-origin! any/c)
