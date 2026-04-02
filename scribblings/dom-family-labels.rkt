#lang racket/base

(require racket/contract/base
         "dom-core-labels.rkt")

;; Docs-only fake bindings for the split DOM wrapper Scribble pages.

(provide
  (all-from-out "dom-core-labels.rkt")
  (struct-out animation)
  (struct-out computed-style-map)
  (struct-out dom-rect-list)
  (struct-out audio-track-list)
  (struct-out text-track-list)
  (struct-out video-track-list)
  (struct-out time-ranges)
  (struct-out audio-track)
  (struct-out text-track)
  (struct-out video-track)
  (struct-out media-keys-info)
  (struct-out media-source-info)
  (struct-out element)
  element-class-list
  element-id
  element-set-id!
  element-class-name
  element-set-class-name!
  element-set-attribute!
  element-get-attribute
  element-tag-name
  element-local-name
  element-namespace-uri
  element-prefix
  element-is-connected?
  element-has-attribute?
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
  element-query-selector
  element-get-bounding-client-rect
  element-get-client-rects
  element-inner-html
  element-set-inner-html!
  element-outer-html
  element-text-content
  element-set-text-content!
  element-scroll-top
  element-set-scroll-top!
  element-scroll-left
  element-set-scroll-left!
  element-scroll-width
  element-scroll-height
  element-client-width
  element-client-height
  element-offset-width
  element-offset-height
  element-append!
  element-prepend!
  element-before!
  element-after!
  element-remove!
  element-replace-children!
  element-replace-with!
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
  element-request-fullscreen
  element-request-pointer-lock
  element-scroll-into-view!
  element-scroll!
  element-scroll-by!
  element-scroll-to!
  dom-rect-list-length
  dom-rect-list-item
  element-set-attribute-ns!
  element-toggle-attribute!
  animation-raw
  computed-style-map-raw
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
  document-create-comment
  document-create-cdata-section
  document-create-document-fragment
  document-create-processing-instruction
  document-adopt-node
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
  (struct-out dom-rect)
  (struct-out selection)
  (struct-out media-stream)
  (struct-out media-error-info)
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
  media-controls-list
  media-keys
  media-set-media-keys!
  media-src-object
  media-set-src-object!
  media-add-text-track!
  media-audio-tracks
  media-buffered
  media-error
  media-error-info-code
  media-error-info-message
  media-play
  media-pause
  media-load!
  media-fast-seek!
  media-can-play-type
  media-played
  media-seekable
  media-text-tracks
  media-video-tracks
  media-capture-stream
  audio-track-kind
  audio-track-label
  audio-track-language
  audio-track-id
  audio-track-enabled?
  audio-track-set-enabled!
  text-track-kind
  text-track-label
  text-track-language
  text-track-id
  text-track-mode
  text-track-set-mode!
  video-track-kind
  video-track-label
  video-track-language
  video-track-id
  video-track-selected?
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
(struct dom-token-list (raw) #:transparent)
(struct shadow-root (raw) #:transparent)
(struct animation (raw) #:transparent)
(struct computed-style-map (raw) #:transparent)
(struct node-list (raw) #:transparent)
(struct html-collection (raw) #:transparent)
(struct dom-rect-list (raw) #:transparent)
(struct audio-track-list (raw) #:transparent)
(struct text-track-list (raw) #:transparent)
(struct video-track-list (raw) #:transparent)
(struct time-ranges (raw) #:transparent)
(struct dom-rect (raw) #:transparent)
(struct selection (raw) #:transparent)
(struct media-stream (raw) #:transparent)
(struct media-error-info (raw) #:transparent)
(define element-class-list any/c)
(define element-id any/c)
(define element-set-id! any/c)
(define element-class-name any/c)
(define element-set-class-name! any/c)
(define element-set-attribute! any/c)
(define element-get-attribute any/c)
(define element-tag-name any/c)
(define element-local-name any/c)
(define element-namespace-uri any/c)
(define element-prefix any/c)
(define element-is-connected? any/c)
(define element-has-attribute? any/c)
(define element-remove-attribute! any/c)
(define element-remove-attribute-ns! any/c)
(define element-matches? any/c)
(define element-closest any/c)
(define element-get-attribute-names any/c)
(define element-get-attribute-ns any/c)
(define dom-token-list-value any/c)
(define dom-token-list-length any/c)
(define dom-token-list-item any/c)
(define dom-token-list-contains? any/c)
(define dom-token-list-add! any/c)
(define dom-token-list-remove! any/c)
(define dom-token-list-toggle! any/c)
(define dom-token-list-replace! any/c)
(define element-children any/c)
(define element-child-element-count any/c)
(define element-first-element-child any/c)
(define element-last-element-child any/c)
(define element-query-selector any/c)
(define element-get-bounding-client-rect any/c)
(define element-get-client-rects any/c)
(define element-inner-html any/c)
(define element-set-inner-html! any/c)
(define element-outer-html any/c)
(define element-text-content any/c)
(define element-set-text-content! any/c)
(define element-scroll-top any/c)
(define element-set-scroll-top! any/c)
(define element-scroll-left any/c)
(define element-set-scroll-left! any/c)
(define element-scroll-width any/c)
(define element-scroll-height any/c)
(define element-client-width any/c)
(define element-client-height any/c)
(define element-offset-width any/c)
(define element-offset-height any/c)
(define element-append! any/c)
(define element-prepend! any/c)
(define element-before! any/c)
(define element-after! any/c)
(define element-remove! any/c)
(define element-replace-children! any/c)
(define element-replace-with! any/c)
(define element-get-elements-by-class-name any/c)
(define element-get-elements-by-tag-name any/c)
(define element-get-elements-by-tag-name-ns any/c)
(define element-insert-adjacent-element! any/c)
(define element-insert-adjacent-html! any/c)
(define element-insert-adjacent-text! any/c)
(define element-computed-style-map any/c)
(define element-get-animations any/c)
(define element-attach-shadow! any/c)
(define element-shadow-root any/c)
(define element-animate any/c)
(define element-request-fullscreen any/c)
(define element-request-pointer-lock any/c)
(define element-scroll-into-view! any/c)
(define element-scroll! any/c)
(define element-scroll-by! any/c)
(define element-scroll-to! any/c)
(define node-list-length any/c)
(define node-list-item any/c)
(define html-collection-length any/c)
(define html-collection-item any/c)
(define html-collection-named-item any/c)
(define dom-rect-list-length any/c)
(define dom-rect-list-item any/c)
(define audio-track-list-length any/c)
(define audio-track-list-item any/c)
(define text-track-list-length any/c)
(define text-track-list-item any/c)
(define video-track-list-length any/c)
(define video-track-list-item any/c)
(define time-ranges-length any/c)
(define time-ranges-start any/c)
(define time-ranges-end any/c)
(define element-set-attribute-ns! any/c)
(define element-toggle-attribute! any/c)
(define shadow-root-host any/c)
(define shadow-root-mode any/c)
(define shadow-root-delegates-focus? any/c)
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
(define document-create-comment any/c)
(define document-create-cdata-section any/c)
(define document-create-document-fragment any/c)
(define document-create-processing-instruction any/c)
(define document-adopt-node any/c)

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
(define selection-range-count any/c)
(define selection-is-collapsed? any/c)
(define selection-anchor-node any/c)
(define selection-focus-node any/c)
(define selection-to-string any/c)
(define selection-remove-all-ranges! any/c)
(define media-query-list-media any/c)
(define media-query-list-matches? any/c)
(define css-style-declaration-css-text any/c)
(define css-style-declaration-set-css-text! any/c)
(define css-style-declaration-length any/c)
(define css-style-declaration-item any/c)
(define css-style-declaration-get-property-value any/c)
(define css-style-declaration-set-property! any/c)
(define css-style-declaration-remove-property! any/c)
(define document-close any/c)
(define document-element-from-point any/c)
(define document-elements-from-point any/c)
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
(define media-controls-list any/c)
(define media-keys any/c)
(define media-set-media-keys! any/c)
(define media-src-object any/c)
(define media-set-src-object! any/c)
(define media-add-text-track! any/c)
(define media-audio-tracks any/c)
(define media-buffered any/c)
(define media-error any/c)
(define media-error-info-code any/c)
(define media-error-info-message any/c)
(define audio-track-kind any/c)
(define audio-track-label any/c)
(define audio-track-language any/c)
(define audio-track-id any/c)
(define audio-track-enabled? any/c)
(define audio-track-set-enabled! boolean?)
(define text-track-kind any/c)
(define text-track-label any/c)
(define text-track-language any/c)
(define text-track-id any/c)
(define text-track-mode any/c)
(define text-track-set-mode! any/c)
(define video-track-kind any/c)
(define video-track-label any/c)
(define video-track-language any/c)
(define video-track-id any/c)
(define video-track-selected? any/c)
(define media-play any/c)
(define media-pause any/c)
(define media-load! any/c)
(define media-fast-seek! any/c)
(define media-can-play-type any/c)
(define media-played any/c)
(define media-seekable any/c)
(define media-text-tracks any/c)
(define media-video-tracks any/c)
(define media-capture-stream any/c)
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
