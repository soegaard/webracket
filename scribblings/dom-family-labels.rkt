#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for the split DOM wrapper Scribble pages.

(provide
  window
  window-self
  window-document
  window-name
  window-set-name!
  window-location
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
  performance-now
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

(define window any/c)
(define window-self any/c)
(define window-document any/c)
(define window-name any/c)
(define window-set-name! any/c)
(define window-location any/c)
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
(define performance-now any/c)
(define document any/c)
(define document-head any/c)
(define document-body any/c)
(define document-element any/c)
(define document-create-element any/c)
(define document-create-text-node any/c)
(define document-get-element-by-id any/c)
(define document-query-selector any/c)
(define document-query-selector-all any/c)
(define document-has-focus? any/c)
(define document-get-selection any/c)
(define document-close any/c)
(define document-open any/c)
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
