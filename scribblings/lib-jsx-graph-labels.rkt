#lang racket/base

(require racket/contract/base)

(provide
  jsx-board
  jsx-board-raw
  jsx-board?
  jsx-element
  jsx-element-raw
  jsx-element?
  jsx-point
  jsx-point-raw
  jsx-point?
  jsx-init-board
  jsx-board-create
  jsx-board-create-point
  jsx-board-create-line
  jsx-board-create-segment
  jsx-board-create-circle
  jsx-board-create-intersection
  jsx-board-create-text
  jsx-board-update!
  jsx-board-full-update!
  jsx-board-remove-object!
  jsx-board-suspend-update!
  jsx-board-unsuspend-update!
  jsx-point?
  jsx-point-attractor-distance
  jsx-set-point-attractor-distance!
  jsx-point-attractors
  jsx-set-point-attractors!
  jsx-point-attractor-unit
  jsx-set-point-attractor-unit!
  jsx-point-attract-to-grid
  jsx-set-point-attract-to-grid!
  jsx-point-face
  jsx-set-point-face!
  jsx-point-ignored-snap-to-points
  jsx-set-point-ignored-snap-to-points!
  jsx-point-infobox-digits
  jsx-set-point-infobox-digits!
  jsx-point-show-infobox
  jsx-set-point-show-infobox!
  jsx-point-size
  jsx-set-point-size!
  jsx-point-size-unit
  jsx-set-point-size-unit!
  jsx-point-snap-size-x
  jsx-set-point-snap-size-x!
  jsx-point-snap-size-y
  jsx-set-point-snap-size-y!
  jsx-point-snap-to-grid
  jsx-set-point-snap-to-grid!
  jsx-point-snap-to-points
  jsx-set-point-snap-to-points!
  jsx-point-snatch-distance
  jsx-set-point-snatch-distance!
  jsx-point-style
  jsx-set-point-style!
  jsx-point-x
  jsx-point-y
  jsx-point-zoom
  jsx-set-point-zoom!
  jsx-point-has-point
  jsx-point-is-on
  jsx-point-make-intersection!
  jsx-point-normalize-face
  jsx-point-set-style!
  jsx-point-update!
  jsx-point-update-renderer!
  jsx-point-update-transform!
  jsx-key?
  jsx-key->string
  jsx-parents
  jsx-set-attribute!
  jsx-dot
  jsx-create-board
  jsx-create
  jsx-create-point
  jsx-create-line
  jsx-create-segment
  jsx-create-circle
  jsx-create-perpendicular
  jsx-create-intersection
  jsx-create-text
  jsx-board-count-children
  jsx-coordinates
  jsx-on
  (for-label (all-defined-out)))

(define jsx-board any/c)
(define jsx-board-raw any/c)
(define jsx-board? any/c)
(define jsx-element any/c)
(define jsx-element-raw any/c)
(define jsx-element? any/c)
(define jsx-point any/c)
(define jsx-point-raw any/c)
(define jsx-point? any/c)
(define jsx-init-board any/c)
(define jsx-board-create any/c)
(define jsx-board-create-point any/c)
(define jsx-board-create-line any/c)
(define jsx-board-create-segment any/c)
(define jsx-board-create-circle any/c)
(define jsx-board-create-intersection any/c)
(define jsx-board-create-text any/c)
(define jsx-board-update! any/c)
(define jsx-board-full-update! any/c)
(define jsx-board-remove-object! any/c)
(define jsx-board-suspend-update! any/c)
(define jsx-board-unsuspend-update! any/c)
(define jsx-point-attractor-distance any/c)
(define jsx-set-point-attractor-distance! any/c)
(define jsx-point-attractors any/c)
(define jsx-set-point-attractors! any/c)
(define jsx-point-attractor-unit any/c)
(define jsx-set-point-attractor-unit! any/c)
(define jsx-point-attract-to-grid any/c)
(define jsx-set-point-attract-to-grid! any/c)
(define jsx-point-face any/c)
(define jsx-set-point-face! any/c)
(define jsx-point-ignored-snap-to-points any/c)
(define jsx-set-point-ignored-snap-to-points! any/c)
(define jsx-point-infobox-digits any/c)
(define jsx-set-point-infobox-digits! any/c)
(define jsx-point-show-infobox any/c)
(define jsx-set-point-show-infobox! any/c)
(define jsx-point-size any/c)
(define jsx-set-point-size! any/c)
(define jsx-point-size-unit any/c)
(define jsx-set-point-size-unit! any/c)
(define jsx-point-snap-size-x any/c)
(define jsx-set-point-snap-size-x! any/c)
(define jsx-point-snap-size-y any/c)
(define jsx-set-point-snap-size-y! any/c)
(define jsx-point-snap-to-grid any/c)
(define jsx-set-point-snap-to-grid! any/c)
(define jsx-point-snap-to-points any/c)
(define jsx-set-point-snap-to-points! any/c)
(define jsx-point-snatch-distance any/c)
(define jsx-set-point-snatch-distance! any/c)
(define jsx-point-style any/c)
(define jsx-set-point-style! any/c)
(define jsx-point-x any/c)
(define jsx-point-y any/c)
(define jsx-point-zoom any/c)
(define jsx-set-point-zoom! any/c)
(define jsx-point-has-point any/c)
(define jsx-point-is-on any/c)
(define jsx-point-make-intersection! any/c)
(define jsx-point-normalize-face any/c)
(define jsx-point-set-style! any/c)
(define jsx-point-update! any/c)
(define jsx-point-update-renderer! any/c)
(define jsx-point-update-transform! any/c)
(define jsx-key? any/c)
(define jsx-key->string any/c)
(define jsx-parents any/c)
(define jsx-set-attribute! any/c)
(define jsx-dot any/c)
(define jsx-create-board any/c)
(define jsx-create any/c)
(define jsx-create-point any/c)
(define jsx-create-line any/c)
(define jsx-create-segment any/c)
(define jsx-create-circle any/c)
(define jsx-create-perpendicular any/c)
(define jsx-create-intersection any/c)
(define jsx-create-text any/c)
(define jsx-board-count-children any/c)
(define jsx-coordinates any/c)
(define jsx-on any/c)
