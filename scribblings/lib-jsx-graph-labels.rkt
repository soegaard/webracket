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
  jsx-element-get-attribute
  jsx-element-get-attributes
  jsx-element-get-label-anchor
  jsx-element-get-name
  jsx-element-get-parents
  jsx-element-get-property
  jsx-element-get-snap-sizes
  jsx-element-get-text-anchor
  jsx-element-get-type
  jsx-element-has-point?
  jsx-element-hide!
  jsx-element-hide-element!
  jsx-element-no-highlight!
  jsx-element-prepare-update!
  jsx-element-remove!
  jsx-element-remove-all-ticks!
  jsx-element-remove-child!
  jsx-element-remove-descendants!
  jsx-element-remove-event!
  jsx-element-remove-ticks!
  jsx-element-set-attribute!
  jsx-element-set-label!
  jsx-element-set-label-text!
  jsx-element-set-name!
  jsx-element-set-parents!
  jsx-element-set-position!
  jsx-element-set-position-directly!
  jsx-element-set-property!
  jsx-element-show!
  jsx-element-show-element!
  jsx-element-update!
  jsx-element-update-renderer!
  jsx-element-update-visibility!
  jsx-element-use-locale!
  jsx-element-add-child!
  jsx-element-add-descendants!
  jsx-element-add-parents!
  jsx-element-add-parents-from-jc-functions!
  jsx-element-add-rotation!
  jsx-element-add-ticks!
  jsx-element-add-transform!
  jsx-element-animate!
  jsx-element-bounds
  jsx-element-clear-trace!
  jsx-element-clone-to-background!
  jsx-element-count-children
  jsx-element-create-gradient!
  jsx-element-create-label!
  jsx-element-draggable?
  jsx-element-eval
  jsx-element-eval-vis-prop
  jsx-element-format-number-locale
  jsx-element-full-update!
  jsx-element-generate-polynomial
  jsx-element-handle-snap-to-grid!
  jsx-element-normalize!
  jsx-element-resolve-shortcuts!
  jsx-element-set-arrow!
  jsx-element-set-dash!
  jsx-element-set-display-rend-node!
  jsx-element-snap-to-points!
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
  jsx-create-view3d
  jsx-view3d-create
  jsx-view3d-create-point3d
  jsx-view3d-create-line3d
  jsx-view3d-create-circle3d
  jsx-view3d-create-plane3d
  jsx-view3d-create-axes3d
  jsx-view3d-create-axis3d
  jsx-view3d-create-functiongraph3d
  jsx-view3d-create-sphere3d
  jsx-view3d-create-face3d
  jsx-view3d-create-surface3d
  jsx-view3d-create-intersectioncircle3d
  jsx-view3d-create-intersectionline3d
  jsx-view3d-create-curve3d
  jsx-view3d-create-parametricsurface3d
  jsx-view3d-create-polyhedron3d
  jsx-view3d-create-text3d
  jsx-view3d-create-ticks3d
  jsx-view3d-create-transformation3d
  jsx-view3d-create-vectorfield3d
  jsx-create-point
  jsx-create-line
  jsx-create-segment
  jsx-create-arc
  jsx-create-angle
  jsx-create-sector
  jsx-create-glider
  jsx-create-circle
  jsx-create-conic
  jsx-create-ellipse
  jsx-create-curve
  jsx-create-functiongraph
  jsx-create-polygon
  jsx-create-midpoint
  jsx-create-parallel
  jsx-create-arrowparallel
  jsx-create-axis
  jsx-create-grid
  jsx-create-boxplot
  jsx-create-tangent
  jsx-create-tangentto
  jsx-create-polarline
  jsx-create-polepoint
  jsx-create-radicalaxis
  jsx-create-circumcircle
  jsx-create-incircle
  jsx-create-incenter
  jsx-create-minorarc
  jsx-create-minorsector
  jsx-create-circumcirclearc
  jsx-create-circumcirclesector
  jsx-create-semicircle
  jsx-create-majorarc
  jsx-create-majorsector
  jsx-create-curveintersection
  jsx-create-curvedifference
  jsx-create-curveunion
  jsx-create-derivative
  jsx-create-integral
  jsx-create-riemannsum
  jsx-riemannsum-value
  jsx-create-slopefield
  jsx-slopefield-set-f!
  jsx-create-vectorfield
  jsx-vectorfield-set-f!
  jsx-create-implicitcurve
  jsx-create-spline
  jsx-create-cardinalspline
  jsx-create-comb
  jsx-create-metapostspline
  jsx-create-polygonalchain
  jsx-create-regularpolygon
  jsx-create-hyperbola
  jsx-create-parabola
  jsx-create-stepfunction
  jsx-create-inequality
  jsx-create-turtle
  jsx-create-nonreflexangle
  jsx-create-perpendicular
  jsx-create-reflection
  jsx-create-bisector
  jsx-create-normal
  jsx-create-intersection
  jsx-create-arrow
  jsx-create-button
  jsx-create-checkbox
  jsx-create-input
  jsx-create-slider
  jsx-create-chart
  jsx-create-legend
  jsx-create-smartlabel
  jsx-create-text
  jsx-create-image
  jsx-create-foreignobject
  jsx-create-tapemeasure
  jsx-create-hatch
  jsx-create-ticks
  jsx-create-transformation
  jsx-create-tracecurve
  jsx-create-slopetriangle
  jsx-create-parallelogram
  jsx-create-reflexangle
  jsx-create-measurement
  jsx-create-bisectorlines
  jsx-create-perpendicularsegment
  jsx-create-circumcenter
  jsx-create-mirrorelement
  jsx-create-mirrorpoint
  jsx-create-otherintersection
  jsx-create-orthogonalprojection
  jsx-create-parallelpoint
  jsx-create-perpendicularpoint
  jsx-create-group
  jsx-checkbox-value
  jsx-input-set!
  jsx-input-value
  jsx-slider-set-max!
  jsx-slider-set-min!
  jsx-slider-set-value!
  jsx-slider-value
  jsx-chart-draw-bar
  jsx-chart-draw-fit
  jsx-chart-draw-line
  jsx-chart-draw-pie
  jsx-chart-draw-points
  jsx-chart-draw-radar
  jsx-chart-draw-spline
  jsx-chart-update-data-array!
  jsx-chart-update-renderer!
  jsx-foreignobject-H
  jsx-foreignobject-W
  jsx-foreignobject-has-point?
  jsx-foreignobject-set-size!
  jsx-foreignobject-update!
  jsx-foreignobject-update-renderer!
  jsx-foreignobject-update-size!
  jsx-foreignobject-update-span!
  jsx-tapemeasure-value
  jsx-text-_createFctUpdateText
  jsx-text-_setText
  jsx-text-bounds
  jsx-text-checkForSizeUpdate
  jsx-text-convertGeonext2CSS
  jsx-text-convertGeonextAndSketchometry2CSS
  jsx-text-convertSketchometry2CSS
  jsx-text-crudeSizeEstimate
  jsx-text-escapeTicks
  jsx-text-expandShortMath
  jsx-text-generateTerm
  jsx-text-getAnchorX
  jsx-text-getAnchorY
  jsx-text-getNumberOfConflicts
  jsx-text-getSize
  jsx-text-hasPoint
  jsx-text-notifyParents
  jsx-text-poorMansTeX
  jsx-text-replaceSub
  jsx-text-replaceSup
  jsx-text-setAutoPosition
  jsx-text-setCoords
  jsx-text-setText
  jsx-text-setTextJessieCode
  jsx-text-unescapeTicks
  jsx-text-updateSize
  jsx-text-updateText
  jsx-text-utf8_decode
  jsx-text-valueTagToJessieCode
  jsx-image-H
  jsx-image-W
  jsx-image-has-point?
  jsx-image-set-size!
  jsx-image-update!
  jsx-image-update-renderer!
  jsx-image-update-size!
  jsx-image-update-span!
  jsx-group-add-parents!
  jsx-group-add-point!
  jsx-group-add-points!
  jsx-group-add-rotation-point!
  jsx-group-add-scale-point!
  jsx-group-add-translation-point!
  jsx-group-set-scale-center!
  jsx-group-update!
  jsx-board-count-children
  jsx-board-num-objects
  jsx-board-objects-list
  jsx-board-id
  jsx-board-container
  jsx-board-renderer
  jsx-board-canvas-width
  jsx-board-canvas-height
  jsx-board-bounding-box
  jsx-board-add-grid!
  jsx-board-add-hook!
  jsx-board-add-keyboard-event-handlers!
  jsx-board-add-mouse-event-handlers!
  jsx-board-add-pointer-event-handlers!
  jsx-board-add-resize-event-handlers!
  jsx-board-add-touch-event-handlers!
  jsx-board-add-wheel-event-handlers!
  jsx-board-add-fullscreen-event-handlers!
  jsx-board-get-mouse-position
  jsx-board-get-usr-coords-of-mouse
  jsx-board-get-coords-top-left-corner
  jsx-board-get-all-objects-under-mouse
  jsx-board-get-all-under-mouse
  jsx-board-set-attribute!
  jsx-board-set-bounding-box!
  jsx-board-set-zoom!
  jsx-board-resize-container!
  jsx-board-remove-grids!
  jsx-board-remove-hook!
  jsx-board-remove-keyboard-event-handlers!
  jsx-board-remove-mouse-event-handlers!
  jsx-board-remove-pointer-event-handlers!
  jsx-board-remove-resize-event-handlers!
  jsx-board-remove-touch-event-handlers!
  jsx-board-select
  jsx-board-zoom100!
  jsx-board-zoom-all-points!
  jsx-board-zoom-in!
  jsx-board-zoom-out!
  jsx-board-start-selection-mode!
  jsx-board-stop-selection-mode!
  jsx-board-stop-all-animation!
  jsx-board-clear-traces!
  jsx-board-dehighlight-all!
  jsx-board-update-coords!
  jsx-board-update-csstransforms!
  jsx-board-update-elements!
  jsx-board-update-hooks!
  jsx-board-suppress-default!
  jsx-board-init-infobox!
  jsx-board-init-move-object!
  jsx-board-init-move-origin!
  jsx-board-highlight-custom-infobox!
  jsx-board-highlight-infobox!
  jsx-board-move-object!
  jsx-board-show-dependencies!
  jsx-board-show-xml!
  jsx-board-to-fullscreen!
  jsx-board-start-resize-observer!
  jsx-board-stop-resize-observer!
  jsx-board-start-intersection-observer!
  jsx-board-stop-intersection-observer!
  jsx-board-has-point?
  jsx-board-move-origin!
  jsx-board-set-id
  jsx-board-update-renderer!
  jsx-board-update-renderer-canvas!
  jsx-board-update-infobox!
  jsx-board-zoom-elements!
  jsx-line-direction
  jsx-line-get-angle
  jsx-line-get-rise
  jsx-line-get-slope
  jsx-line-horizontal?
  jsx-line-vertical?
  jsx-line-l
  jsx-line-slope
  jsx-line-set-fixed-length!
  jsx-line-x
  jsx-line-y
  jsx-line-z
  jsx-arc-get-radius
  jsx-arc-has-point-sector?
  jsx-arc-radius
  jsx-arc-value
  jsx-angle-free?
  jsx-angle-set-angle!
  jsx-angle-value
  jsx-sector-area
  jsx-sector-has-point-sector?
  jsx-sector-l
  jsx-sector-perimeter
  jsx-sector-radius
  jsx-sector-set-position-directly!
  jsx-sector-set-radius!
  jsx-glider-start-animation!
  jsx-glider-stop-animation!
  jsx-circle-area
  jsx-circle-bounds
  jsx-circle-diameter
  jsx-circle-get-radius
  jsx-circle-perimeter
  jsx-circle-radius!
  jsx-circle-set-radius!
  jsx-circle-update-quadraticform!
  jsx-circle-update-renderer!
  jsx-circle-update-stdform!
  jsx-circle-x
  jsx-circle-y
  jsx-circle-z
  jsx-curve-allocate-points!
  jsx-curve-generate-term
  jsx-curve-get-label-position
  jsx-curve-get-transformation-source
  jsx-curve-has-point?
  jsx-curve-interpolation-function-from-array
  jsx-curve-max-x
  jsx-curve-min-x
  jsx-curve-move-to!
  jsx-curve-notify-parents!
  jsx-curve-update-curve!
  jsx-curve-update-data-array!
  jsx-curve-update-renderer!
  jsx-curve-update-transform!
  jsx-polygon-add-points!
  jsx-polygon-area
  jsx-polygon-bounding-box
  jsx-polygon-find-point
  jsx-polygon-has-point?
  jsx-polygon-hide-element!
  jsx-polygon-insert-points!
  jsx-polygon-intersect
  jsx-polygon-l
  jsx-polygon-perimeter
  jsx-polygon-pnpoly
  jsx-polygon-remove-points!
  jsx-polygon-set-position-directly!
  jsx-polygon-show-element!
  jsx-polygon-update-renderer!
  jsx-coordinates
  jsx-on
  jsx-element-add-event!
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
(define jsx-element-get-attribute any/c)
(define jsx-element-get-attributes any/c)
(define jsx-element-get-label-anchor any/c)
(define jsx-element-get-name any/c)
(define jsx-element-get-parents any/c)
(define jsx-element-get-property any/c)
(define jsx-element-get-snap-sizes any/c)
(define jsx-element-get-text-anchor any/c)
(define jsx-element-get-type any/c)
(define jsx-element-has-point? any/c)
(define jsx-element-hide! any/c)
(define jsx-element-hide-element! any/c)
(define jsx-element-no-highlight! any/c)
(define jsx-element-prepare-update! any/c)
(define jsx-element-remove! any/c)
(define jsx-element-remove-all-ticks! any/c)
(define jsx-element-remove-child! any/c)
(define jsx-element-remove-descendants! any/c)
(define jsx-element-remove-event! any/c)
(define jsx-element-remove-ticks! any/c)
(define jsx-element-set-attribute! any/c)
(define jsx-element-set-label! any/c)
(define jsx-element-set-label-text! any/c)
(define jsx-element-set-name! any/c)
(define jsx-element-set-parents! any/c)
(define jsx-element-set-position! any/c)
(define jsx-element-set-position-directly! any/c)
(define jsx-element-set-property! any/c)
(define jsx-element-show! any/c)
(define jsx-element-show-element! any/c)
(define jsx-element-update! any/c)
(define jsx-element-update-renderer! any/c)
(define jsx-element-update-visibility! any/c)
(define jsx-element-use-locale! any/c)
(define jsx-element-add-child! any/c)
(define jsx-element-add-descendants! any/c)
(define jsx-element-add-parents! any/c)
(define jsx-element-add-parents-from-jc-functions! any/c)
(define jsx-element-add-rotation! any/c)
(define jsx-element-add-ticks! any/c)
(define jsx-element-add-transform! any/c)
(define jsx-element-animate! any/c)
(define jsx-element-bounds any/c)
(define jsx-element-clear-trace! any/c)
(define jsx-element-clone-to-background! any/c)
(define jsx-element-count-children any/c)
(define jsx-element-create-gradient! any/c)
(define jsx-element-create-label! any/c)
(define jsx-element-draggable? any/c)
(define jsx-element-eval any/c)
(define jsx-element-eval-vis-prop any/c)
(define jsx-element-format-number-locale any/c)
(define jsx-element-full-update! any/c)
(define jsx-element-generate-polynomial any/c)
(define jsx-element-handle-snap-to-grid! any/c)
(define jsx-element-normalize! any/c)
(define jsx-element-resolve-shortcuts! any/c)
(define jsx-element-set-arrow! any/c)
(define jsx-element-set-dash! any/c)
(define jsx-element-set-display-rend-node! any/c)
(define jsx-element-snap-to-points! any/c)
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
(define jsx-create-view3d any/c)
(define jsx-view3d-create any/c)
(define jsx-view3d-create-point3d any/c)
(define jsx-view3d-create-line3d any/c)
(define jsx-view3d-create-circle3d any/c)
(define jsx-view3d-create-plane3d any/c)
(define jsx-view3d-create-axes3d any/c)
(define jsx-view3d-create-axis3d any/c)
(define jsx-view3d-create-functiongraph3d any/c)
(define jsx-view3d-create-sphere3d any/c)
(define jsx-view3d-create-face3d any/c)
(define jsx-view3d-create-surface3d any/c)
(define jsx-view3d-create-intersectioncircle3d any/c)
(define jsx-view3d-create-intersectionline3d any/c)
(define jsx-view3d-create-curve3d any/c)
(define jsx-view3d-create-parametricsurface3d any/c)
(define jsx-view3d-create-polyhedron3d any/c)
(define jsx-view3d-create-text3d any/c)
(define jsx-view3d-create-ticks3d any/c)
(define jsx-view3d-create-transformation3d any/c)
(define jsx-view3d-create-vectorfield3d any/c)
(define jsx-create-point any/c)
(define jsx-create-line any/c)
(define jsx-create-segment any/c)
(define jsx-create-arc any/c)
(define jsx-create-angle any/c)
(define jsx-create-sector any/c)
(define jsx-create-glider any/c)
(define jsx-create-circle any/c)
(define jsx-create-conic any/c)
(define jsx-create-ellipse any/c)
(define jsx-create-curve any/c)
(define jsx-create-functiongraph any/c)
(define jsx-create-polygon any/c)
(define jsx-create-midpoint any/c)
(define jsx-create-parallel any/c)
(define jsx-create-arrowparallel any/c)
(define jsx-create-axis any/c)
(define jsx-create-grid any/c)
(define jsx-create-boxplot any/c)
(define jsx-create-tangent any/c)
(define jsx-create-tangentto any/c)
(define jsx-create-polarline any/c)
(define jsx-create-polepoint any/c)
(define jsx-create-radicalaxis any/c)
(define jsx-create-circumcircle any/c)
(define jsx-create-incircle any/c)
(define jsx-create-incenter any/c)
(define jsx-create-minorarc any/c)
(define jsx-create-minorsector any/c)
(define jsx-create-circumcirclearc any/c)
(define jsx-create-circumcirclesector any/c)
(define jsx-create-semicircle any/c)
(define jsx-create-majorarc any/c)
(define jsx-create-majorsector any/c)
(define jsx-create-curveintersection any/c)
(define jsx-create-curvedifference any/c)
(define jsx-create-curveunion any/c)
(define jsx-create-derivative any/c)
(define jsx-create-integral any/c)
(define jsx-create-riemannsum any/c)
(define jsx-riemannsum-value any/c)
(define jsx-create-slopefield any/c)
(define jsx-slopefield-set-f! any/c)
(define jsx-create-vectorfield any/c)
(define jsx-vectorfield-set-f! any/c)
(define jsx-create-implicitcurve any/c)
(define jsx-create-spline any/c)
(define jsx-create-cardinalspline any/c)
(define jsx-create-comb any/c)
(define jsx-create-metapostspline any/c)
(define jsx-create-polygonalchain any/c)
(define jsx-create-regularpolygon any/c)
(define jsx-create-hyperbola any/c)
(define jsx-create-parabola any/c)
(define jsx-create-stepfunction any/c)
(define jsx-create-inequality any/c)
(define jsx-create-turtle any/c)
(define jsx-create-nonreflexangle any/c)
(define jsx-create-perpendicular any/c)
(define jsx-create-reflection any/c)
(define jsx-create-bisector any/c)
(define jsx-create-normal any/c)
(define jsx-create-intersection any/c)
(define jsx-create-arrow any/c)
(define jsx-create-button any/c)
(define jsx-create-checkbox any/c)
(define jsx-create-input any/c)
(define jsx-create-slider any/c)
(define jsx-create-chart any/c)
(define jsx-create-legend any/c)
(define jsx-create-smartlabel any/c)
(define jsx-create-text any/c)
(define jsx-create-image any/c)
(define jsx-create-foreignobject any/c)
(define jsx-create-tapemeasure any/c)
(define jsx-create-hatch any/c)
(define jsx-create-ticks any/c)
(define jsx-create-transformation any/c)
(define jsx-create-tracecurve any/c)
(define jsx-create-slopetriangle any/c)
(define jsx-create-parallelogram any/c)
(define jsx-create-reflexangle any/c)
(define jsx-create-measurement any/c)
(define jsx-create-bisectorlines any/c)
(define jsx-create-perpendicularsegment any/c)
(define jsx-create-circumcenter any/c)
(define jsx-create-mirrorelement any/c)
(define jsx-create-mirrorpoint any/c)
(define jsx-create-otherintersection any/c)
(define jsx-create-orthogonalprojection any/c)
(define jsx-create-parallelpoint any/c)
(define jsx-create-perpendicularpoint any/c)
(define jsx-create-group any/c)
(define jsx-checkbox-value any/c)
(define jsx-input-set! any/c)
(define jsx-input-value any/c)
(define jsx-slider-set-max! any/c)
(define jsx-slider-set-min! any/c)
(define jsx-slider-set-value! any/c)
(define jsx-slider-value any/c)
(define jsx-chart-draw-bar any/c)
(define jsx-chart-draw-fit any/c)
(define jsx-chart-draw-line any/c)
(define jsx-chart-draw-pie any/c)
(define jsx-chart-draw-points any/c)
(define jsx-chart-draw-radar any/c)
(define jsx-chart-draw-spline any/c)
(define jsx-chart-update-data-array! any/c)
(define jsx-chart-update-renderer! any/c)
(define jsx-foreignobject-H any/c)
(define jsx-foreignobject-W any/c)
(define jsx-foreignobject-has-point? any/c)
(define jsx-foreignobject-set-size! any/c)
(define jsx-foreignobject-update! any/c)
(define jsx-foreignobject-update-renderer! any/c)
(define jsx-foreignobject-update-size! any/c)
(define jsx-foreignobject-update-span! any/c)
(define jsx-tapemeasure-value any/c)
(define jsx-text-_createFctUpdateText any/c)
(define jsx-text-_setText any/c)
(define jsx-text-bounds any/c)
(define jsx-text-checkForSizeUpdate any/c)
(define jsx-text-convertGeonext2CSS any/c)
(define jsx-text-convertGeonextAndSketchometry2CSS any/c)
(define jsx-text-convertSketchometry2CSS any/c)
(define jsx-text-crudeSizeEstimate any/c)
(define jsx-text-escapeTicks any/c)
(define jsx-text-expandShortMath any/c)
(define jsx-text-generateTerm any/c)
(define jsx-text-getAnchorX any/c)
(define jsx-text-getAnchorY any/c)
(define jsx-text-getNumberOfConflicts any/c)
(define jsx-text-getSize any/c)
(define jsx-text-hasPoint any/c)
(define jsx-text-notifyParents any/c)
(define jsx-text-poorMansTeX any/c)
(define jsx-text-replaceSub any/c)
(define jsx-text-replaceSup any/c)
(define jsx-text-setAutoPosition any/c)
(define jsx-text-setCoords any/c)
(define jsx-text-setText any/c)
(define jsx-text-setTextJessieCode any/c)
(define jsx-text-unescapeTicks any/c)
(define jsx-text-updateSize any/c)
(define jsx-text-updateText any/c)
(define jsx-text-utf8_decode any/c)
(define jsx-text-valueTagToJessieCode any/c)
(define jsx-image-H any/c)
(define jsx-image-W any/c)
(define jsx-image-has-point? any/c)
(define jsx-image-set-size! any/c)
(define jsx-image-update! any/c)
(define jsx-image-update-renderer! any/c)
(define jsx-image-update-size! any/c)
(define jsx-image-update-span! any/c)
(define jsx-group-add-parents! any/c)
(define jsx-group-add-point! any/c)
(define jsx-group-add-points! any/c)
(define jsx-group-add-rotation-point! any/c)
(define jsx-group-add-scale-point! any/c)
(define jsx-group-add-translation-point! any/c)
(define jsx-group-set-scale-center! any/c)
(define jsx-group-update! any/c)
(define jsx-group-set-rotation-center! any/c)
(define jsx-group-set-rotation-points! any/c)
(define jsx-group-set-translation-points! any/c)
(define jsx-board-count-children any/c)
(define jsx-board-num-objects any/c)
(define jsx-board-objects-list any/c)
(define jsx-board-id any/c)
(define jsx-board-container any/c)
(define jsx-board-renderer any/c)
(define jsx-board-canvas-width any/c)
(define jsx-board-canvas-height any/c)
(define jsx-board-bounding-box any/c)
(define jsx-board-add-grid! any/c)
(define jsx-board-add-hook! any/c)
(define jsx-board-add-keyboard-event-handlers! any/c)
(define jsx-board-add-mouse-event-handlers! any/c)
(define jsx-board-add-pointer-event-handlers! any/c)
(define jsx-board-add-resize-event-handlers! any/c)
(define jsx-board-add-touch-event-handlers! any/c)
(define jsx-board-add-wheel-event-handlers! any/c)
(define jsx-board-add-event-handlers! any/c)
(define jsx-board-add-fullscreen-event-handlers! any/c)
(define jsx-board-add-child! any/c)
(define jsx-board-calculate-snap-sizes! any/c)
(define jsx-board-add-event! any/c)
(define jsx-board-get-mouse-position any/c)
(define jsx-board-get-usr-coords-of-mouse any/c)
(define jsx-board-get-coords-top-left-corner any/c)
(define jsx-board-get-bounding-box any/c)
(define jsx-board-get-scr-coords-of-mouse any/c)
(define jsx-board-get-all-objects-under-mouse any/c)
(define jsx-board-get-all-under-mouse any/c)
(define jsx-board-set-attribute! any/c)
(define jsx-board-set-bounding-box! any/c)
(define jsx-board-set-zoom! any/c)
(define jsx-board-resize-container! any/c)
(define jsx-board-remove-grids! any/c)
(define jsx-board-remove-hook! any/c)
(define jsx-board-remove-event! any/c)
(define jsx-board-remove-keyboard-event-handlers! any/c)
(define jsx-board-remove-mouse-event-handlers! any/c)
(define jsx-board-remove-pointer-event-handlers! any/c)
(define jsx-board-remove-resize-event-handlers! any/c)
(define jsx-board-remove-touch-event-handlers! any/c)
(define jsx-board-remove-event-handlers! any/c)
(define jsx-board-select any/c)
(define jsx-board-zoom100! any/c)
(define jsx-board-zoom-all-points! any/c)
(define jsx-board-zoom-in! any/c)
(define jsx-board-zoom-out! any/c)
(define jsx-board-start-selection-mode! any/c)
(define jsx-board-stop-selection-mode! any/c)
(define jsx-board-stop-all-animation! any/c)
(define jsx-board-clear-traces! any/c)
(define jsx-board-dehighlight-all! any/c)
(define jsx-board-update-coords! any/c)
(define jsx-board-update-container-dims! any/c)
(define jsx-board-update-csstransforms! any/c)
(define jsx-board-update-elements! any/c)
(define jsx-board-update-hooks! any/c)
(define jsx-board-update-conditions! any/c)
(define jsx-board-suppress-default! any/c)
(define jsx-board-init-infobox! any/c)
(define jsx-board-init-move-object! any/c)
(define jsx-board-init-move-origin! any/c)
(define jsx-board-highlight-custom-infobox! any/c)
(define jsx-board-highlight-infobox! any/c)
(define jsx-board-move-object! any/c)
(define jsx-board-show-dependencies! any/c)
(define jsx-board-create-roulette! any/c)
(define jsx-board-show-xml! any/c)
(define jsx-board-to-fullscreen! any/c)
(define jsx-board-start-resize-observer! any/c)
(define jsx-board-stop-resize-observer! any/c)
(define jsx-board-start-intersection-observer! any/c)
(define jsx-board-stop-intersection-observer! any/c)
(define jsx-board-has-point? any/c)
(define jsx-board-move-origin! any/c)
(define jsx-board-set-id any/c)
(define jsx-board-update-renderer! any/c)
(define jsx-board-update-renderer-canvas! any/c)
(define jsx-board-update-infobox! any/c)
(define jsx-board-zoom-elements! any/c)
(define jsx-line-direction any/c)
(define jsx-line-get-angle any/c)
(define jsx-line-get-rise any/c)
(define jsx-line-get-slope any/c)
(define jsx-line-horizontal? any/c)
(define jsx-line-vertical? any/c)
(define jsx-line-l any/c)
(define jsx-line-slope any/c)
(define jsx-line-set-fixed-length! any/c)
(define jsx-line-x any/c)
(define jsx-line-y any/c)
(define jsx-line-z any/c)
(define jsx-arc-get-radius any/c)
(define jsx-arc-has-point-sector? any/c)
(define jsx-arc-radius any/c)
(define jsx-arc-value any/c)
(define jsx-angle-free? any/c)
(define jsx-angle-set-angle! any/c)
(define jsx-angle-value any/c)
(define jsx-sector-area any/c)
(define jsx-sector-has-point-sector? any/c)
(define jsx-sector-l any/c)
(define jsx-sector-perimeter any/c)
(define jsx-sector-radius any/c)
(define jsx-sector-set-position-directly! any/c)
(define jsx-sector-set-radius! any/c)
(define jsx-glider-start-animation! any/c)
(define jsx-glider-stop-animation! any/c)
(define jsx-circle-area any/c)
(define jsx-circle-bounds any/c)
(define jsx-circle-diameter any/c)
(define jsx-circle-get-radius any/c)
(define jsx-circle-perimeter any/c)
(define jsx-circle-radius! any/c)
(define jsx-circle-set-radius! any/c)
(define jsx-circle-update-quadraticform! any/c)
(define jsx-circle-update-renderer! any/c)
(define jsx-circle-update-stdform! any/c)
(define jsx-circle-x any/c)
(define jsx-circle-y any/c)
(define jsx-circle-z any/c)
(define jsx-curve-allocate-points! any/c)
(define jsx-curve-generate-term any/c)
(define jsx-curve-get-label-position any/c)
(define jsx-curve-get-transformation-source any/c)
(define jsx-curve-has-point? any/c)
(define jsx-curve-interpolation-function-from-array any/c)
(define jsx-curve-max-x any/c)
(define jsx-curve-min-x any/c)
(define jsx-curve-move-to! any/c)
(define jsx-curve-notify-parents! any/c)
(define jsx-curve-update-curve! any/c)
(define jsx-curve-update-data-array! any/c)
(define jsx-curve-update-renderer! any/c)
(define jsx-curve-update-transform! any/c)
(define jsx-polygon-add-points! any/c)
(define jsx-polygon-area any/c)
(define jsx-polygon-bounding-box any/c)
(define jsx-polygon-find-point any/c)
(define jsx-polygon-has-point? any/c)
(define jsx-polygon-hide-element! any/c)
(define jsx-polygon-insert-points! any/c)
(define jsx-polygon-intersect any/c)
(define jsx-polygon-l any/c)
(define jsx-polygon-perimeter any/c)
(define jsx-polygon-pnpoly any/c)
(define jsx-polygon-remove-points! any/c)
(define jsx-polygon-set-position-directly! any/c)
(define jsx-polygon-show-element! any/c)
(define jsx-polygon-update-renderer! any/c)
(define jsx-coordinates any/c)
(define jsx-on any/c)
(define jsx-element-add-event! any/c)
