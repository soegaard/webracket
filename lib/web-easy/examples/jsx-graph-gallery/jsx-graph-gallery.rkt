;;;
;;; JSXGraph Gallery
;;;

(include-lib web-easy)
(include-lib event)
(include-lib document)
(include-lib jsx-graph)
(include-lib console)

(define (gallery-headline title)
  (h2 title #:style "margin: 1.8rem 0 0.6rem; font-size: 1.35rem;"))

(define geometry-board-id "jsx-graph-gallery-geometry")
(define group-board-id "jsx-graph-gallery-group")
(define chart-board-id "jsx-graph-gallery-chart")
(define point-board-id "jsx-graph-gallery-point")
(define line-board-id "jsx-graph-gallery-line")
(define arc-board-id "jsx-graph-gallery-arc")
(define angle-board-id "jsx-graph-gallery-angle")
(define sector-board-id "jsx-graph-gallery-sector")
(define arrowparallel-board-id "jsx-graph-gallery-arrowparallel")
(define axis-board-id "jsx-graph-gallery-axis")
(define segment-board-id "jsx-graph-gallery-segment")
(define intersection-board-id "jsx-graph-gallery-intersection")
(define orthogonal-board-id "jsx-graph-gallery-normal")
(define grid-board-id "jsx-graph-gallery-grid")
(define boxplot-board-id "jsx-graph-gallery-boxplot")
(define tangent-board-id "jsx-graph-gallery-tangent")
(define tangentto-board-id "jsx-graph-gallery-tangentto")
(define polarline-board-id "jsx-graph-gallery-polarline")
(define polepoint-board-id "jsx-graph-gallery-polepoint")
(define radicalaxis-board-id "jsx-graph-gallery-radicalaxis")
(define circumcircle-board-id "jsx-graph-gallery-circumcircle")
(define circumcirclearc-board-id "jsx-graph-gallery-circumcirclearc")
(define circumcirclesector-board-id "jsx-graph-gallery-circumcirclesector")
(define semicircle-board-id "jsx-graph-gallery-semicircle")
(define majorarc-board-id "jsx-graph-gallery-majorarc")
(define majorsector-board-id "jsx-graph-gallery-majorsector")
(define curveintersection-board-id "jsx-graph-gallery-curveintersection")
(define curvedifference-board-id "jsx-graph-gallery-curvedifference")
(define curveunion-board-id "jsx-graph-gallery-curveunion")
(define derivative-board-id "jsx-graph-gallery-derivative")
(define integral-board-id "jsx-graph-gallery-integral")
(define riemannsum-board-id "jsx-graph-gallery-riemannsum")
(define slopefield-board-id "jsx-graph-gallery-slopefield")
(define vectorfield-board-id "jsx-graph-gallery-vectorfield")
(define implicitcurve-board-id "jsx-graph-gallery-implicitcurve")
(define spline-board-id "jsx-graph-gallery-spline")
(define cardinalspline-board-id "jsx-graph-gallery-cardinalspline")
(define comb-board-id "jsx-graph-gallery-comb")
(define metapostspline-board-id "jsx-graph-gallery-metapostspline")
(define polygonalchain-board-id "jsx-graph-gallery-polygonalchain")
(define regularpolygon-board-id "jsx-graph-gallery-regularpolygon")
(define hyperbola-board-id "jsx-graph-gallery-hyperbola")
(define parabola-board-id "jsx-graph-gallery-parabola")
(define stepfunction-board-id "jsx-graph-gallery-stepfunction")
(define inequality-board-id "jsx-graph-gallery-inequality")
(define turtle-board-id "jsx-graph-gallery-turtle")
(define incircle-board-id "jsx-graph-gallery-incircle")
(define conic-board-id "jsx-graph-gallery-conic")
(define ellipse-board-id "jsx-graph-gallery-ellipse")
(define functiongraph-board-id "jsx-graph-gallery-functiongraph")
(define curve-board-id "jsx-graph-gallery-curve")
(define polygon-board-id "jsx-graph-gallery-polygon")
(define arrow-board-id "jsx-graph-gallery-arrow")
(define circle-board-id "jsx-graph-gallery-circle")
(define glider-board-id "jsx-graph-gallery-glider")
(define button-board-id "jsx-graph-gallery-button")
(define legend-board-id "jsx-graph-gallery-legend")
(define midpoint-board-id "jsx-graph-gallery-midpoint")
(define parallel-board-id "jsx-graph-gallery-parallel")
(define perpendicular-board-id "jsx-graph-gallery-perpendicular")
(define reflection-board-id "jsx-graph-gallery-reflection")
(define bisector-board-id "jsx-graph-gallery-bisector")
(define checkbox-board-id "jsx-graph-gallery-checkbox")
(define input-board-id "jsx-graph-gallery-input")
(define slider-board-id "jsx-graph-gallery-slider")
(define smartlabel-board-id "jsx-graph-gallery-smartlabel")
(define text-board-id "jsx-graph-gallery-text")
(define foreignobject-board-id "jsx-graph-gallery-foreignobject")
(define tapemeasure-board-id "jsx-graph-gallery-tapemeasure")
(define ticks-board-id "jsx-graph-gallery-ticks")
(define transformation-board-id "jsx-graph-gallery-transformation")
(define tracecurve-board-id "jsx-graph-gallery-tracecurve")
(define parallelogram-board-id "jsx-graph-gallery-parallelogram")
(define reflexangle-board-id "jsx-graph-gallery-reflexangle")
(define bisectorlines-board-id "jsx-graph-gallery-bisectorlines")
(define perpendicularsegment-board-id "jsx-graph-gallery-perpendicularsegment")
(define measurement-board-id "jsx-graph-gallery-measurement")
(define circumcenter-board-id "jsx-graph-gallery-circumcenter")
(define mirrorelement-board-id "jsx-graph-gallery-mirrorelement")
(define mirrorpoint-board-id "jsx-graph-gallery-mirrorpoint")
(define otherintersection-board-id "jsx-graph-gallery-otherintersection")
(define orthogonalprojection-board-id "jsx-graph-gallery-orthogonalprojection")
(define parallelpoint-board-id "jsx-graph-gallery-parallelpoint")
(define perpendicularpoint-board-id "jsx-graph-gallery-perpendicularpoint")
(define constructions-board-id "jsx-graph-gallery-constructions")
(define widgets-board-id "jsx-graph-gallery-widgets")
(define annotation-board-id "jsx-graph-gallery-annotation")
(define view3d-board-id "jsx-graph-gallery-view3d")
(define point3d-board-id "jsx-graph-gallery-point3d")
(define line3d-board-id "jsx-graph-gallery-line3d")
(define circle3d-board-id "jsx-graph-gallery-circle3d")
(define plane3d-board-id "jsx-graph-gallery-plane3d")
(define axes3d-board-id "jsx-graph-gallery-axes3d")
(define axis3d-board-id "jsx-graph-gallery-axis3d")
(define functiongraph3d-board-id "jsx-graph-gallery-functiongraph3d")
(define intersectioncircle3d-board-id "jsx-graph-gallery-intersectioncircle3d")
(define intersectionline3d-board-id "jsx-graph-gallery-intersectionline3d")
(define curve3d-board-id "jsx-graph-gallery-curve3d")
(define parametricsurface3d-board-id "jsx-graph-gallery-parametricsurface3d")
(define polyhedron3d-board-id "jsx-graph-gallery-polyhedron3d")
(define text3d-board-id "jsx-graph-gallery-text3d")
(define sphere3d-board-id "jsx-graph-gallery-sphere3d")
(define surface3d-board-id "jsx-graph-gallery-surface3d")
(define face3d-board-id "jsx-graph-gallery-face3d")
(define ticks3d-board-id "jsx-graph-gallery-ticks3d")
(define transformation3d-board-id "jsx-graph-gallery-transformation3d")
(define vectorfield3d-board-id "jsx-graph-gallery-vectorfield3d")

(define @status (@ "Loading JSXGraph assets..."))
(define @summary (@ "Waiting for the boards to initialize."))

(define geometry-board #f)
(define geometry-p #f)
(define geometry-q #f)
(define geometry-arrow #f)
(define geometry-slider #f)
(define geometry-label #f)

(define group-board #f)
(define group-p #f)
(define group-q #f)
(define group-group #f)

(define chart-board #f)
(define chart #f)
(define chart-legend #f)

(define geometry-board-ready? #f)
(define group-board-ready? #f)
(define chart-board-ready? #f)
(define point-board-ready? #f)
(define line-board-ready? #f)
(define arc-board-ready? #f)
(define angle-board-ready? #f)
(define sector-board-ready? #f)
(define arrowparallel-board-ready? #f)
(define axis-board-ready? #f)
(define segment-board-ready? #f)
(define intersection-board-ready? #f)
(define orthogonal-board-ready? #f)
(define grid-board-ready? #f)
(define boxplot-board-ready? #f)
(define tangent-board-ready? #f)
(define tangentto-board-ready? #f)
(define polarline-board-ready? #f)
(define polepoint-board-ready? #f)
(define radicalaxis-board-ready? #f)
(define circumcircle-board-ready? #f)
(define circumcirclearc-board-ready? #f)
(define circumcirclesector-board-ready? #f)
(define semicircle-board-ready? #f)
(define majorarc-board-ready? #f)
(define majorsector-board-ready? #f)
(define curveintersection-board-ready? #f)
(define curvedifference-board-ready? #f)
(define curveunion-board-ready? #f)
(define derivative-board-ready? #f)
(define integral-board-ready? #f)
(define riemannsum-board-ready? #f)
(define slopefield-board-ready? #f)
(define vectorfield-board-ready? #f)
(define implicitcurve-board-ready? #f)
(define spline-board-ready? #f)
(define cardinalspline-board-ready? #f)
(define comb-board-ready? #f)
(define metapostspline-board-ready? #f)
(define polygonalchain-board-ready? #f)
(define regularpolygon-board-ready? #f)
(define hyperbola-board-ready? #f)
(define parabola-board-ready? #f)
(define stepfunction-board-ready? #f)
(define inequality-board-ready? #f)
(define turtle-board-ready? #f)
(define incircle-board-ready? #f)
(define conic-board-ready? #f)
(define ellipse-board-ready? #f)
(define functiongraph-board-ready? #f)
(define curve-board-ready? #f)
(define polygon-board-ready? #f)
(define arrow-board-ready? #f)
(define circle-board-ready? #f)
(define glider-board-ready? #f)
(define button-board-ready? #f)
(define legend-board-ready? #f)
(define midpoint-board-ready? #f)
(define parallel-board-ready? #f)
(define perpendicular-board-ready? #f)
(define reflection-board-ready? #f)
(define bisector-board-ready? #f)
(define checkbox-board-ready? #f)
(define input-board-ready? #f)
(define slider-board-ready? #f)
(define smartlabel-board-ready? #f)
(define text-board-ready? #f)
(define foreignobject-board-ready? #f)
(define tapemeasure-board-ready? #f)
(define ticks-board-ready? #f)
(define transformation-board-ready? #f)
(define tracecurve-board-ready? #f)
(define parallelogram-board-ready? #f)
(define reflexangle-board-ready? #f)
(define bisectorlines-board-ready? #f)
(define perpendicularsegment-board-ready? #f)
(define measurement-board-ready? #f)
(define circumcenter-board-ready? #f)
(define mirrorelement-board-ready? #f)
(define mirrorpoint-board-ready? #f)
(define otherintersection-board-ready? #f)
(define orthogonalprojection-board-ready? #f)
(define parallelpoint-board-ready? #f)
(define perpendicularpoint-board-ready? #f)
(define constructions-board-ready? #f)
(define widgets-board-ready? #f)
(define annotation-board-ready? #f)
(define view3d-board-ready? #f)
(define point3d-board-ready? #f)
(define line3d-board-ready? #f)
(define circle3d-board-ready? #f)
(define plane3d-board-ready? #f)
(define axes3d-board-ready? #f)
(define axis3d-board-ready? #f)
(define functiongraph3d-board-ready? #f)
(define intersectioncircle3d-board-ready? #f)
(define intersectionline3d-board-ready? #f)
(define curve3d-board-ready? #f)
(define parametricsurface3d-board-ready? #f)
(define polyhedron3d-board-ready? #f)
(define text3d-board-ready? #f)
(define sphere3d-board-ready? #f)
(define surface3d-board-ready? #f)
(define face3d-board-ready? #f)
(define ticks3d-board-ready? #f)
(define transformation3d-board-ready? #f)
(define vectorfield3d-board-ready? #f)

(define point-board #f)
(define point-free #f)
(define point-fixed #f)
(define point-restricted #f)
(define point-base-line #f)
(define line-board #f)
(define line-object #f)
(define arc-board #f)
(define arc-p1 #f)
(define arc-p2 #f)
(define arc-p3 #f)
(define arc-leg-1 #f)
(define arc-leg-2 #f)
(define arc-object #f)
(define angle-board #f)
(define angle-leg-1 #f)
(define angle-leg-2 #f)
(define angle-object #f)
(define sector-board #f)
(define sector-object #f)
(define arrowparallel-board #f)
(define arrowparallel-p1 #f)
(define arrowparallel-p2 #f)
(define arrowparallel-p3 #f)
(define arrowparallel-segment #f)
(define arrowparallel-object #f)
(define axis-board #f)
(define axis-x #f)
(define axis-y #f)
(define segment-board #f)
(define segment-a #f)
(define segment-b #f)
(define segment-object #f)
(define intersection-board #f)
(define intersection-a #f)
(define intersection-b #f)
(define intersection-c #f)
(define intersection-d #f)
(define intersection-line-1 #f)
(define intersection-line-2 #f)
(define intersection-point #f)
(define orthogonal-board #f)
(define orthogonal-curve #f)
(define orthogonal-point #f)
(define orthogonal-object #f)
(define grid-board #f)
(define grid-object #f)
(define grid-mode "standard")
(define boxplot-board #f)
(define boxplot-q #f)
(define boxplot-object #f)
(define tangent-board #f)
(define tangent-curve #f)
(define tangent-glider #f)
(define tangent-line #f)
(define tangentto-board #f)
(define tangentto-circle #f)
(define tangentto-point #f)
(define tangentto-left #f)
(define tangentto-right #f)
(define polarline-board #f)
(define polarline-p1 #f)
(define polarline-p2 #f)
(define polarline-p3 #f)
(define polarline-p4 #f)
(define polarline-p5 #f)
(define polarline-p6 #f)
(define polarline-conic #f)
(define polarline-object #f)
(define polepoint-board #f)
(define polepoint-p1 #f)
(define polepoint-p2 #f)
(define polepoint-p3 #f)
(define polepoint-p4 #f)
(define polepoint-p5 #f)
(define polepoint-p6 #f)
(define polepoint-p7 #f)
(define polepoint-conic #f)
(define polepoint-line #f)
(define polepoint-object #f)
(define radicalaxis-board #f)
(define radicalaxis-p1 #f)
(define radicalaxis-p2 #f)
(define radicalaxis-p3 #f)
(define radicalaxis-p4 #f)
(define radicalaxis-circle-1 #f)
(define radicalaxis-circle-2 #f)
(define radicalaxis-object #f)
(define circumcircle-board #f)
(define circumcircle-p1 #f)
(define circumcircle-p2 #f)
(define circumcircle-p3 #f)
(define circumcircle-triangle #f)
(define circumcircle-object #f)
(define circumcirclearc-board #f)
(define circumcirclearc-p1 #f)
(define circumcirclearc-p2 #f)
(define circumcirclearc-p3 #f)
(define circumcirclearc-object #f)
(define circumcirclesector-board #f)
(define circumcirclesector-p1 #f)
(define circumcirclesector-p2 #f)
(define circumcirclesector-p3 #f)
(define circumcirclesector-object #f)
(define semicircle-board #f)
(define semicircle-p1 #f)
(define semicircle-p2 #f)
(define semicircle-object #f)
(define majorarc-board #f)
(define majorarc-p1 #f)
(define majorarc-p2 #f)
(define majorarc-p3 #f)
(define majorarc-object #f)
(define majorsector-board #f)
(define majorsector-p1 #f)
(define majorsector-p2 #f)
(define majorsector-p3 #f)
(define majorsector-object #f)
(define curveintersection-board #f)
(define curveintersection-f-1 #f)
(define curveintersection-f-2 #f)
(define curveintersection-object #f)
(define curvedifference-board #f)
(define curvedifference-f-1 #f)
(define curvedifference-f-2 #f)
(define curvedifference-object #f)
(define curveunion-board #f)
(define curveunion-f-1 #f)
(define curveunion-f-2 #f)
(define curveunion-object #f)
(define derivative-board #f)
(define derivative-curve #f)
(define derivative-object #f)
(define integral-board #f)
(define integral-curve #f)
(define integral-object #f)
(define riemannsum-board #f)
(define riemannsum-curve #f)
(define riemannsum-object #f)
(define riemannsum-value-text #f)
(define slopefield-board #f)
(define slopefield-object #f)
(define vectorfield-board #f)
(define vectorfield-object #f)
(define implicitcurve-board #f)
(define implicitcurve-object #f)
(define implicitcurve-text #f)
(define spline-board #f)
(define spline-p1 #f)
(define spline-p2 #f)
(define spline-p3 #f)
(define spline-p4 #f)
(define spline-object #f)
(define cardinalspline-board #f)
(define cardinalspline-object #f)
(define comb-board #f)
(define comb-object #f)
(define metapostspline-board #f)
(define metapostspline-object #f)
(define polygonalchain-board #f)
(define polygonalchain-object #f)
(define regularpolygon-board #f)
(define regularpolygon-object #f)
(define hyperbola-board #f)
(define hyperbola-a #f)
(define hyperbola-b #f)
(define hyperbola-c #f)
(define hyperbola-object #f)
(define parabola-board #f)
(define parabola-a #f)
(define parabola-b #f)
(define parabola-c #f)
(define parabola-object #f)
(define stepfunction-board #f)
(define stepfunction-object #f)
(define inequality-board #f)
(define inequality-p #f)
(define inequality-q #f)
(define inequality-line #f)
(define inequality-object #f)
(define turtle-board #f)
(define turtle-object #f)
(define incircle-board #f)
(define incircle-p1 #f)
(define incircle-p2 #f)
(define incircle-p3 #f)
(define incircle-triangle #f)
(define incircle-object #f)
(define conic-board #f)
(define conic-a #f)
(define conic-b #f)
(define conic-c #f)
(define conic-d #f)
(define conic-e #f)
(define conic-object #f)
(define ellipse-board #f)
(define ellipse-a #f)
(define ellipse-b #f)
(define ellipse-c #f)
(define ellipse-object #f)
(define functiongraph-board #f)
(define functiongraph-object #f)
(define curve-board #f)
(define curve-object #f)
(define polygon-board #f)
(define polygon-a #f)
(define polygon-b #f)
(define polygon-c #f)
(define polygon-d #f)
(define polygon-object #f)
(define arrow-board #f)
(define arrow-a #f)
(define arrow-b #f)
(define arrow-object #f)
(define circle-board #f)
(define circle-center #f)
(define circle-through #f)
(define circle-object #f)
(define glider-board #f)
(define glider-base-circle #f)
(define glider-center #f)
(define glider-through #f)
(define glider-point #f)
(define glider-object #f)
(define button-board #f)
(define button-object #f)
(define legend-board #f)
(define legend-chart #f)
(define legend-object #f)
(define midpoint-board #f)
(define midpoint-a #f)
(define midpoint-b #f)
(define midpoint-segment #f)
(define midpoint-object #f)
(define parallel-board #f)
(define parallel-a #f)
(define parallel-b #f)
(define parallel-c #f)
(define parallel-line #f)
(define parallel-object #f)
(define perpendicular-board #f)
(define perpendicular-a #f)
(define perpendicular-b #f)
(define perpendicular-c #f)
(define perpendicular-line #f)
(define perpendicular-object #f)
(define reflection-board #f)
(define reflection-a #f)
(define reflection-b #f)
(define reflection-c #f)
(define reflection-line #f)
(define reflection-segment #f)
(define reflection-object #f)
(define bisector-board #f)
(define bisector-a #f)
(define bisector-b #f)
(define bisector-c #f)
(define bisector-ab #f)
(define bisector-bc #f)
(define bisector-angle #f)
(define bisector-object #f)
(define checkbox-board #f)
(define checkbox-object #f)
(define checkbox-status #f)
(define input-board #f)
(define input-object #f)
(define input-graph #f)
(define input-expression "Math.sin(x)")
(define slider-board #f)
(define slider-object #f)
(define smartlabel-board #f)
(define smartlabel-point #f)
(define smartlabel-object #f)
(define text-board #f)
(define text-object #f)
(define foreignobject-board #f)
(define foreignobject-object #f)
(define tapemeasure-board #f)
(define tapemeasure-a #f)
(define tapemeasure-b #f)
(define tapemeasure-object #f)
(define measurement-board #f)
(define measurement-a #f)
(define measurement-b #f)
(define measurement-circle #f)
(define measurement-object #f)
(define circumcenter-board #f)
(define circumcenter-a #f)
(define circumcenter-b #f)
(define circumcenter-c #f)
(define circumcenter-triangle #f)
(define circumcenter-object #f)
(define mirrorelement-board #f)
(define mirrorelement-a #f)
(define mirrorelement-mirror #f)
(define mirrorelement-object #f)
(define mirrorpoint-board #f)
(define mirrorpoint-a #f)
(define mirrorpoint-b #f)
(define mirrorpoint-object #f)
(define otherintersection-board #f)
(define otherintersection-center-1 #f)
(define otherintersection-through-1 #f)
(define otherintersection-center-2 #f)
(define otherintersection-through-2 #f)
(define otherintersection-circle-1 #f)
(define otherintersection-circle-2 #f)
(define otherintersection-primary #f)
(define otherintersection-secondary #f)
(define orthogonalprojection-board #f)
(define orthogonalprojection-line-a #f)
(define orthogonalprojection-line-b #f)
(define orthogonalprojection-line #f)
(define orthogonalprojection-point #f)
(define orthogonalprojection-object #f)
(define parallelpoint-board #f)
(define parallelpoint-p1 #f)
(define parallelpoint-p2 #f)
(define parallelpoint-p3 #f)
(define parallelpoint-object #f)
(define perpendicularpoint-board #f)
(define perpendicularpoint-line-a #f)
(define perpendicularpoint-line-b #f)
(define perpendicularpoint-line #f)
(define perpendicularpoint-point #f)
(define perpendicularpoint-object #f)
(define ticks-board #f)
(define ticks-line #f)
(define ticks-object #f)
(define transformation-board #f)
(define transformation-source #f)
(define transformation-target #f)
(define transformation-object #f)
(define tracecurve-board #f)
(define tracecurve-circle #f)
(define tracecurve-point #f)
(define tracecurve-glider #f)
(define tracecurve-segment #f)
(define tracecurve-midpoint #f)
(define tracecurve-object #f)
(define parallelogram-board #f)
(define parallelogram-p1 #f)
(define parallelogram-p2 #f)
(define parallelogram-p3 #f)
(define parallelogram-object #f)
(define reflexangle-board #f)
(define reflexangle-p1 #f)
(define reflexangle-p2 #f)
(define reflexangle-p3 #f)
(define reflexangle-angle #f)
(define reflexangle-text #f)
(define reflexangle-object #f)
(define bisectorlines-board #f)
(define bisectorlines-line-1 #f)
(define bisectorlines-line-2 #f)
(define bisectorlines-object #f)
(define perpendicularsegment-board #f)
(define perpendicularsegment-line-a #f)
(define perpendicularsegment-line-b #f)
(define perpendicularsegment-point #f)
(define perpendicularsegment-object #f)
(define constructions-board #f)
(define widgets-board #f)
(define widget-button #f)
(define widget-button-label #f)
(define widget-graph #f)
(define widget-expression "Math.sin(x)")
(define widget-graph-visible? #t)
(define annotation-board #f)
(define annotation-slider #f)
(define annotation-text #f)
(define view3d-board #f)
(define view3d-view #f)
(define point3d-board #f)
(define point3d-view #f)
(define point3d-point #f)
(define line3d-board #f)
(define line3d-view #f)
(define line3d-point-1 #f)
(define line3d-point-2 #f)
(define line3d-line #f)
(define circle3d-board #f)
(define circle3d-view #f)
(define circle3d-center #f)
(define circle3d-point #f)
(define circle3d-circle #f)
(define plane3d-board #f)
(define plane3d-view #f)
(define plane3d-point-1 #f)
(define plane3d-point-2 #f)
(define plane3d-point-3 #f)
(define plane3d-plane #f)
(define plane3d-footprint #f)
(define axes3d-board #f)
(define axes3d-view #f)
(define axes3d-axes #f)
(define axis3d-board #f)
(define axis3d-view #f)
(define axis3d-axis #f)
(define functiongraph3d-board #f)
(define functiongraph3d-view #f)
(define functiongraph3d-surface #f)
(define intersectioncircle3d-board #f)
(define intersectioncircle3d-view #f)
(define intersectioncircle3d-sphere-1 #f)
(define intersectioncircle3d-sphere-2 #f)
(define intersectioncircle3d-intersection #f)
(define intersectionline3d-board #f)
(define intersectionline3d-view #f)
(define intersectionline3d-plane-1 #f)
(define intersectionline3d-plane-2 #f)
(define intersectionline3d-line #f)
(define curve3d-board #f)
(define curve3d-view #f)
(define curve3d-curve #f)
(define parametricsurface3d-board #f)
(define parametricsurface3d-view #f)
(define parametricsurface3d-surface #f)
(define polyhedron3d-board #f)
(define polyhedron3d-view #f)
(define polyhedron3d-object #f)
(define text3d-board #f)
(define text3d-view #f)
(define text3d-object #f)
(define sphere3d-board #f)
(define sphere3d-view #f)
(define sphere3d-center #f)
(define sphere3d-point #f)
(define sphere3d-object #f)
(define surface3d-board #f)
(define surface3d-view #f)
(define surface3d-object #f)
(define face3d-board #f)
(define face3d-view #f)
(define face3d-polyhedron #f)
(define face3d-object #f)
(define ticks3d-board #f)
(define ticks3d-view #f)
(define ticks3d-object #f)
(define transformation3d-board #f)
(define transformation3d-view #f)
(define transformation3d-transform #f)
(define transformation3d-point #f)
(define transformation3d-object #f)
(define vectorfield3d-board #f)
(define vectorfield3d-view #f)
(define vectorfield3d-object #f)

;; extern-present? : any/c -> boolean?
;;   Check whether a JS value is a live external value.
(define (extern-present? v)
  (external? v))

;; gallery-3d-view-options : -> any/c
;;   Build the 3D view options that match the official hidden-plane examples.
(define (gallery-3d-view-options)
  (js-object
   (vector (vector "projection" "parallel")
           (vector "axesPosition" "none")
           (vector "xPlaneRear"
                   (js-object
                    (vector (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "yPlaneRear"
                   (js-object
                    (vector (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "zPlaneRear"
                   (js-object
                    (vector (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "xPlaneFront"
                   (js-object
                    (vector (vector "visible" #t)
                            (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "yPlaneFront"
                   (js-object
                    (vector (vector "visible" #t)
                            (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "zPlaneFront"
                   (js-object
                    (vector (vector "visible" #t)
                            (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "xPlaneRearYAxis" (js-object (vector (vector "visible" #f))))
           (vector "xPlaneRearZAxis" (js-object (vector (vector "visible" #f))))
           (vector "yPlaneRearXAxis" (js-object (vector (vector "visible" #f))))
           (vector "yPlaneRearZAxis" (js-object (vector (vector "visible" #f))))
           (vector "zPlaneRearXAxis" (js-object (vector (vector "visible" #f))))
           (vector "zPlaneRearYAxis" (js-object (vector (vector "visible" #f))))
           (vector "xPlaneFrontYAxis" (js-object (vector (vector "visible" #f))))
           (vector "xPlaneFrontZAxis" (js-object (vector (vector "visible" #f))))
           (vector "yPlaneFrontXAxis" (js-object (vector (vector "visible" #f))))
           (vector "yPlaneFrontZAxis" (js-object (vector (vector "visible" #f))))
           (vector "zPlaneFrontXAxis" (js-object (vector (vector "visible" #f))))
           (vector "zPlaneFrontYAxis" (js-object (vector (vector "visible" #f)))))))

;; gallery-3d-view-options/trackball : -> any/c
;;   Build the 3D view options used by the main 3D gallery board.
(define (gallery-3d-view-options/trackball)
  (js-object
   (vector (vector "projection" "central")
           (vector "axesPosition" "none")
           (vector "xPlaneRear"
                   (js-object
                    (vector (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "yPlaneRear"
                   (js-object
                    (vector (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "zPlaneRear"
                   (js-object
                    (vector (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "xPlaneFront"
                   (js-object
                    (vector (vector "visible" #t)
                            (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "yPlaneFront"
                   (js-object
                    (vector (vector "visible" #t)
                            (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "zPlaneFront"
                   (js-object
                    (vector (vector "visible" #t)
                            (vector "fillColor" "#fff")
                            (vector "mesh3d"
                                    (js-object (vector (vector "visible" #f)))))))
           (vector "xPlaneRearYAxis" (js-object (vector (vector "visible" #f))))
           (vector "xPlaneRearZAxis" (js-object (vector (vector "visible" #f))))
           (vector "yPlaneRearXAxis" (js-object (vector (vector "visible" #f))))
           (vector "yPlaneRearZAxis" (js-object (vector (vector "visible" #f))))
           (vector "zPlaneRearXAxis" (js-object (vector (vector "visible" #f))))
           (vector "zPlaneRearYAxis" (js-object (vector (vector "visible" #f))))
           (vector "xPlaneFrontYAxis" (js-object (vector (vector "visible" #f))))
           (vector "xPlaneFrontZAxis" (js-object (vector (vector "visible" #f))))
           (vector "yPlaneFrontXAxis" (js-object (vector (vector "visible" #f))))
           (vector "yPlaneFrontZAxis" (js-object (vector (vector "visible" #f))))
           (vector "zPlaneFrontXAxis" (js-object (vector (vector "visible" #f))))
           (vector "zPlaneFrontYAxis" (js-object (vector (vector "visible" #f))))
           (vector "trackball"
                   (js-object (vector (vector "enabled" #t)))))))

;; set-status! : string? -> void?
;;   Update the status line in the gallery.
(define (set-status! s)
  (obs-set! @status s))

;; set-summary! : string? -> void?
;;   Update the summary line in the gallery.
(define (set-summary! s)
  (obs-set! @summary s))

;; grid-style-attributes : string? -> any/c
;;   Return the JSXGraph attributes for a named grid style.
(define (grid-style-attributes mode)
  (cond
    [(string=? mode "standard")
     (js-object (vector))]
    [(string=? mode "fancy")
     (js-object
      (vector (vector "major"
                      (js-object
                       (vector (vector "face" "plus")
                               (vector "size" 7)
                               (vector "strokeColor" "green")
                               (vector "strokeOpacity" 1))))
              (vector "minor"
                      (js-object
                       (vector (vector "size" 4))))
              (vector "minorElements" 3)))]
    [else
     (js-object
      (vector (vector "major"
                      (js-object
                       (vector (vector "face" "regularPolygon")
                               (vector "size" 8)
                               (vector "strokeColor" "blue")
                               (vector "fillColor" "orange")
                               (vector "strokeOpacity" 1))))
              (vector "minor"
                      (js-object
                       (vector (vector "face" "diamond")
                               (vector "size" 4)
                               (vector "strokeColor" "green")
                               (vector "fillColor" "grey"))))
              (vector "minorElements" 1)
              (vector "includeBoundaries" #f)))]))

;; refresh-grid-board! : -> void?
;;   Rebuild the grid board using the current grid mode.
(define (refresh-grid-board!)
  (when grid-board
    (jsx-board-remove-grids! grid-board)
    (set! grid-object
          (jsx-create-grid grid-board
                           (jsx-parents)
                           (grid-style-attributes grid-mode)))
    (jsx-board-full-update! grid-board)))

;; ensure-jsxgraph-assets! : -> void?
;;   Load the JSXGraph CSS and core script with a local fallback.
(define (ensure-jsxgraph-assets!)
  (define target
    (or (document-head)
        (document-body)))

  (define cdn-css "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraph.css")
  (define cdn-js  "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js")
  (define local-css "../../web-site-new/local/assets/vendor/jsxgraph/jsxgraph.css")
  (define local-js  "../../web-site-new/local/assets/vendor/jsxgraph/jsxgraphcore.js")

  (unless (document-query-selector "link[data-jsxgraph-css='1']")
    (define link (document-create-element "link"))
    (element-set-attribute! link "rel" "stylesheet")
    (element-set-attribute! link "href" cdn-css)
    (element-set-attribute! link "data-jsxgraph-css" "1")
    (element-add-event-listener!
     link
     "error"
     (lambda (_evt)
       (element-set-attribute! link "href" local-css)))
    (element-append! target link))

  (unless (document-query-selector "script[data-jsxgraph-core='1']")
    (define script (document-create-element "script"))
    (element-set-attribute! script "src" cdn-js)
    (element-set-attribute! script "data-jsxgraph-core" "1")
    (element-add-event-listener!
     script
     "error"
     (lambda (_evt)
       (element-set-attribute! script "src" local-js)))
    (element-append! target script)))

;; init-geometry-board! : -> boolean?
;;   Build the JSXGraph geometry board once the browser assets have loaded.
(define (init-geometry-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (if (not (extern-present? jxg))
      #f
      (let ()
        (define jsxgraph (js-ref jxg "JSXGraph"))
        (if (not (extern-present? jsxgraph))
            #f
            (let ()
              (unless geometry-board-ready?
                (console-log "Geometry board")
                (set! geometry-board
                      (jsx-create-board
                       geometry-board-id
                       (js-object
                        (vector (vector "boundingbox" #[-7 7 7 -7])
                                (vector "axis" #t)
                                (vector "keepaspectratio" #t)))))
                (set! geometry-p
                      (jsx-create-point geometry-board
                                        (jsx-parents -4 1)
                                        (js-object (vector (vector "name" "P")
                                                           (vector "size" 4)))))
                (set! geometry-q
                      (jsx-create-point geometry-board
                                        (jsx-parents 3 -1)
                                        (js-object (vector (vector "name" "Q")
                                                           (vector "size" 4)))))
                (set! geometry-arrow
                      (jsx-create-arrow geometry-board
                                        (jsx-parents geometry-p geometry-q)))
                (set! geometry-label
                      (jsx-create-smartlabel
                       geometry-board
                       (jsx-parents geometry-p)
                       (js-object
                        (vector (vector "digits" 1)
                                (vector "unit" "m")
                                (vector "dir" "col")
                                (vector "useMathJax" #f)))))
                (set! geometry-slider
                      (jsx-create-slider geometry-board
                                         (jsx-parents (jsx-parents -5 -5)
                                                       (jsx-parents 2 -5)
                                                       (jsx-parents 0 1 3))
                                         (js-object
                                          (vector (vector "name" "s")
                                                  (vector "strokeColor" "black")
                                                  (vector "fillColor" "white")))))
                (set! geometry-board-ready? #t))
              (void (jsx-board-full-update! geometry-board))
              #t)))))

;; init-group-board! : -> boolean?
;;   Build the JSXGraph group board once the browser assets have loaded.
(define (init-group-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless group-board-ready?
          (console-log "Group board")
          (set! group-board
                (jsx-create-board
                 group-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! group-p
                (jsx-create-point group-board
                                  (jsx-parents -3 2)
                                  (js-object (vector (vector "name" "P")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#c53030")))))
          (set! group-q
                (jsx-create-point group-board
                                  (jsx-parents 2 -1)
                                  (js-object (vector (vector "name" "Q")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
                (set! group-group
                      (jsx-create-group group-board
                                        (jsx-parents group-q)
                                        (js-object
                                         (vector (vector "name" "G")
                                                 (vector "strokeColor" "#4a5568")
                                                 (vector "fillColor" "#4a5568")))))
          (jsx-group-add-point! group-group group-q)
          (jsx-group-set-rotation-center! group-group group-p)
          (jsx-group-set-rotation-points! group-group (jsx-parents group-q))
          (jsx-group-update! group-group)
          (set! group-board-ready? #t))
        (void (jsx-board-full-update! group-board))
        #t)))

;; init-chart-board! : -> boolean?
;;   Build the JSXGraph chart board once the browser assets have loaded.
(define (init-chart-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        ;; Shift the bars left and slightly down so the legend stays clear.
        (define x-values (jsx-parents -5 -4 -3 -2 -1 0 1 2 3 4 5 6))
        (define data-values (jsx-parents 0 3 3 23 29 33 42 18 7 0 0 0))
        (define colors (jsx-parents "green" "yellow" "red" "blue"))
        (unless chart-board-ready?
          (console-log "Chart board")
          (set! chart-board
                (jsx-create-board
                 chart-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 50 13 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! chart
                (jsx-create-chart
                 chart-board
                 (jsx-parents x-values data-values)
                 (js-object
                  (vector (vector "chartStyle" "bar")
                          (vector "width" 1.4)
                          (vector "labels" data-values)
                          (vector "colors" colors)))))
          (set! chart-legend
                (jsx-create-legend
                 chart-board
                 (jsx-parents 10 45)
                 (js-object
                  (vector (vector "labels" data-values)
                          (vector "colors" colors)
                          (vector "strokeWidth" 5)))))
          (set! chart-board-ready? #t))
        (void (jsx-board-full-update! chart-board))
        #t)))

;; init-point-board! : -> boolean?
;;   Build the JSXGraph point board once the browser assets have loaded.
(define (init-point-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless point-board-ready?
          (console-log "Point board")
          (set! point-board
                (jsx-create-board
                 point-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! point-free
                (jsx-create-point point-board
                                  (jsx-parents -2 1)
                                  (js-object
                                   (vector (vector "name" "A")
                                           (vector "size" 6)
                                           (vector "strokeColor" "#2b6cb0")
                                           (vector "fillColor" "#2b6cb0")))))
          (set! point-fixed
                (jsx-create-point point-board
                                  (jsx-parents 2 1)
                                  (js-object
                                   (vector (vector "name" "B")
                                           (vector "size" 6)
                                           (vector "fixed" #t)
                                           (vector "strokeColor" "#c53030")
                                           (vector "fillColor" "#c53030")))))
          (set! point-base-line
                (jsx-create-line point-board
                                 (jsx-parents point-free point-fixed)
                                 (js-object
                                  (vector (vector "strokeColor" "#4a5568")
                                          (vector "strokeWidth" 2)))))
          (set! point-restricted
                (jsx-create-glider
                 point-board
                 (jsx-parents 0 -1 point-base-line)
                 (js-object
                  (vector (vector "name" "C")
                          (vector "size" 6)
                          (vector "strokeColor" "#805ad5")
                          (vector "fillColor" "#805ad5")))))
          (set! point-board-ready? #t))
        (void (jsx-board-full-update! point-board))
        #t)))

;; init-line-board! : -> boolean?
;;   Build the JSXGraph line board once the browser assets have loaded.
(define (init-line-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless line-board-ready?
          (console-log "Line board")
          (set! line-board
                (jsx-create-board
                 line-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (define line-p1
            (jsx-create-point line-board
                              (jsx-parents -2 2)
                              (js-object (vector (vector "name" "A")
                                                 (vector "size" 4)))))
          (define line-p2
            (jsx-create-point line-board
                              (jsx-parents 2 -1)
                              (js-object (vector (vector "name" "B")
                                                 (vector "size" 4)))))
          (define line-segment-p1
            (jsx-create-point line-board
                              (jsx-parents -3 1)
                              (js-object (vector (vector "name" "C")
                                                 (vector "size" 4)))))
          (define line-segment-p2
            (jsx-create-point line-board
                              (jsx-parents -1 3)
                              (js-object (vector (vector "name" "D")
                                                 (vector "size" 4)))))
          (define line-ray-p1
            (jsx-create-point line-board
                              (jsx-parents 0 -2)
                              (js-object (vector (vector "name" "E")
                                                 (vector "size" 4)))))
          (define line-ray-p2
            (jsx-create-point line-board
                              (jsx-parents 3 -2)
                              (js-object (vector (vector "name" "F")
                                                 (vector "size" 4)))))
          (set! line-object
                (jsx-create-line line-board
                                 (jsx-parents line-p1 line-p2)
                                 (js-object
                                  (vector (vector "strokeColor" "#2b6cb0")
                                          (vector "strokeWidth" 3)))))
          (define line-segment
            (jsx-create-segment line-board
                                (jsx-parents line-segment-p1 line-segment-p2)
                                (js-object
                                 (vector (vector "strokeColor" "#4a5568")
                                         (vector "strokeWidth" 3)))))
          (define line-ray
            (jsx-create-line line-board
                             (jsx-parents line-ray-p1 line-ray-p2)
                             (js-object
                              (vector (vector "straightFirst" #f)
                                      (vector "straightLast" #t)
                                      (vector "strokeColor" "#d53f8c")
                                      (vector "strokeWidth" 3)))))
          (void line-segment)
          (void line-ray)
          (set! line-board-ready? #t))
        (void (jsx-board-full-update! line-board))
        #t)))

;; init-arc-board! : -> boolean?
;;   Build the JSXGraph arc board once the browser assets have loaded.
(define (init-arc-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless arc-board-ready?
          (console-log "Arc board")
          (set! arc-board
                (jsx-create-board
                 arc-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 5 5 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (define arc-p1
            (jsx-create-point arc-board
                              (jsx-parents -2 1)
                              (js-object (vector (vector "name" "A")
                                                 (vector "size" 4)))))
          (define arc-p2
            (jsx-create-point arc-board
                              (jsx-parents 0 3)
                              (js-object (vector (vector "name" "B")
                                                 (vector "size" 4)))))
          (define arc-p3
            (jsx-create-point arc-board
                              (jsx-parents 2 1)
                              (js-object (vector (vector "name" "C")
                                                 (vector "size" 4)))))
          (set! arc-leg-1
                (jsx-create-segment arc-board
                                    (jsx-parents arc-p2 arc-p1)
                                    (js-object (vector (vector "strokeColor" "#4a5568")
                                                       (vector "strokeWidth" 2)))))
          (set! arc-leg-2
                (jsx-create-segment arc-board
                                    (jsx-parents arc-p1 arc-p3)
                                    (js-object (vector (vector "strokeColor" "#4a5568")
                                                       (vector "strokeWidth" 2)))))
          (set! arc-object
                (jsx-create-arc arc-board
                                (jsx-parents arc-p1 arc-p2 arc-p3)
                                (js-object
                                 (vector (vector "strokeColor" "#d53f8c")
                                         (vector "strokeWidth" 3)
                                         (vector "fillOpacity" 0.12)))))
          (set! arc-board-ready? #t))
        (void (jsx-board-full-update! arc-board))
        #t)))

;; init-angle-board! : -> boolean?
;;   Build the JSXGraph angle board once the browser assets have loaded.
(define (init-angle-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless angle-board-ready?
          (console-log "Angle board")
          (set! angle-board
                (jsx-create-board
                 angle-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 5 5 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (define angle-p1
            (jsx-create-point angle-board
                              (jsx-parents -2 -1)
                              (js-object (vector (vector "name" "A")
                                                 (vector "size" 4)))))
          (define angle-p2
            (jsx-create-point angle-board
                              (jsx-parents 0 1)
                              (js-object (vector (vector "name" "B")
                                                 (vector "size" 4)))))
          (define angle-p3
            (jsx-create-point angle-board
                              (jsx-parents 2 -1)
                              (js-object (vector (vector "name" "C")
                                                 (vector "size" 4)))))
          (set! angle-leg-1
                (jsx-create-segment angle-board
                                    (jsx-parents angle-p2 angle-p1)
                                    (js-object
                                     (vector (vector "strokeColor" "#718096")
                                             (vector "strokeWidth" 2)))))
          (set! angle-leg-2
                (jsx-create-segment angle-board
                                    (jsx-parents angle-p2 angle-p3)
                                    (js-object
                                     (vector (vector "strokeColor" "#718096")
                                             (vector "strokeWidth" 2)))))
          (set! angle-object
                (jsx-create-angle angle-board
                                  (jsx-parents angle-p1 angle-p2 angle-p3)
                                  (js-object
                                   (vector (vector "strokeColor" "#2b6cb0")
                                           (vector "fillColor" "#bee3f8")
                                           (vector "fillOpacity" 0.35)))))
          (set! angle-board-ready? #t))
        (void (jsx-board-full-update! angle-board))
        #t)))

;; init-sector-board! : -> boolean?
;;   Build the JSXGraph sector board once the browser assets have loaded.
(define (init-sector-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless sector-board-ready?
          (console-log "Sector board")
          (set! sector-board
                (jsx-create-board
                 sector-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 5 5 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (define sector-p1
            (jsx-create-point sector-board
                              (jsx-parents -2 -1)
                              (js-object (vector (vector "name" "A")
                                                 (vector "size" 4)))))
          (define sector-p2
            (jsx-create-point sector-board
                              (jsx-parents 0 1)
                              (js-object (vector (vector "name" "B")
                                                 (vector "size" 4)))))
          (define sector-p3
            (jsx-create-point sector-board
                              (jsx-parents 2 -1)
                              (js-object (vector (vector "name" "C")
                                                 (vector "size" 4)))))
          (set! sector-object
                (jsx-create-sector sector-board
                                   (jsx-parents sector-p1 sector-p2 sector-p3)
                                   (js-object
                                    (vector (vector "strokeColor" "#2b6cb0")
                                            (vector "fillColor" "#bee3f8")
                                            (vector "fillOpacity" 0.35)))))
          (set! sector-board-ready? #t))
        (void (jsx-board-full-update! sector-board))
        #t)))

;; init-arrowparallel-board! : -> boolean?
;;   Build the JSXGraph arrowparallel board once the browser assets have loaded.
(define (init-arrowparallel-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless arrowparallel-board-ready?
          (console-log "Arrowparallel board")
          (set! arrowparallel-board
                (jsx-create-board
                 arrowparallel-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 9 9 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! arrowparallel-p1
                (jsx-create-point arrowparallel-board
                                  (jsx-parents 0 2)
                                  (js-object (vector (vector "name" "P1")
                                                     (vector "size" 4)))))
          (set! arrowparallel-p2
                (jsx-create-point arrowparallel-board
                                  (jsx-parents 2 1)
                                  (js-object (vector (vector "name" "P2")
                                                     (vector "size" 4)))))
          (set! arrowparallel-segment
                (jsx-create-segment arrowparallel-board
                                    (jsx-parents arrowparallel-p1 arrowparallel-p2)))
          (set! arrowparallel-p3
                (jsx-create-point arrowparallel-board
                                  (jsx-parents 3 3)
                                  (js-object (vector (vector "name" "P3")
                                                     (vector "size" 4)))))
          (set! arrowparallel-object
                (jsx-create-arrowparallel arrowparallel-board
                                          (jsx-parents arrowparallel-p1
                                                       arrowparallel-p2
                                                       arrowparallel-p3)))
          (set! arrowparallel-board-ready? #t))
        (void (jsx-board-full-update! arrowparallel-board))
        #t)))

;; init-axis-board! : -> boolean?
;;   Build the JSXGraph axis board once the browser assets have loaded.
(define (init-axis-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless axis-board-ready?
          (console-log "Axis board")
          (set! axis-board
                (jsx-create-board
                 axis-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #f)
                          (vector "keepaspectratio" #t)))))
          (set! axis-x
                (jsx-create-axis axis-board
                                 (jsx-parents (jsx-parents 0 0)
                                              (jsx-parents 1 0))
                                 (js-object
                                  (vector (vector "name" "x")
                                          (vector "strokeColor" "#2b6cb0")))))
          (set! axis-y
                (jsx-create-axis axis-board
                                 (jsx-parents (jsx-parents 0 0)
                                              (jsx-parents 0 1))
                                 (js-object
                                  (vector (vector "name" "y")
                                          (vector "strokeColor" "#c53030")))))
          (set! axis-board-ready? #t))
        (void (jsx-board-full-update! axis-board))
        #t)))

;; init-segment-board! : -> boolean?
;;   Build the JSXGraph segment board once the browser assets have loaded.
(define (init-segment-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless segment-board-ready?
          (console-log "Segment board")
          (set! segment-board
                (jsx-create-board
                 segment-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! segment-a
                (jsx-create-point segment-board
                                  (jsx-parents -2 1)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 4)))))
          (set! segment-b
                (jsx-create-point segment-board
                                  (jsx-parents 2 -1)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 4)))))
          (set! segment-object
                (jsx-create-segment segment-board
                                    (jsx-parents segment-a segment-b)
                                    (js-object (vector (vector "name" "AB")))))
          (set! segment-board-ready? #t))
        (void (jsx-board-full-update! segment-board))
        #t)))

;; init-intersection-board! : -> boolean?
;;   Build the JSXGraph intersection board once the browser assets have loaded.
(define (init-intersection-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless intersection-board-ready?
          (console-log "Intersection board")
          (set! intersection-board
                (jsx-create-board
                 intersection-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! intersection-a
                (jsx-create-point intersection-board
                                  (jsx-parents -2 2)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 4)))))
          (set! intersection-b
                (jsx-create-point intersection-board
                                  (jsx-parents 2 -1)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 4)))))
          (set! intersection-c
                (jsx-create-point intersection-board
                                  (jsx-parents -2 -1)
                                  (js-object (vector (vector "name" "C")
                                                     (vector "size" 4)))))
          (set! intersection-d
                (jsx-create-point intersection-board
                                  (jsx-parents 2 2)
                                  (js-object (vector (vector "name" "D")
                                                     (vector "size" 4)))))
          (set! intersection-line-1
                (jsx-create-line intersection-board
                                 (jsx-parents intersection-a intersection-b)
                                 (js-object (vector (vector "name" "l1")))))
          (set! intersection-line-2
                (jsx-create-line intersection-board
                                 (jsx-parents intersection-c intersection-d)
                                 (js-object (vector (vector "name" "l2")))))
          (set! intersection-point
                (jsx-create-intersection intersection-board
                                         (jsx-parents intersection-line-1
                                                      intersection-line-2)
                                         (js-object (vector (vector "name" "I")
                                                            (vector "size" 4)
                                                            (vector "fillColor" "#c53030")
                                                            (vector "strokeColor" "#c53030")))))
          (set! intersection-board-ready? #t))
        (void (jsx-board-full-update! intersection-board))
        #t)))

;; init-orthogonal-board! : -> boolean?
;;   Build the JSXGraph normal board once the browser assets have loaded.
(define (init-orthogonal-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless orthogonal-board-ready?
          (console-log "Normal board")
          (set! orthogonal-board
                (jsx-create-board
                 orthogonal-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! orthogonal-curve
                (jsx-create-circle orthogonal-board
                                   (jsx-parents (jsx-parents 0 0)
                                                (jsx-parents 1 0))
                                   (js-object (vector (vector "name" "c")))))
          (set! orthogonal-point
                (jsx-create-point orthogonal-board
                                  (jsx-parents 1.5 1.4)
                                  (js-object (vector (vector "name" "P")
                                                     (vector "size" 4)
                                                     (vector "fillColor" "#c53030")
                                                     (vector "strokeColor" "#c53030")))))
          (set! orthogonal-object
                (jsx-create-normal orthogonal-board
                                   (jsx-parents orthogonal-curve orthogonal-point)
                                   (js-object (vector (vector "name" "n")))))
          (set! orthogonal-board-ready? #t))
        (void (jsx-board-full-update! orthogonal-board))
        #t)))

;; init-grid-board! : -> boolean?
;;   Build the JSXGraph grid board once the browser assets have loaded.
(define (init-grid-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless grid-board-ready?
          (console-log "Grid board")
          (set! grid-board
                (jsx-create-board
                 grid-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (refresh-grid-board!)
          (set! grid-board-ready? #t))
        (void (jsx-board-full-update! grid-board))
        #t)))

;; init-boxplot-board! : -> boolean?
;;   Build the JSXGraph boxplot board once the browser assets have loaded.
(define (init-boxplot-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless boxplot-board-ready?
          (console-log "Boxplot board")
          (set! boxplot-board
                (jsx-create-board
                 boxplot-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-5 8 7 -2])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! boxplot-q
                (jsx-create-boxplot
                 boxplot-board
                 (jsx-parents (jsx-parents -1 2 3 3.5 5) 2 4)
                 (js-object
                  (vector (vector "strokeWidth" 3)))))
          (set! boxplot-object boxplot-q)
          (set! boxplot-board-ready? #t))
        (void (jsx-board-full-update! boxplot-board))
        #t)))

;; init-tangent-board! : -> boolean?
;;   Build the JSXGraph tangent board once the browser assets have loaded.
(define (init-tangent-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless tangent-board-ready?
          (console-log "Tangent board")
          (set! tangent-board
                (jsx-create-board
                 tangent-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! tangent-curve
                (jsx-create-functiongraph
                 tangent-board
                 (jsx-parents (lambda (x) (/ (* x x) 5.0)) -5 5)
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")))))
          (set! tangent-glider
                (jsx-create-glider
                 tangent-board
                 (jsx-parents 1.0 0.2 tangent-curve)
                 (js-object
                  (vector (vector "name" "g")
                          (vector "size" 4)
                          (vector "strokeColor" "#c53030")))))
          (set! tangent-line
                (jsx-create-tangent tangent-board
                                    (jsx-parents tangent-glider)
                                    (js-object
                                     (vector (vector "strokeColor" "#4a5568")
                                             (vector "dash" 2)))))
          (set! tangent-board-ready? #t))
        (void (jsx-board-full-update! tangent-board))
        #t)))

;; init-tangentto-board! : -> boolean?
;;   Build the JSXGraph tangentto board once the browser assets have loaded.
(define (init-tangentto-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless tangentto-board-ready?
          (console-log "TangentTo board")
          (set! tangentto-board
                (jsx-create-board
                 tangentto-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-8 8 8 -8])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! tangentto-circle
                (jsx-create-circle
                 tangentto-board
                 (jsx-parents (jsx-parents 3 0) (jsx-parents 3 4))
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")))))
          (set! tangentto-point
                (jsx-create-point
                 tangentto-board
                 (jsx-parents 0 6)
                 (js-object
                  (vector (vector "name" "P")
                          (vector "size" 4)
                          (vector "strokeColor" "#c53030")))))
          (set! tangentto-left
                (jsx-create-tangentto
                 tangentto-board
                 (jsx-parents tangentto-circle tangentto-point 0)
                 (js-object
                  (vector (vector "color" "black")
                          (vector "polar" (js-object (vector (vector "visible" #t))))
                          (vector "point" (js-object (vector (vector "visible" #t))))))))
          (set! tangentto-right
                (jsx-create-tangentto
                 tangentto-board
                 (jsx-parents tangentto-circle tangentto-point 1)
                 (js-object
                  (vector (vector "color" "black")))))
          (set! tangentto-board-ready? #t))
        (void (jsx-board-full-update! tangentto-board))
        #t)))

;; init-polarline-board! : -> boolean?
;;   Build the JSXGraph polarline board once the browser assets have loaded.
(define (init-polarline-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless polarline-board-ready?
          (console-log "PolarLine board")
          (set! polarline-board
                (jsx-create-board
                 polarline-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 5 5 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! polarline-p1 (jsx-create-point polarline-board (jsx-parents -1 2)
                                               (js-object (vector (vector "name" "A")))))
          (set! polarline-p2 (jsx-create-point polarline-board (jsx-parents 1 4)
                                               (js-object (vector (vector "name" "B")))))
          (set! polarline-p3 (jsx-create-point polarline-board (jsx-parents -1 -2)
                                               (js-object (vector (vector "name" "C")))))
          (set! polarline-p4 (jsx-create-point polarline-board (jsx-parents 0 0)
                                               (js-object (vector (vector "name" "D")))))
          (set! polarline-p5 (jsx-create-point polarline-board (jsx-parents 4 -2)
                                               (js-object (vector (vector "name" "E")))))
          (set! polarline-conic
                (jsx-create-conic
                 polarline-board
                 (jsx-parents polarline-p1 polarline-p2 polarline-p3 polarline-p4 polarline-p5)))
          (set! polarline-p6 (jsx-create-point polarline-board (jsx-parents -1 1)
                                               (js-object (vector (vector "name" "P")))))
          (set! polarline-object
                (jsx-create-polarline polarline-board
                                      (jsx-parents polarline-conic polarline-p6)
                                      (js-object
                                       (vector (vector "strokeColor" "#4a5568")))))
          (set! polarline-board-ready? #t))
        (void (jsx-board-full-update! polarline-board))
        #t)))

;; init-polepoint-board! : -> boolean?
;;   Build the JSXGraph polepoint board once the browser assets have loaded.
(define (init-polepoint-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless polepoint-board-ready?
          (console-log "PolePoint board")
          (set! polepoint-board
                (jsx-create-board
                 polepoint-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 5 5 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! polepoint-p1 (jsx-create-point polepoint-board (jsx-parents -1 2)
                                               (js-object (vector (vector "name" "A")))))
          (set! polepoint-p2 (jsx-create-point polepoint-board (jsx-parents 1 4)
                                               (js-object (vector (vector "name" "B")))))
          (set! polepoint-p3 (jsx-create-point polepoint-board (jsx-parents -1 -2)
                                               (js-object (vector (vector "name" "C")))))
          (set! polepoint-p4 (jsx-create-point polepoint-board (jsx-parents 0 0)
                                               (js-object (vector (vector "name" "D")))))
          (set! polepoint-p5 (jsx-create-point polepoint-board (jsx-parents 4 -2)
                                               (js-object (vector (vector "name" "E")))))
          (set! polepoint-conic
                (jsx-create-conic
                 polepoint-board
                 (jsx-parents polepoint-p1 polepoint-p2 polepoint-p3 polepoint-p4 polepoint-p5)))
          (set! polepoint-p6 (jsx-create-point polepoint-board (jsx-parents -1 4)
                                               (js-object (vector (vector "name" "P")))))
          (set! polepoint-p7 (jsx-create-point polepoint-board (jsx-parents 2 -2)
                                               (js-object (vector (vector "name" "Q")))))
          (set! polepoint-line
                (jsx-create-line polepoint-board
                                 (jsx-parents polepoint-p6 polepoint-p7)
                                 (js-object
                                  (vector (vector "strokeColor" "#4a5568")))))
          (set! polepoint-object
                (jsx-create-polepoint polepoint-board
                                      (jsx-parents polepoint-conic polepoint-line)
                                      (js-object
                                       (vector (vector "strokeColor" "#c53030")))))
          (set! polepoint-board-ready? #t))
        (void (jsx-board-full-update! polepoint-board))
        #t)))

;; init-radicalaxis-board! : -> boolean?
;;   Build the JSXGraph radicalaxis board once the browser assets have loaded.
(define (init-radicalaxis-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless radicalaxis-board-ready?
          (console-log "RadicalAxis board")
          (set! radicalaxis-board
                (jsx-create-board
                 radicalaxis-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 9 9 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! radicalaxis-p1 (jsx-create-point radicalaxis-board (jsx-parents 2 3)
                                                 (js-object (vector (vector "name" "A")))))
          (set! radicalaxis-p2 (jsx-create-point radicalaxis-board (jsx-parents 1 4)
                                                 (js-object (vector (vector "name" "B")))))
          (set! radicalaxis-circle-1
                (jsx-create-circle
                 radicalaxis-board
                 (jsx-parents radicalaxis-p1 radicalaxis-p2)))
          (set! radicalaxis-p3 (jsx-create-point radicalaxis-board (jsx-parents 6 5)
                                                 (js-object (vector (vector "name" "C")))))
          (set! radicalaxis-p4 (jsx-create-point radicalaxis-board (jsx-parents 8 6)
                                                 (js-object (vector (vector "name" "D")))))
          (set! radicalaxis-circle-2
                (jsx-create-circle
                 radicalaxis-board
                 (jsx-parents radicalaxis-p3 radicalaxis-p4)))
          (set! radicalaxis-object
                (jsx-create-radicalaxis radicalaxis-board
                                        (jsx-parents radicalaxis-circle-1 radicalaxis-circle-2)
                                        (js-object
                                         (vector (vector "strokeColor" "#4a5568")))))
          (set! radicalaxis-board-ready? #t))
        (void (jsx-board-full-update! radicalaxis-board))
        #t)))

;; init-circumcircle-board! : -> boolean?
;;   Build the JSXGraph circumcircle board once the browser assets have loaded.
(define (init-circumcircle-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless circumcircle-board-ready?
          (console-log "Circumcircle board")
          (set! circumcircle-board
                (jsx-create-board
                 circumcircle-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 9 9 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! circumcircle-p1 (jsx-create-point circumcircle-board (jsx-parents 0 2)
                                                  (js-object (vector (vector "name" "A")))))
          (set! circumcircle-p2 (jsx-create-point circumcircle-board (jsx-parents 2 1)
                                                  (js-object (vector (vector "name" "B")))))
          (set! circumcircle-p3 (jsx-create-point circumcircle-board (jsx-parents 3 3)
                                                  (js-object (vector (vector "name" "C")))))
          (set! circumcircle-triangle
                (jsx-create-polygon circumcircle-board
                                    (jsx-parents circumcircle-p1 circumcircle-p2 circumcircle-p3)
                                    (js-object
                                     (vector (vector "fillOpacity" 0.12)
                                             (vector "strokeColor" "#4a5568")
                                             (vector "highlightStrokeColor" "#4a5568")))))
          (set! circumcircle-object
                (jsx-create-circumcircle circumcircle-board
                                         (jsx-parents circumcircle-p1 circumcircle-p2 circumcircle-p3)
                                         (js-object
                                          (vector (vector "strokeColor" "#2b6cb0")))))
          (set! circumcircle-board-ready? #t))
        (void (jsx-board-full-update! circumcircle-board))
        #t)))

;; init-circumcirclearc-board! : -> boolean?
;;   Build the JSXGraph circumcirclearc board once the browser assets have loaded.
(define (init-circumcirclearc-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless circumcirclearc-board-ready?
          (console-log "CircumcircleArc board")
          (set! circumcirclearc-board
                (jsx-create-board
                 circumcirclearc-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 9 9 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! circumcirclearc-p1 (jsx-create-point circumcirclearc-board (jsx-parents 0 2)
                                                     (js-object (vector (vector "name" "A")))))
          (set! circumcirclearc-p2 (jsx-create-point circumcirclearc-board (jsx-parents 2 1)
                                                     (js-object (vector (vector "name" "B")))))
          (set! circumcirclearc-p3 (jsx-create-point circumcirclearc-board (jsx-parents 3 3)
                                                     (js-object (vector (vector "name" "C")))))
          (set! circumcirclearc-object
                (jsx-create-circumcirclearc circumcirclearc-board
                                            (jsx-parents circumcirclearc-p1
                                                         circumcirclearc-p2
                                                         circumcirclearc-p3)
                                            (js-object
                                             (vector (vector "strokeColor" "#2b6cb0")))))
          (set! circumcirclearc-board-ready? #t))
        (void (jsx-board-full-update! circumcirclearc-board))
        #t)))

;; init-circumcirclesector-board! : -> boolean?
;;   Build the JSXGraph circumcirclesector board once the browser assets have loaded.
(define (init-circumcirclesector-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless circumcirclesector-board-ready?
          (console-log "CircumcircleSector board")
          (set! circumcirclesector-board
                (jsx-create-board
                 circumcirclesector-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 9 9 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! circumcirclesector-p1 (jsx-create-point circumcirclesector-board (jsx-parents 0 2)
                                                        (js-object (vector (vector "name" "A")))))
          (set! circumcirclesector-p2 (jsx-create-point circumcirclesector-board (jsx-parents 2 1)
                                                        (js-object (vector (vector "name" "B")))))
          (set! circumcirclesector-p3 (jsx-create-point circumcirclesector-board (jsx-parents 3 3)
                                                        (js-object (vector (vector "name" "C")))))
          (set! circumcirclesector-object
                (jsx-create-circumcirclesector circumcirclesector-board
                                               (jsx-parents circumcirclesector-p1
                                                            circumcirclesector-p2
                                                            circumcirclesector-p3)
                                               (js-object
                                                (vector (vector "strokeColor" "#4a5568")))))
          (set! circumcirclesector-board-ready? #t))
        (void (jsx-board-full-update! circumcirclesector-board))
        #t)))

;; init-semicircle-board! : -> boolean?
;;   Build the JSXGraph semicircle board once the browser assets have loaded.
(define (init-semicircle-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless semicircle-board-ready?
          (console-log "Semicircle board")
          (set! semicircle-board
                (jsx-create-board
                 semicircle-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 9 9 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! semicircle-p1 (jsx-create-point semicircle-board (jsx-parents 0 2)
                                                (js-object (vector (vector "name" "A")))))
          (set! semicircle-p2 (jsx-create-point semicircle-board (jsx-parents 4 2)
                                                (js-object (vector (vector "name" "B")))))
          (set! semicircle-object
                (jsx-create-semicircle semicircle-board
                                       (jsx-parents semicircle-p1 semicircle-p2)
                                       (js-object
                                        (vector (vector "strokeColor" "#2b6cb0")))))
          (set! semicircle-board-ready? #t))
        (void (jsx-board-full-update! semicircle-board))
        #t)))

;; init-majorarc-board! : -> boolean?
;;   Build the JSXGraph majorarc board once the browser assets have loaded.
(define (init-majorarc-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless majorarc-board-ready?
          (console-log "MajorArc board")
          (set! majorarc-board
                (jsx-create-board
                 majorarc-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 9 9 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! majorarc-p1 (jsx-create-point majorarc-board (jsx-parents 0 2)
                                              (js-object (vector (vector "name" "A")))))
          (set! majorarc-p2 (jsx-create-point majorarc-board (jsx-parents 2 1)
                                              (js-object (vector (vector "name" "B")))))
          (set! majorarc-p3 (jsx-create-point majorarc-board (jsx-parents 3 3)
                                              (js-object (vector (vector "name" "C")))))
          (define majorarc-ac
            (jsx-create-segment majorarc-board
                                (jsx-parents majorarc-p1 majorarc-p3)
                                (js-object
                                 (vector (vector "strokeColor" "#718096")
                                         (vector "strokeWidth" 2)))))
          (define majorarc-ab
            (jsx-create-segment majorarc-board
                                (jsx-parents majorarc-p1 majorarc-p2)
                                (js-object
                                 (vector (vector "strokeColor" "#718096")
                                         (vector "strokeWidth" 2)))))
          (set! majorarc-object
                (jsx-create-majorarc majorarc-board
                                     (jsx-parents majorarc-p1 majorarc-p2 majorarc-p3)
                                     (js-object
                                      (vector (vector "strokeColor" "#2b6cb0")))))
          (void majorarc-ac)
          (void majorarc-ab)
          (set! majorarc-board-ready? #t))
        (void (jsx-board-full-update! majorarc-board))
        #t)))

;; init-majorsector-board! : -> boolean?
;;   Build the JSXGraph majorsector board once the browser assets have loaded.
(define (init-majorsector-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless majorsector-board-ready?
          (console-log "MajorSector board")
          (set! majorsector-board
                (jsx-create-board
                 majorsector-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 9 9 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! majorsector-p1 (jsx-create-point majorsector-board (jsx-parents 0 2)
                                                 (js-object (vector (vector "name" "A")))))
          (set! majorsector-p2 (jsx-create-point majorsector-board (jsx-parents 2 1)
                                                 (js-object (vector (vector "name" "B")))))
          (set! majorsector-p3 (jsx-create-point majorsector-board (jsx-parents 3 3)
                                                 (js-object (vector (vector "name" "C")))))
          (set! majorsector-object
                (jsx-create-majorsector majorsector-board
                                        (jsx-parents majorsector-p1 majorsector-p2 majorsector-p3)
                                        (js-object
                                         (vector (vector "strokeColor" "#4a5568")))))
          (set! majorsector-board-ready? #t))
        (void (jsx-board-full-update! majorsector-board))
        #t)))

;; init-curveintersection-board! : -> boolean?
;;   Build the JSXGraph curveintersection board once the browser assets have loaded.
(define (init-curveintersection-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless curveintersection-board-ready?
          (console-log "CurveIntersection board")
          (set! curveintersection-board
                (jsx-create-board
                 curveintersection-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! curveintersection-f-1
                (jsx-create-circle curveintersection-board
                                   (jsx-parents (jsx-parents -1 0) 3)
                                   (js-object (vector (vector "strokeColor" "#2b6cb0")))))
          (set! curveintersection-f-2
                (jsx-create-circle curveintersection-board
                                   (jsx-parents (jsx-parents 1 0) 3)
                                   (js-object (vector (vector "strokeColor" "#c53030")))))
          (set! curveintersection-object
                (jsx-create-curveintersection curveintersection-board
                                              (jsx-parents curveintersection-f-1
                                                           curveintersection-f-2)
                                              (js-object
                                               (vector (vector "fillColor" "gold")
                                                       (vector "fillOpacity" 0.3)))))
          (set! curveintersection-board-ready? #t))
        (void (jsx-board-full-update! curveintersection-board))
        #t)))

;; init-curvedifference-board! : -> boolean?
;;   Build the JSXGraph curvedifference board once the browser assets have loaded.
(define (init-curvedifference-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless curvedifference-board-ready?
          (console-log "CurveDifference board")
          (set! curvedifference-board
                (jsx-create-board
                 curvedifference-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! curvedifference-f-1
                (jsx-create-circle curvedifference-board
                                   (jsx-parents (jsx-parents -1 0) 3)
                                   (js-object (vector (vector "strokeColor" "#2b6cb0")))))
          (set! curvedifference-f-2
                (jsx-create-circle curvedifference-board
                                   (jsx-parents (jsx-parents 1 0) 3)
                                   (js-object (vector (vector "strokeColor" "#c53030")))))
          (set! curvedifference-object
                (jsx-create-curvedifference curvedifference-board
                                            (jsx-parents curvedifference-f-1
                                                         curvedifference-f-2)
                                            (js-object
                                             (vector (vector "fillColor" "gold")
                                                     (vector "fillOpacity" 0.3)))))
          (set! curvedifference-board-ready? #t))
        (void (jsx-board-full-update! curvedifference-board))
        #t)))

;; init-curveunion-board! : -> boolean?
;;   Build the JSXGraph curveunion board once the browser assets have loaded.
(define (init-curveunion-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless curveunion-board-ready?
          (console-log "CurveUnion board")
          (set! curveunion-board
                (jsx-create-board
                 curveunion-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! curveunion-f-1
                (jsx-create-circle curveunion-board
                                   (jsx-parents (jsx-parents -1 0) 3)
                                   (js-object (vector (vector "strokeColor" "#2b6cb0")))))
          (set! curveunion-f-2
                (jsx-create-circle curveunion-board
                                   (jsx-parents (jsx-parents 1 0) 3)
                                   (js-object (vector (vector "strokeColor" "#c53030")))))
          (set! curveunion-object
                (jsx-create-curveunion curveunion-board
                                       (jsx-parents curveunion-f-1
                                                    curveunion-f-2)
                                       (js-object
                                        (vector (vector "fillColor" "gold")
                                                (vector "fillOpacity" 0.3)))))
          (set! curveunion-board-ready? #t))
        (void (jsx-board-full-update! curveunion-board))
        #t)))

;; init-derivative-board! : -> boolean?
;;   Build the JSXGraph derivative board once the browser assets have loaded.
(define (init-derivative-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless derivative-board-ready?
          (console-log "Derivative board")
          (set! derivative-board
                (jsx-create-board
                 derivative-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! derivative-curve
                (jsx-create-functiongraph derivative-board
                                          (jsx-parents (lambda (x) (/ (* x x) 5.0)) -5 5)
                                          (js-object
                                           (vector (vector "strokeColor" "#2b6cb0")))))
          (set! derivative-object
                (jsx-create-derivative derivative-board
                                       (jsx-parents derivative-curve)
                                       (js-object
                                        (vector (vector "strokeColor" "#c53030")
                                                (vector "dash" 2)))))
          (set! derivative-board-ready? #t))
        (void (jsx-board-full-update! derivative-board))
        #t)))

;; init-integral-board! : -> boolean?
;;   Build the JSXGraph integral board once the browser assets have loaded.
(define (init-integral-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless integral-board-ready?
          (console-log "Integral board")
          (set! integral-board
                (jsx-create-board
                 integral-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! integral-curve
                (jsx-create-functiongraph integral-board
                                          (jsx-parents (lambda (t) (* t (cos t))) -5 5)
                                          (js-object
                                           (vector (vector "strokeColor" "#2b6cb0")))))
          (set! integral-object
                (jsx-create-integral integral-board
                                     (jsx-parents (jsx-parents -2 2)
                                                  integral-curve)
                                     (js-object
                                      (vector (vector "fillColor" "#f6ad55")
                                              (vector "fillOpacity" 0.3)))))
          (set! integral-board-ready? #t))
        (void (jsx-board-full-update! integral-board))
        #t)))

;; init-riemannsum-board! : -> boolean?
;;   Build the JSXGraph riemannsum board once the browser assets have loaded.
(define (init-riemannsum-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless riemannsum-board-ready?
          (console-log "Riemannsum board")
          (define riemannsum-f (lambda (x) (+ (* 0.5 x x) (- (* 2 x)))))
          (set! riemannsum-board
                (jsx-create-board
                 riemannsum-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-3 7 5 -3])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! riemannsum-curve
                (jsx-create-functiongraph riemannsum-board
                                          (jsx-parents riemannsum-f -2 5)
                                          (js-object
                                           (vector (vector "strokeColor" "#2b6cb0")))))
          (set! riemannsum-object
                (jsx-create-riemannsum riemannsum-board
                                       (jsx-parents riemannsum-f 8 "upper" -2 5)
                                       (js-object
                                        (vector (vector "fillColor" "#f6ad55")
                                                (vector "fillOpacity" 0.3)))))
          (set! riemannsum-value-text
                (jsx-create-text
                 riemannsum-board
                 (jsx-parents
                  -2
                  -2
                  (procedure->external
                   (lambda ()
                     (format "Riemann sum value = ~a"
                             (jsx-riemannsum-value riemannsum-object)))))
                 (js-object
                  (vector (vector "fontSize" 14)
                          (vector "anchorX" "left")))))
          (set! riemannsum-board-ready? #t))
        (void (jsx-board-full-update! riemannsum-board))
        #t)))

;; init-slopefield-board! : -> boolean?
;;   Build the JSXGraph slopefield board once the browser assets have loaded.
(define (init-slopefield-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless slopefield-board-ready?
          (console-log "Slopefield board")
          (define slope-f (lambda (x y) (+ (* x x) y)))
          (define slope-f-updated (lambda (x y) (- (+ (* x x) (* y y)) 2)))
          (set! slopefield-board
                (jsx-create-board
                 slopefield-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-8 8 8 -8])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! slopefield-object
                (jsx-create-slopefield slopefield-board
                                       (jsx-parents slope-f
                                                    (jsx-parents -6 25 6)
                                                    (jsx-parents -5 20 5))
                                       (js-object
                                        (vector (vector "strokeColor" "#2b6cb0")))))
          (jsx-slopefield-set-f! slopefield-object slope-f-updated)
          (set! slopefield-board-ready? #t))
        (void (jsx-board-full-update! slopefield-board))
        #t)))

;; init-vectorfield-board! : -> boolean?
;;   Build the JSXGraph vectorfield board once the browser assets have loaded.
(define (init-vectorfield-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless vectorfield-board-ready?
          (console-log "Vectorfield board")
          (define vector-fx (lambda (x y) (sin y)))
          (define vector-fy (lambda (x y) (cos x)))
          (define vector-fx-updated (lambda (x y) (- (sin y))))
          (define vector-fy-updated (lambda (x y) (cos (+ x y))))
          (set! vectorfield-board
                (jsx-create-board
                 vectorfield-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-8 8 8 -8])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! vectorfield-object
                (jsx-create-vectorfield vectorfield-board
                                        (jsx-parents (jsx-parents vector-fx vector-fy)
                                                     (jsx-parents -4 16 4)
                                                     (jsx-parents -4 16 4))
                                        (js-object
                                         (vector (vector "strokeColor" "#2f855a")))))
          (jsx-vectorfield-set-f! vectorfield-object
                                  (vector vector-fx-updated vector-fy-updated))
          (set! vectorfield-board-ready? #t))
        (void (jsx-board-full-update! vectorfield-board))
        #t)))

;; init-implicitcurve-board! : -> boolean?
;;   Build the JSXGraph implicitcurve board once the browser assets have loaded.
(define (init-implicitcurve-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless implicitcurve-board-ready?
          (console-log "ImplicitCurve board")
          (define implicitcurve-f
            (lambda (x y)
              (+ (/ (* x x) 16.0)
                 (* y y)
                 -1.0)))
          (set! implicitcurve-board
                (jsx-create-board
                 implicitcurve-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-8 8 8 -8])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! implicitcurve-object
                (jsx-create-implicitcurve implicitcurve-board
                                          (jsx-parents implicitcurve-f)
                                          (js-object
                                           (vector (vector "strokeWidth" 3)
                                                   (vector "strokeColor" "#c53030")
                                                   (vector "strokeOpacity" 0.8)))))
          (set! implicitcurve-text
                (jsx-create-text
                 implicitcurve-board
                 (jsx-parents -7 6 "Implicit curve relation: x^2/16 + y^2 - 1 = 0")
                 (js-object
                  (vector (vector "fontSize" 14)
                          (vector "anchorX" "left")))))
          (set! implicitcurve-board-ready? #t))
        (void (jsx-board-full-update! implicitcurve-board))
        #t)))

;; init-spline-board! : -> boolean?
;;   Build the JSXGraph spline board once the browser assets have loaded.
(define (init-spline-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless spline-board-ready?
          (console-log "Spline board")
          (set! spline-board
                (jsx-create-board
                 spline-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-8 8 8 -8])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! spline-p1 (jsx-create-point spline-board (jsx-parents -2 2)
                                            (js-object (vector (vector "size" 4)
                                                               (vector "face" "o")
                                                               (vector "name" "P1")))))
          (set! spline-p2 (jsx-create-point spline-board (jsx-parents 0 -1)
                                            (js-object (vector (vector "size" 4)
                                                               (vector "face" "o")
                                                               (vector "name" "P2")))))
          (set! spline-p3 (jsx-create-point spline-board (jsx-parents 2 0)
                                            (js-object (vector (vector "size" 4)
                                                               (vector "face" "o")
                                                               (vector "name" "P3")))))
          (set! spline-p4 (jsx-create-point spline-board (jsx-parents 4 1)
                                            (js-object (vector (vector "size" 4)
                                                               (vector "face" "o")
                                                               (vector "name" "P4")))))
          (set! spline-object
                (jsx-create-spline spline-board
                                   (jsx-parents spline-p1 spline-p2 spline-p3 spline-p4)
                                   (js-object
                                    (vector (vector "strokeWidth" 3)
                                            (vector "strokeColor" "#2b6cb0")))))
          (set! spline-board-ready? #t))
        (void (jsx-board-full-update! spline-board))
        #t)))

;; init-cardinalspline-board! : -> boolean?
;;   Build the JSXGraph cardinalspline board once the browser assets have loaded.
(define (init-cardinalspline-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless cardinalspline-board-ready?
          (console-log "Cardinalspline board")
          (set! cardinalspline-board
                (jsx-create-board
                 cardinalspline-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 8 8 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (define cardinal-p1
            (jsx-create-point cardinalspline-board
                              (jsx-parents -4 1)
                              (js-object (vector (vector "name" "A")
                                                 (vector "size" 4)))))
          (define cardinal-p2
            (jsx-create-point cardinalspline-board
                              (jsx-parents -2 4)
                              (js-object (vector (vector "name" "B")
                                                 (vector "size" 4)))))
          (define cardinal-p3
            (jsx-create-point cardinalspline-board
                              (jsx-parents 0 0)
                              (js-object (vector (vector "name" "C")
                                                 (vector "size" 4)))))
          (define cardinal-p4
            (jsx-create-point cardinalspline-board
                              (jsx-parents 2 3)
                              (js-object (vector (vector "name" "D")
                                                 (vector "size" 4)))))
          (define cardinal-p5
            (jsx-create-point cardinalspline-board
                              (jsx-parents 4 1)
                              (js-object (vector (vector "name" "E")
                                                 (vector "size" 4)))))
          (define cardinal-tau
            (jsx-create-slider cardinalspline-board
                               (jsx-parents (jsx-parents -5 -4)
                                            (jsx-parents 2 -4)
                                            (jsx-parents 0.001 0.5 1))
                               (js-object
                                (vector (vector "name" "tau")
                                        (vector "strokeColor" "black")
                                        (vector "fillColor" "white")))))
          (set! cardinalspline-object
                (jsx-create-cardinalspline
                 cardinalspline-board
                 (jsx-parents (jsx-parents cardinal-p1 cardinal-p2 cardinal-p3
                                           cardinal-p4 cardinal-p5)
                              (procedure->external
                               (lambda ()
                                 (jsx-slider-value cardinal-tau)))
                              "centripetal")
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")
                          (vector "strokeWidth" 3)))))
          (set! cardinalspline-board-ready? #t))
        (void (jsx-board-full-update! cardinalspline-board))
        #t)))

;; init-comb-board! : -> boolean?
;;   Build the JSXGraph comb board once the browser assets have loaded.
(define (init-comb-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless comb-board-ready?
          (console-log "Comb board")
          (set! comb-board
                (jsx-create-board
                 comb-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (define comb-p1
            (jsx-create-point comb-board
                              (jsx-parents -3 0)
                              (js-object (vector (vector "name" "P1")
                                                 (vector "size" 4)))))
          (define comb-p2
            (jsx-create-point comb-board
                              (jsx-parents 3 0)
                              (js-object (vector (vector "name" "P2")
                                                 (vector "size" 4)))))
          (set! comb-object
                (jsx-create-comb comb-board
                                 (jsx-parents comb-p1 comb-p2)
                                 (js-object
                                  (vector (vector "width" 0.35)
                                          (vector "frequency" 0.25)
                                          (vector "angle" (/ pi 4))
                                          (vector "curve"
                                                  (js-object
                                                   (vector (vector "strokeColor" "#2b6cb0"))))))))
          (set! comb-board-ready? #t))
        (void (jsx-board-full-update! comb-board))
        #t)))

;; init-metapostspline-board! : -> boolean?
;;   Build the JSXGraph metapostspline board once the browser assets have loaded.
(define (init-metapostspline-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless metapostspline-board-ready?
          (console-log "MetapostSpline board")
          (set! metapostspline-board
                (jsx-create-board
                 metapostspline-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 8 8 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (define mp-p1
            (jsx-create-point metapostspline-board
                              (jsx-parents -4 -3)
                              (js-object (vector (vector "name" "A")
                                                 (vector "size" 4)))))
          (define mp-p2
            (jsx-create-point metapostspline-board
                              (jsx-parents -1 -3)
                              (js-object (vector (vector "name" "B")
                                                 (vector "size" 4)))))
          (define mp-p3
            (jsx-create-point metapostspline-board
                              (jsx-parents 2 -5)
                              (js-object (vector (vector "name" "C")
                                                 (vector "size" 4)))))
          (define mp-p4
            (jsx-create-point metapostspline-board
                              (jsx-parents 5 -2)
                              (js-object (vector (vector "name" "D")
                                                 (vector "size" 4)))))
          (define mp-tension
            (jsx-create-slider metapostspline-board
                               (jsx-parents (jsx-parents -5 6)
                                            (jsx-parents 2 6)
                                            (jsx-parents 0 1 20))
                               (js-object (vector (vector "name" "tension")))))
          (define mp-curl
            (jsx-create-slider metapostspline-board
                               (jsx-parents (jsx-parents -5 5)
                                            (jsx-parents 2 5)
                                            (jsx-parents 0 1 30))
                               (js-object (vector (vector "name" "curl")))))
          (define mp-dir
            (jsx-create-slider metapostspline-board
                               (jsx-parents (jsx-parents -5 4)
                                            (jsx-parents 2 4)
                                            (jsx-parents -180 0 180))
                               (js-object (vector (vector "name" "direction")))))
          (define mp-controls
            (js-object
             (vector (vector "tension"
                             (procedure->external
                              (lambda ()
                                (jsx-slider-value mp-tension))))
                     (vector "direction"
                             (js-object
                              (vector (vector "1"
                                              (procedure->external
                                               (lambda ()
                                                 (jsx-slider-value mp-dir)))))))
                     (vector "curl"
                             (js-object
                              (vector (vector "0"
                                              (procedure->external
                                               (lambda ()
                                                 (jsx-slider-value mp-curl))))
                                      (vector "3"
                                              (procedure->external
                                               (lambda ()
                                                 (jsx-slider-value mp-curl)))))))
                     (vector "isClosed" #f))))
          (set! metapostspline-object
                (jsx-create-metapostspline metapostspline-board
                                           (jsx-parents
                                            (jsx-parents mp-p1 mp-p2 mp-p3 mp-p4)
                                            mp-controls)
                                           (js-object
                                            (vector (vector "strokeColor" "#2b6cb0")
                                                    (vector "strokeWidth" 3)))))
          (set! metapostspline-board-ready? #t))
        (void (jsx-board-full-update! metapostspline-board))
        #t)))

;; init-polygonalchain-board! : -> boolean?
;;   Build the JSXGraph polygonalchain board once the browser assets have loaded.
(define (init-polygonalchain-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless polygonalchain-board-ready?
          (console-log "PolygonalChain board")
          (set! polygonalchain-board
                (jsx-create-board
                 polygonalchain-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (define chain-points
            (jsx-parents (jsx-create-point polygonalchain-board (jsx-parents -4 0)
                                           (js-object (vector (vector "size" 4)
                                                              (vector "name" "A"))))
                         (jsx-create-point polygonalchain-board (jsx-parents -1 -3)
                                           (js-object (vector (vector "size" 4)
                                                              (vector "name" "B"))))
                         (jsx-create-point polygonalchain-board (jsx-parents 0 2)
                                           (js-object (vector (vector "size" 4)
                                                              (vector "name" "C"))))
                         (jsx-create-point polygonalchain-board (jsx-parents 2 1)
                                           (js-object (vector (vector "size" 4)
                                                              (vector "name" "D"))))
                         (jsx-create-point polygonalchain-board (jsx-parents 4 -2)
                                           (js-object (vector (vector "size" 4)
                                                              (vector "name" "E"))))))
          (define chain-attrs
            (js-object
             (vector (vector "borders"
                             (js-object (vector (vector "strokeWidth" 3))))
                     (vector "fillOpacity" 0)
                     (vector "strokeColor" "#2b6cb0"))))
          (set! polygonalchain-object
                (jsx-create-polygonalchain polygonalchain-board
                                           chain-points
                                           chain-attrs))
          (set! polygonalchain-board-ready? #t))
        (void (jsx-board-full-update! polygonalchain-board))
        #t)))

;; init-regularpolygon-board! : -> boolean?
;;   Build the JSXGraph regularpolygon board once the browser assets have loaded.
(define (init-regularpolygon-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless regularpolygon-board-ready?
          (console-log "RegularPolygon board")
          (set! regularpolygon-board
                (jsx-create-board
                 regularpolygon-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 9 9 -1])
                          (vector "axis" #f)
                          (vector "keepaspectratio" #t)))))
          (define rg-p1
            (jsx-create-point regularpolygon-board
                              (jsx-parents 0 2)
                              (js-object (vector (vector "name" "A")
                                                 (vector "size" 4)))))
          (define rg-p2
            (jsx-create-point regularpolygon-board
                              (jsx-parents 2 1)
                              (js-object (vector (vector "name" "B")
                                                 (vector "size" 4)))))
          (set! regularpolygon-object
                (jsx-create-regularpolygon regularpolygon-board
                                           (jsx-parents rg-p1 rg-p2 5)
                                           (js-object
                                            (vector (vector "fillOpacity" 0)
                                                    (vector "strokeColor" "#2b6cb0")
                                                    (vector "strokeWidth" 3)))))
          (set! regularpolygon-board-ready? #t))
        (void (jsx-board-full-update! regularpolygon-board))
        #t)))

;; init-hyperbola-board! : -> boolean?
;;   Build the JSXGraph hyperbola board once the browser assets have loaded.
(define (init-hyperbola-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless hyperbola-board-ready?
          (set! hyperbola-board
                (jsx-create-board
                 hyperbola-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "keepaspectratio" #t)))))
          (set! hyperbola-a (jsx-create-point hyperbola-board (jsx-parents -1 4)
                                              (js-object (vector (vector "name" "A")))))
          (set! hyperbola-b (jsx-create-point hyperbola-board (jsx-parents -1 -4)
                                              (js-object (vector (vector "name" "B")))))
          (set! hyperbola-c (jsx-create-point hyperbola-board (jsx-parents 1 1)
                                              (js-object (vector (vector "name" "C")))))
          (set! hyperbola-object
                (jsx-create-hyperbola hyperbola-board
                                      (jsx-parents hyperbola-a hyperbola-b hyperbola-c)
                                      (js-object
                                       (vector (vector "strokeColor" "#2b6cb0")
                                               (vector "strokeWidth" 3)))))
          (set! hyperbola-board-ready? #t))
        (void (jsx-board-full-update! hyperbola-board))
        #t)))

;; init-parabola-board! : -> boolean?
;;   Build the JSXGraph parabola board once the browser assets have loaded.
(define (init-parabola-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless parabola-board-ready?
          (set! parabola-board
                (jsx-create-board
                 parabola-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! parabola-a (jsx-create-point parabola-board (jsx-parents -1 4)
                                             (js-object (vector (vector "name" "A")))))
          (set! parabola-b (jsx-create-point parabola-board (jsx-parents -1 -4)
                                             (js-object (vector (vector "name" "B")))))
          (define parabola-line
            (jsx-create-line parabola-board
                             (jsx-parents parabola-a parabola-b)
                             (js-object (vector (vector "strokeColor" "#4a5568")))))
          (set! parabola-c (jsx-create-point parabola-board (jsx-parents 1 1)
                                             (js-object (vector (vector "name" "C")))))
          (set! parabola-object
                (jsx-create-parabola parabola-board
                                     (jsx-parents parabola-c parabola-line)
                                     (js-object
                                      (vector (vector "strokeColor" "#d53f8c")
                                              (vector "strokeWidth" 3)))))
          (set! parabola-board-ready? #t))
        (void (jsx-board-full-update! parabola-board))
        #t)))

;; init-stepfunction-board! : -> boolean?
;;   Build the JSXGraph stepfunction board once the browser assets have loaded.
(define (init-stepfunction-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless stepfunction-board-ready?
          (set! stepfunction-board
                (jsx-create-board
                 stepfunction-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 5 6 -2])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! stepfunction-object
                (jsx-create-stepfunction
                 stepfunction-board
                 (jsx-parents #(0 1 2 3 4 5) #(1 3 0 2 2 1))
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")
                          (vector "strokeWidth" 3)))))
          (set! stepfunction-board-ready? #t))
        (void (jsx-board-full-update! stepfunction-board))
        #t)))

;; init-inequality-board! : -> boolean?
;;   Build the JSXGraph inequality board once the browser assets have loaded.
(define (init-inequality-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless inequality-board-ready?
          (set! inequality-board
                (jsx-create-board
                 inequality-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 6 10 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! inequality-p (jsx-create-point inequality-board (jsx-parents 1 3)
                                               (js-object (vector (vector "name" "P")))))
          (set! inequality-q (jsx-create-point inequality-board (jsx-parents -2 -4)
                                               (js-object (vector (vector "name" "Q")))))
          (set! inequality-line
                (jsx-create-line inequality-board
                                 (jsx-parents inequality-p inequality-q)
                                 (js-object (vector (vector "strokeColor" "#4a5568")))))
          (set! inequality-object
                (jsx-create-inequality inequality-board
                                       (jsx-parents inequality-line)
                                       (js-object
                                        (vector (vector "fillOpacity" 0.3)
                                                (vector "fillColor" "#bee3f8")))))
          (set! inequality-board-ready? #t))
        (void (jsx-board-full-update! inequality-board))
        #t)))

;; init-conic-board! : -> boolean?
;;   Build the JSXGraph conic board once the browser assets have loaded.
(define (init-conic-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless conic-board-ready?
          (set! conic-board
                (jsx-create-board
                 conic-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! conic-a (jsx-create-point conic-board (jsx-parents -4 4)
                                          (js-object (vector (vector "name" "A")))))
          (set! conic-b (jsx-create-point conic-board (jsx-parents -1 5)
                                          (js-object (vector (vector "name" "B")))))
          (set! conic-c (jsx-create-point conic-board (jsx-parents 2 4)
                                          (js-object (vector (vector "name" "C")))))
          (set! conic-d (jsx-create-point conic-board (jsx-parents -3 2)
                                          (js-object (vector (vector "name" "D")))))
          (set! conic-e (jsx-create-point conic-board (jsx-parents 1 1)
                                          (js-object (vector (vector "name" "E")))))
          (set! conic-object
                (jsx-create-conic conic-board
                                  (jsx-parents conic-a conic-b conic-c conic-d conic-e)
                                  (js-object
                                   (vector (vector "strokeColor" "#2b6cb0")))))
          (set! conic-board-ready? #t))
        (void (jsx-board-full-update! conic-board))
        #t)))

;; init-ellipse-board! : -> boolean?
;;   Build the JSXGraph ellipse board once the browser assets have loaded.
(define (init-ellipse-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless ellipse-board-ready?
          (set! ellipse-board
                (jsx-create-board
                 ellipse-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! ellipse-a (jsx-create-point ellipse-board (jsx-parents -4 4)
                                            (js-object (vector (vector "name" "A")))))
          (set! ellipse-b (jsx-create-point ellipse-board (jsx-parents -1 5)
                                            (js-object (vector (vector "name" "B")))))
          (set! ellipse-c (jsx-create-point ellipse-board (jsx-parents 2 4)
                                            (js-object (vector (vector "name" "C")))))
          (set! ellipse-object
                (jsx-create-ellipse ellipse-board
                                    (jsx-parents ellipse-a ellipse-b ellipse-c)
                                    (js-object
                                     (vector (vector "strokeColor" "#d53f8c")))))
          (set! ellipse-board-ready? #t))
        (void (jsx-board-full-update! ellipse-board))
        #t)))

;; init-functiongraph-board! : -> boolean?
;;   Build the JSXGraph functiongraph board once the browser assets have loaded.
(define (init-functiongraph-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless functiongraph-board-ready?
          (console-log "Functiongraph board")
          (set! functiongraph-board
                (jsx-create-board
                 functiongraph-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! functiongraph-object
                (jsx-create-functiongraph
                 functiongraph-board
                 (jsx-parents
                  (lambda (x) (- (* 0.4 x x) (* 1.5 x)))
                  -3
                  4)
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")
                          (vector "strokeWidth" 3)))))
          (set! functiongraph-board-ready? #t))
        (void (jsx-board-full-update! functiongraph-board))
        #t)))

;; init-curve-board! : -> boolean?
;;   Build the JSXGraph curve board once the browser assets have loaded.
(define (init-curve-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless curve-board-ready?
          (console-log "Curve board")
          (set! curve-board
                (jsx-create-board
                 curve-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! curve-object
                (jsx-create-curve
                 curve-board
                 (jsx-parents (jsx-parents -4 -3 -2 -1 0 1 2 3 4)
                              (jsx-parents 0 1 2 1 0 -1 0 1 0))
                 (js-object
                  (vector (vector "strokeColor" "#d53f8c")
                          (vector "fillOpacity" 0)
                          (vector "strokeWidth" 3)))))
          (set! curve-board-ready? #t))
        (void (jsx-board-full-update! curve-board))
        #t)))

;; init-polygon-board! : -> boolean?
;;   Build the JSXGraph polygon board once the browser assets have loaded.
(define (init-polygon-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless polygon-board-ready?
          (console-log "Polygon board")
          (set! polygon-board
                (jsx-create-board
                 polygon-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! polygon-a (jsx-create-point polygon-board (jsx-parents -4 2)
                                            (js-object (vector (vector "name" "A")))))
          (set! polygon-b (jsx-create-point polygon-board (jsx-parents -1 4)
                                            (js-object (vector (vector "name" "B")))))
          (set! polygon-c (jsx-create-point polygon-board (jsx-parents 2 3)
                                            (js-object (vector (vector "name" "C")))))
          (set! polygon-d (jsx-create-point polygon-board (jsx-parents -2 0)
                                            (js-object (vector (vector "name" "D")))))
          (set! polygon-object
                (jsx-create-polygon polygon-board
                                    (jsx-parents polygon-a polygon-b polygon-c polygon-d)
                                    (js-object
                                     (vector (vector "fillOpacity" 0.15)
                                             (vector "strokeColor" "#2b6cb0")))))
          (set! polygon-board-ready? #t))
        (void (jsx-board-full-update! polygon-board))
        #t)))

;; init-turtle-board! : -> boolean?
;;   Build the JSXGraph turtle board once the browser assets have loaded.
(define (init-turtle-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless turtle-board-ready?
          (set! turtle-board
                (jsx-create-board
                 turtle-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! turtle-object
                (jsx-create-turtle turtle-board
                                   (jsx-parents -3 -1.25 0)
                                   (js-object
                                    (vector (vector "strokeColor" "#2b6cb0")
                                            (vector "strokeWidth" 3)
                                            (vector "strokeOpacity" 0.6)))))
          (let ()
            (define (turtle-call! method . args)
              (jsx-element-call/nullish turtle-object method (list->vector args)))
            (define (koch! len depth)
              (cond
                [(zero? depth)
                 (turtle-call! "forward" len)]
                [else
                 (define next (/ len 3))
                 (koch! next (sub1 depth))
                 (turtle-call! "right" 60)
                 (koch! next (sub1 depth))
                 (turtle-call! "left" 120)
                 (koch! next (sub1 depth))
                 (turtle-call! "right" 60)
                 (koch! next (sub1 depth))]))
            (turtle-call! "penDown")
            (koch! 6 4))
          (set! turtle-board-ready? #t))
        (void (jsx-board-full-update! turtle-board))
        #t)))

;; init-incircle-board! : -> boolean?
;;   Build the JSXGraph incircle board once the browser assets have loaded.
(define (init-incircle-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless incircle-board-ready?
          (set! incircle-board
                (jsx-create-board
                 incircle-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 9 9 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! incircle-p1 (jsx-create-point incircle-board (jsx-parents 0 2)
                                              (js-object (vector (vector "name" "A")))))
          (set! incircle-p2 (jsx-create-point incircle-board (jsx-parents 2 1)
                                              (js-object (vector (vector "name" "B")))))
          (set! incircle-p3 (jsx-create-point incircle-board (jsx-parents 3 3)
                                              (js-object (vector (vector "name" "C")))))
          (set! incircle-triangle
                (jsx-create-polygon
                 incircle-board
                 (jsx-parents incircle-p1 incircle-p2 incircle-p3)
                 (js-object
                  (vector (vector "fillOpacity" 0)
                          (vector "strokeColor" "#4a5568")
                          (vector "highlightStrokeColor" "#4a5568")))))
          (set! incircle-object
                (jsx-create-incircle incircle-board
                                     (jsx-parents incircle-p1 incircle-p2 incircle-p3)
                                     (js-object
                                      (vector (vector "strokeColor" "#c53030")))))
          (set! incircle-board-ready? #t))
        (void (jsx-board-full-update! incircle-board))
        #t)))

;; init-arrow-board! : -> boolean?
;;   Build the JSXGraph arrow board once the browser assets have loaded.
(define (init-arrow-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless arrow-board-ready?
          (console-log "Arrow board")
          (set! arrow-board
                (jsx-create-board
                 arrow-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! arrow-a
                (jsx-create-point arrow-board
                                  (jsx-parents -2 1)
                                  (js-object (vector (vector "name" "A")))))
          (set! arrow-b
                (jsx-create-point arrow-board
                                  (jsx-parents 2 -1)
                                  (js-object (vector (vector "name" "B")))))
          (set! arrow-object
                (jsx-create-arrow arrow-board
                                  (jsx-parents arrow-a arrow-b)
                                  (js-object (vector (vector "strokeColor" "#2b6cb0")))))
          (set! arrow-board-ready? #t))
        (void (jsx-board-full-update! arrow-board))
        #t)))

;; init-circle-board! : -> boolean?
;;   Build the JSXGraph circle board once the browser assets have loaded.
(define (init-circle-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless circle-board-ready?
          (console-log "Circle board")
          (set! circle-board
                (jsx-create-board
                 circle-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-5 5 5 -5])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! circle-center
                (jsx-create-point circle-board
                                  (jsx-parents -1 0)
                                  (js-object (vector (vector "name" "O")))))
          (set! circle-through
                (jsx-create-point circle-board
                                  (jsx-parents 2 1)
                                  (js-object (vector (vector "name" "P")))))
          (set! circle-object
                (jsx-create-circle circle-board
                                   (jsx-parents circle-center circle-through)
                                   (js-object (vector (vector "strokeColor" "#2b6cb0")))))
          (set! circle-board-ready? #t))
        (void (jsx-board-full-update! circle-board))
        #t)))

;; init-glider-board! : -> boolean?
;;   Build the JSXGraph glider board once the browser assets have loaded.
(define (init-glider-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless glider-board-ready?
          (console-log "Glider board")
          (set! glider-board
                (jsx-create-board
                 glider-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-5 5 5 -5])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! glider-center
                (jsx-create-point glider-board
                                  (jsx-parents -1 0)
                                  (js-object (vector (vector "name" "O")))))
          (set! glider-through
                (jsx-create-point glider-board
                                  (jsx-parents 2 0)
                                  (js-object (vector (vector "name" "P")))))
          (set! glider-base-circle
                (jsx-create-circle glider-board
                                   (jsx-parents glider-center glider-through)
                                   (js-object (vector (vector "strokeColor" "#2b6cb0")))))
          (set! glider-object
                (jsx-create-glider glider-board
                                   (jsx-parents -1 2 glider-base-circle)
                                   (js-object (vector (vector "name" "A")
                                                      (vector "strokeColor" "#c53030")
                                                      (vector "fillColor" "#f56565")))))
          (set! glider-point glider-object)
          (set! glider-board-ready? #t))
        (void (jsx-board-full-update! glider-board))
        #t)))

;; init-button-board! : -> boolean?
;;   Build the JSXGraph button board once the browser assets have loaded.
(define (init-button-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless button-board-ready?
          (console-log "Button board")
          (set! button-board
                (jsx-create-board
                 button-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! button-object
                (jsx-create-button
                 button-board
                 (jsx-parents -2 0 "Click me"
                              (procedure->external
                               (lambda ()
                                 (set-summary! "The dedicated button board button was clicked."))))
                 (js-object
                  (vector (vector "name" "button")))))
          (set! button-board-ready? #t))
        (void (jsx-board-full-update! button-board))
        #t)))

;; init-legend-board! : -> boolean?
;;   Build the JSXGraph legend board once the browser assets have loaded.
(define (init-legend-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless legend-board-ready?
          (console-log "Legend board")
          (set! legend-board
                (jsx-create-board
                 legend-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! legend-chart
                (jsx-create-chart
                 legend-board
                 (jsx-parents #(1 2 3 4) #(4 2 3 1))
                 (js-object
                  (vector (vector "chartStyle" "bar")
                          (vector "labels" #(4 2 3 1))
                          (vector "colors" (jsx-parents "red" "green" "blue" "orange"))))))
          (set! legend-object
                (jsx-create-legend
                 legend-board
                 (jsx-parents 2 3)
                 (js-object
                  (vector (vector "labels" #(4 2 3 1))
                          (vector "colors" (jsx-parents "red" "green" "blue" "orange"))))))
          (set! legend-board-ready? #t))
        (void (jsx-board-full-update! legend-board))
        #t)))

;; init-midpoint-board! : -> boolean?
;;   Build the JSXGraph midpoint board once the browser assets have loaded.
(define (init-midpoint-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless midpoint-board-ready?
          (console-log "Midpoint board")
          (set! midpoint-board
                (jsx-create-board
                 midpoint-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! midpoint-a
                (jsx-create-point midpoint-board
                                  (jsx-parents -2 1)
                                  (js-object (vector (vector "name" "A")))))
          (set! midpoint-b
                (jsx-create-point midpoint-board
                                  (jsx-parents 2 -1)
                                  (js-object (vector (vector "name" "B")))))
          (set! midpoint-segment
                (jsx-create-segment midpoint-board
                                    (jsx-parents midpoint-a midpoint-b)
                                    (js-object (vector (vector "strokeColor" "#4a5568")
                                                       (vector "strokeWidth" 2)))))
          (set! midpoint-object
                (jsx-create-midpoint midpoint-board
                                     (jsx-parents midpoint-a midpoint-b)
                                     (js-object (vector (vector "name" "M")))))
          (set! midpoint-board-ready? #t))
        (void (jsx-board-full-update! midpoint-board))
        #t)))

;; init-parallel-board! : -> boolean?
;;   Build the JSXGraph parallel board once the browser assets have loaded.
(define (init-parallel-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless parallel-board-ready?
          (console-log "Parallel board")
          (set! parallel-board
                (jsx-create-board
                 parallel-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! parallel-a
                (jsx-create-point parallel-board
                                  (jsx-parents -4 -2)
                                  (js-object (vector (vector "name" "A")))))
          (set! parallel-b
                (jsx-create-point parallel-board
                                  (jsx-parents 4 1)
                                  (js-object (vector (vector "name" "B")))))
          (set! parallel-c
                (jsx-create-point parallel-board
                                  (jsx-parents -1 3)
                                  (js-object (vector (vector "name" "C")))))
          (set! parallel-line
                (jsx-create-line parallel-board
                                 (jsx-parents parallel-a parallel-b)
                                 (js-object (vector (vector "strokeColor" "#2b6cb0")))))
          (set! parallel-object
                (jsx-create-parallel parallel-board
                                     (jsx-parents parallel-line parallel-c)
                                     (js-object (vector (vector "strokeColor" "#d53f8c")))))
          (set! parallel-board-ready? #t))
        (void (jsx-board-full-update! parallel-board))
        #t)))

;; init-perpendicular-board! : -> boolean?
;;   Build the JSXGraph perpendicular board once the browser assets have loaded.
(define (init-perpendicular-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless perpendicular-board-ready?
          (console-log "Perpendicular board")
          (set! perpendicular-board
                (jsx-create-board
                 perpendicular-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! perpendicular-a
                (jsx-create-point perpendicular-board
                                  (jsx-parents -4 2)
                                  (js-object (vector (vector "name" "A")))))
          (set! perpendicular-b
                (jsx-create-point perpendicular-board
                                  (jsx-parents 4 1)
                                  (js-object (vector (vector "name" "B")))))
          (set! perpendicular-c
                (jsx-create-point perpendicular-board
                                  (jsx-parents -1 -3)
                                  (js-object (vector (vector "name" "C")))))
          (set! perpendicular-line
                (jsx-create-line perpendicular-board
                                 (jsx-parents perpendicular-a perpendicular-b)
                                 (js-object (vector (vector "strokeColor" "#2b6cb0")))))
          (set! perpendicular-object
                (jsx-create-perpendicular perpendicular-board
                                          (jsx-parents perpendicular-line perpendicular-c)
                                          (js-object (vector (vector "strokeColor" "#d53f8c")))))
          (set! perpendicular-board-ready? #t))
        (void (jsx-board-full-update! perpendicular-board))
        #t)))

;; init-reflection-board! : -> boolean?
;;   Build the JSXGraph reflection board once the browser assets have loaded.
(define (init-reflection-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless reflection-board-ready?
          (console-log "Reflection board")
          (set! reflection-board
                (jsx-create-board
                 reflection-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! reflection-a
                (jsx-create-point reflection-board
                                  (jsx-parents -4 2)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 4)))))
          (set! reflection-b
                (jsx-create-point reflection-board
                                  (jsx-parents 4 1)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 4)))))
          (set! reflection-c
                (jsx-create-point reflection-board
                                  (jsx-parents -1 -3)
                                  (js-object (vector (vector "name" "C")
                                                     (vector "size" 4)))))
          (set! reflection-line
                (jsx-create-line reflection-board
                                 (jsx-parents reflection-a reflection-b)
                                 (js-object (vector (vector "strokeColor" "#2b6cb0")))))
          (set! reflection-object
                (jsx-create-reflection reflection-board
                                       (jsx-parents reflection-c reflection-line)
                                       (js-object (vector (vector "name" "D")
                                                          (vector "size" 4)
                                                          (vector "fillColor" "#c53030")
                                                          (vector "strokeColor" "#9b2c2c")))))
          (set! reflection-segment
                (jsx-create-segment reflection-board
                                    (jsx-parents reflection-c reflection-object)
                                    (js-object (vector (vector "strokeColor" "#4a5568")
                                                       (vector "strokeWidth" 2)))))
          (set! reflection-board-ready? #t))
        (void (jsx-board-full-update! reflection-board))
        #t)))

;; init-bisector-board! : -> boolean?
;;   Build the JSXGraph bisector board once the browser assets have loaded.
(define (init-bisector-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless bisector-board-ready?
          (console-log "Bisector board")
          (set! bisector-board
                (jsx-create-board
                 bisector-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! bisector-a
                (jsx-create-point bisector-board
                                  (jsx-parents -3 2)
                                  (js-object (vector (vector "name" "A")))))
          (set! bisector-b
                (jsx-create-point bisector-board
                                  (jsx-parents 0 0)
                                  (js-object (vector (vector "name" "B")))))
          (set! bisector-c
                (jsx-create-point bisector-board
                                  (jsx-parents 3 2)
                                  (js-object (vector (vector "name" "C")))))
          (set! bisector-ab
                (jsx-create-line bisector-board
                                 (jsx-parents bisector-a bisector-b)
                                 (js-object (vector (vector "strokeColor" "#4a5568")
                                                    (vector "straightFirst" #f)
                                                    (vector "straightLast" #f)))))
          (set! bisector-bc
                (jsx-create-line bisector-board
                                 (jsx-parents bisector-b bisector-c)
                                 (js-object (vector (vector "strokeColor" "#4a5568")
                                                    (vector "straightFirst" #f)
                                                    (vector "straightLast" #f)))))
          (set! bisector-angle
                (jsx-create-angle bisector-board
                                  (jsx-parents bisector-a bisector-b bisector-c)
                                  (js-object (vector (vector "fillOpacity" 0.12)
                                                     (vector "strokeColor" "#4a5568")))))
          (set! bisector-object
                (jsx-create-bisector bisector-board
                                     (jsx-parents bisector-a bisector-b bisector-c)
                                     (js-object (vector (vector "strokeColor" "#d53f8c")))))
          (set! bisector-board-ready? #t))
        (void (jsx-board-full-update! bisector-board))
        #t)))

;; init-checkbox-board! : -> boolean?
;;   Build the JSXGraph checkbox board once the browser assets have loaded.
(define (init-checkbox-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless checkbox-board-ready?
          (console-log "Checkbox board")
          (set! checkbox-board
                (jsx-create-board
                 checkbox-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (console-log "Checkbox board: create checkbox")
          (set! checkbox-object
                (jsx-create-checkbox
                 checkbox-board
                 (jsx-parents -2 1 "Show it")
                 (js-object
                  (vector (vector "name" "check")
                          (vector "checked" #t)))))
          (console-log "Checkbox board: create status")
          (set! checkbox-status
                (jsx-create-text
                 checkbox-board
                 (jsx-parents
                  -2
                  0
                  (procedure->external
                   (lambda args
                     (if (jsx-checkbox-value checkbox-object)
                         "Red point is shown."
                         "Red point is hidden."))))
                 (js-object
                  (vector (vector "fontSize" 14)
                          (vector "anchorX" "left")))))
          (console-log "Checkbox board: create point")
          (define checkbox-indicator
            (jsx-create-point
             checkbox-board
             (jsx-parents 0 0)
             (js-object
              (vector (vector "name" "P")
                      (vector "size" 6)
                      (vector "fillColor" "#c53030")
                      (vector "strokeColor" "#9b2c2c")
                      (vector "visible"
                              (procedure->external
                               (lambda args
                                 (jsx-checkbox-value checkbox-object))))))))
          (console-log "Checkbox board: ready")
          (set! checkbox-board-ready? #t))
        (void (jsx-board-full-update! checkbox-board))
        #t)))

;; init-input-board! : -> boolean?
;;   Build the JSXGraph input board once the browser assets have loaded.
(define (init-input-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless input-board-ready?
          (console-log "Input board")
          (set! input-board
                (jsx-create-board
                 input-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
            (define (input-evaluate-expression x)
              (define expr
                (string-replace (jsx-input-value input-object) "^" "**"))
              (define js-code
                (format "(function(x){ return (~a); })(~a)" expr x))
              (with-handlers ([exn:fail? (lambda (exn) 0)])
                (js-send/value (js-global-this) 'eval (vector js-code))))
          (set! input-object
                (jsx-create-input
                 input-board
                 (jsx-parents -3.2 3.1 "f(x)" "Enter function")
                 (js-object
                  (vector (vector "cssStyle" "width: 10em")))))
          (set! input-graph
                (jsx-create-functiongraph
                 input-board
                 (jsx-parents input-evaluate-expression -4 4)
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")
                          (vector "strokeWidth" 3)))))
          (set! input-board-ready? #t))
        (void (jsx-board-full-update! input-board))
        #t)))

;; init-slider-board! : -> boolean?
;;   Build the JSXGraph slider board once the browser assets have loaded.
(define (init-slider-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless slider-board-ready?
          (console-log "Slider board")
          (set! slider-board
                (jsx-create-board
                 slider-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 3 6 -3])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! slider-object
                (jsx-create-slider
                 slider-board
                 (jsx-parents (jsx-parents -3 -1)
                              (jsx-parents 3 -1)
                              (jsx-parents -1 0 2))
                 (js-object
                  (vector (vector "name" "t")
                          (vector "strokeColor" "black")
                          (vector "fillColor" "white")))))
          (set! slider-board-ready? #t))
        (void (jsx-board-full-update! slider-board))
        #t)))

;; init-smartlabel-board! : -> boolean?
;;   Build the JSXGraph smartlabel board once the browser assets have loaded.
(define (init-smartlabel-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless smartlabel-board-ready?
          (console-log "Smartlabel board")
          (set! smartlabel-board
                (jsx-create-board
                 smartlabel-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! smartlabel-point
                (jsx-create-point
                 smartlabel-board
                 (jsx-parents 0 0)
                 (js-object
                  (vector (vector "name" "P")
                          (vector "size" 4)))))
          (set! smartlabel-object
                (jsx-create-smartlabel
                 smartlabel-board
                 (jsx-parents smartlabel-point)
                 (js-object
                  (vector (vector "digits" 1)
                          (vector "unit" "m")
                          (vector "dir" "col")
                          (vector "useMathJax" #f)))))
          (set! smartlabel-board-ready? #t))
        (void (jsx-board-full-update! smartlabel-board))
        #t)))

;; init-text-board! : -> boolean?
;;   Build the JSXGraph text board once the browser assets have loaded.
(define (init-text-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless text-board-ready?
          (console-log "Text board")
          (set! text-board
                (jsx-create-board
                 text-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! text-object
                (jsx-create-text
                 text-board
                 (jsx-parents -3 1 "Text board: this label shows a plain JSXGraph text object.")
                 (js-object
                  (vector (vector "fontSize" 14)
                          (vector "anchorX" "left")))))
          (set! text-board-ready? #t))
        (void (jsx-board-full-update! text-board))
        #t)))

;; init-foreignobject-board! : -> boolean?
;;   Build the JSXGraph foreign object board once the browser assets have loaded.
(define (init-foreignobject-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless foreignobject-board-ready?
          (console-log "ForeignObject board")
          (set! foreignobject-board
                (jsx-create-board
                 foreignobject-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 4 6 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! foreignobject-object
                (jsx-create-foreignobject
                 foreignobject-board
                 (jsx-parents
                  "<div style='padding:0.5rem 0.75rem;background:#e6fffa;border:1px solid #38b2ac;border-radius:0.5rem;'>ForeignObject board</div>"
                  (jsx-parents -3 1)
                  (jsx-parents 220 80))
                 (js-object
                  (vector (vector "name" "foreignObject")))))
          (set! foreignobject-board-ready? #t))
        (void (jsx-board-full-update! foreignobject-board))
        #t)))

;; init-tapemeasure-board! : -> boolean?
;;   Build the JSXGraph tapemeasure board once the browser assets have loaded.
(define (init-tapemeasure-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless tapemeasure-board-ready?
          (console-log "Tapemeasure board")
          (set! tapemeasure-board
                (jsx-create-board
                 tapemeasure-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 4 6 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! tapemeasure-a
                (jsx-create-point tapemeasure-board
                                  (jsx-parents -3 -1)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 4)))))
          (set! tapemeasure-b
                (jsx-create-point tapemeasure-board
                                  (jsx-parents 3 1)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 4)))))
          (set! tapemeasure-object
                (jsx-create-tapemeasure
                 tapemeasure-board
                 (jsx-parents (jsx-parents -3 -1)
                              (jsx-parents 3 1))
                 (js-object
                  (vector (vector "name" "tape")))))
          (set! tapemeasure-board-ready? #t))
        (void (jsx-board-full-update! tapemeasure-board))
        #t)))

;; init-measurement-board! : -> boolean?
;;   Build the JSXGraph measurement board once the browser assets have loaded.
(define (init-measurement-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless measurement-board-ready?
          (console-log "Measurement board")
          (set! measurement-board
                (jsx-create-board
                 measurement-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 5 6 -4])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! measurement-a
                (jsx-create-point measurement-board
                                  (jsx-parents -2 1)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 4)))))
          (set! measurement-b
                (jsx-create-point measurement-board
                                  (jsx-parents 2 1)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 4)))))
          (set! measurement-circle
                (jsx-create-circle measurement-board
                                   (jsx-parents measurement-a measurement-b)
                                   (js-object
                                    (vector (vector "strokeColor" "#2b6cb0")
                                            (vector "strokeWidth" 3)))))
          (set! measurement-object
                (jsx-create-measurement
                 measurement-board
                 (jsx-parents 0 -2 (jsx-parents "Radius" measurement-circle))
                 (js-object
                  (vector (vector "name" "radius")))))
          (set! measurement-board-ready? #t))
        (void (jsx-board-full-update! measurement-board))
        #t)))

;; init-circumcenter-board! : -> boolean?
;;   Build the JSXGraph circumcenter board once the browser assets have loaded.
(define (init-circumcenter-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless circumcenter-board-ready?
          (console-log "Circumcenter board")
          (set! circumcenter-board
                (jsx-create-board
                 circumcenter-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! circumcenter-a
                (jsx-create-point circumcenter-board
                                  (jsx-parents -3 -1)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 4)))))
          (set! circumcenter-b
                (jsx-create-point circumcenter-board
                                  (jsx-parents 0 3)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 4)))))
          (set! circumcenter-c
                (jsx-create-point circumcenter-board
                                  (jsx-parents 3 -1)
                                  (js-object (vector (vector "name" "C")
                                                     (vector "size" 4)))))
          (set! circumcenter-triangle
                (jsx-create-polygon circumcenter-board
                                    (jsx-parents circumcenter-a
                                                 circumcenter-b
                                                 circumcenter-c)
                                    (js-object
                                     (vector (vector "fillOpacity" 0.05)
                                             (vector "strokeColor" "#4a5568")))))
          (set! circumcenter-object
                (jsx-create-circumcenter
                 circumcenter-board
                 (jsx-parents circumcenter-a circumcenter-b circumcenter-c)
                 (js-object
                  (vector (vector "name" "O")
                          (vector "size" 4)
                          (vector "strokeColor" "#d53f8c")
                          (vector "fillColor" "#d53f8c")))))
          (set! circumcenter-board-ready? #t))
        (void (jsx-board-full-update! circumcenter-board))
        #t)))

;; init-mirrorelement-board! : -> boolean?
;;   Build the JSXGraph mirror element board once the browser assets have loaded.
(define (init-mirrorelement-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless mirrorelement-board-ready?
          (console-log "MirrorElement board")
          (set! mirrorelement-board
                (jsx-create-board
                 mirrorelement-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! mirrorelement-a
                (jsx-create-point mirrorelement-board
                                  (jsx-parents -2 1)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! mirrorelement-mirror
                (jsx-create-point mirrorelement-board
                                  (jsx-parents 0 0)
                                  (js-object (vector (vector "name" "M")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#38a169")))))
          (set! mirrorelement-object
                (jsx-create-mirrorelement
                 mirrorelement-board
                 (jsx-parents mirrorelement-a mirrorelement-mirror)
                 (js-object
                  (vector (vector "name" "D")
                          (vector "size" 5)
                          (vector "strokeColor" "#d53f8c")
                          (vector "fillColor" "#d53f8c")))))
          (set! mirrorelement-board-ready? #t))
        (void (jsx-board-full-update! mirrorelement-board))
        #t)))

;; init-mirrorpoint-board! : -> boolean?
;;   Build the JSXGraph mirror point board once the browser assets have loaded.
(define (init-mirrorpoint-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless mirrorpoint-board-ready?
          (console-log "MirrorPoint board")
          (set! mirrorpoint-board
                (jsx-create-board
                 mirrorpoint-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-5 5 5 -5])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! mirrorpoint-a
                (jsx-create-point mirrorpoint-board
                                  (jsx-parents -1 1)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! mirrorpoint-b
                (jsx-create-point mirrorpoint-board
                                  (jsx-parents 2 -1)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#38a169")))))
          (set! mirrorpoint-object
                (jsx-create-mirrorpoint
                 mirrorpoint-board
                 (jsx-parents mirrorpoint-a mirrorpoint-b)
                 (js-object
                  (vector (vector "name" "C")
                          (vector "size" 5)
                          (vector "strokeColor" "#d53f8c")
                          (vector "fillColor" "#d53f8c")))))
          (set! mirrorpoint-board-ready? #t))
        (void (jsx-board-full-update! mirrorpoint-board))
        #t)))

;; init-otherintersection-board! : -> boolean?
;;   Build the JSXGraph other-intersection board once the browser assets have loaded.
(define (init-otherintersection-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless otherintersection-board-ready?
          (console-log "OtherIntersection board")
          (set! otherintersection-board
                (jsx-create-board
                 otherintersection-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-5 5 5 -5])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! otherintersection-center-1
                (jsx-create-point otherintersection-board
                                  (jsx-parents -1 0)
                                  (js-object (vector (vector "name" "O1")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! otherintersection-through-1
                (jsx-create-point otherintersection-board
                                  (jsx-parents 1 0)
                                  (js-object (vector (vector "name" "P1")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! otherintersection-center-2
                (jsx-create-point otherintersection-board
                                  (jsx-parents 1 0)
                                  (js-object (vector (vector "name" "O2")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#38a169")))))
          (set! otherintersection-through-2
                (jsx-create-point otherintersection-board
                                  (jsx-parents -1 0)
                                  (js-object (vector (vector "name" "P2")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#38a169")))))
          (set! otherintersection-circle-1
                (jsx-create-circle otherintersection-board
                                   (jsx-parents otherintersection-center-1
                                                otherintersection-through-1)
                                   (js-object
                                    (vector (vector "name" "c1")
                                            (vector "strokeColor" "#2b6cb0")
                                            (vector "strokeWidth" 3)))))
          (set! otherintersection-circle-2
                (jsx-create-circle otherintersection-board
                                   (jsx-parents otherintersection-center-2
                                                otherintersection-through-2)
                                   (js-object
                                    (vector (vector "name" "c2")
                                            (vector "strokeColor" "#38a169")
                                            (vector "strokeWidth" 3)))))
          (set! otherintersection-primary
                (jsx-create-intersection
                 otherintersection-board
                 (jsx-parents otherintersection-circle-1
                              otherintersection-circle-2
                              0)
                 (js-object
                  (vector (vector "name" "I")
                          (vector "size" 5)
                          (vector "strokeColor" "#d69e2e")
                          (vector "fillColor" "#d69e2e")))))
          (set! otherintersection-secondary
                (jsx-create-otherintersection
                 otherintersection-board
                 (jsx-parents otherintersection-circle-1
                              otherintersection-circle-2
                              otherintersection-primary)
                 (js-object
                  (vector (vector "name" "J")
                          (vector "size" 5)
                          (vector "strokeColor" "#d53f8c")
                          (vector "fillColor" "#d53f8c")))))
          (set! otherintersection-board-ready? #t))
        (void (jsx-board-full-update! otherintersection-board))
        #t)))

;; init-orthogonalprojection-board! : -> boolean?
;;   Build the JSXGraph orthogonal projection board once the browser assets have loaded.
(define (init-orthogonalprojection-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless orthogonalprojection-board-ready?
          (console-log "Orthogonalprojection board")
          (set! orthogonalprojection-board
                (jsx-create-board
                 orthogonalprojection-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 5 6 -5])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! orthogonalprojection-line-a
                (jsx-create-point orthogonalprojection-board
                                  (jsx-parents -3 -1)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! orthogonalprojection-line-b
                (jsx-create-point orthogonalprojection-board
                                  (jsx-parents 3 1)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! orthogonalprojection-line
                (jsx-create-line orthogonalprojection-board
                                 (jsx-parents orthogonalprojection-line-a
                                              orthogonalprojection-line-b)
                                 (js-object
                                  (vector (vector "name" "l")
                                          (vector "strokeColor" "#2b6cb0")
                                          (vector "strokeWidth" 3)))))
          (set! orthogonalprojection-point
                (jsx-create-point orthogonalprojection-board
                                  (jsx-parents -1 3)
                                  (js-object (vector (vector "name" "P")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#38a169")))))
          (set! orthogonalprojection-object
                (jsx-create-orthogonalprojection
                 orthogonalprojection-board
                 (jsx-parents orthogonalprojection-point
                              orthogonalprojection-line)
                 (js-object
                  (vector (vector "name" "Q")
                          (vector "size" 5)
                          (vector "strokeColor" "#d53f8c")
                          (vector "fillColor" "#d53f8c")))))
          (set! orthogonalprojection-board-ready? #t))
        (void (jsx-board-full-update! orthogonalprojection-board))
        #t)))

;; init-parallelpoint-board! : -> boolean?
;;   Build the JSXGraph parallel point board once the browser assets have loaded.
(define (init-parallelpoint-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless parallelpoint-board-ready?
          (console-log "Parallelpoint board")
          (set! parallelpoint-board
                (jsx-create-board
                 parallelpoint-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 5 6 -5])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! parallelpoint-p1
                (jsx-create-point parallelpoint-board
                                  (jsx-parents -3 -1)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! parallelpoint-p2
                (jsx-create-point parallelpoint-board
                                  (jsx-parents -1 1)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! parallelpoint-p3
                (jsx-create-point parallelpoint-board
                                  (jsx-parents 2 -1)
                                  (js-object (vector (vector "name" "C")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#38a169")))))
          (set! parallelpoint-object
                (jsx-create-parallelpoint
                 parallelpoint-board
                 (jsx-parents parallelpoint-p1
                              parallelpoint-p2
                              parallelpoint-p3)
                 (js-object
                  (vector (vector "name" "D")
                          (vector "size" 5)
                          (vector "strokeColor" "#d53f8c")
                          (vector "fillColor" "#d53f8c")))))
          (set! parallelpoint-board-ready? #t))
        (void (jsx-board-full-update! parallelpoint-board))
        #t)))

;; init-perpendicularpoint-board! : -> boolean?
;;   Build the JSXGraph perpendicular point board once the browser assets have loaded.
(define (init-perpendicularpoint-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless perpendicularpoint-board-ready?
          (console-log "PerpendicularPoint board")
          (set! perpendicularpoint-board
                (jsx-create-board
                 perpendicularpoint-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 5 6 -5])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! perpendicularpoint-line-a
                (jsx-create-point perpendicularpoint-board
                                  (jsx-parents -3 -1)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! perpendicularpoint-line-b
                (jsx-create-point perpendicularpoint-board
                                  (jsx-parents 3 1)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! perpendicularpoint-line
                (jsx-create-line perpendicularpoint-board
                                 (jsx-parents perpendicularpoint-line-a
                                              perpendicularpoint-line-b)
                                 (js-object
                                  (vector (vector "name" "l")
                                          (vector "strokeColor" "#2b6cb0")
                                          (vector "strokeWidth" 3)))))
          (set! perpendicularpoint-point
                (jsx-create-point perpendicularpoint-board
                                  (jsx-parents -1 3)
                                  (js-object (vector (vector "name" "P")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#38a169")))))
          (set! perpendicularpoint-object
                (jsx-create-perpendicularpoint
                 perpendicularpoint-board
                 (jsx-parents perpendicularpoint-point
                              perpendicularpoint-line)
                 (js-object
                  (vector (vector "name" "Q")
                          (vector "size" 5)
                          (vector "strokeColor" "#d53f8c")
                          (vector "fillColor" "#d53f8c")))))
          (set! perpendicularpoint-board-ready? #t))
        (void (jsx-board-full-update! perpendicularpoint-board))
        #t)))

;; init-ticks-board! : -> boolean?
;;   Build the JSXGraph ticks board once the browser assets have loaded.
(define (init-ticks-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless ticks-board-ready?
          (console-log "Ticks board")
          (set! ticks-board
                (jsx-create-board
                 ticks-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 7 7 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! ticks-line
                (jsx-create-line ticks-board
                                 (jsx-parents (jsx-parents 0 3)
                                              (jsx-parents 1 3))
                                 (js-object
                                  (vector (vector "name" "l")
                                          (vector "strokeColor" "#2b6cb0")
                                          (vector "strokeWidth" 3)))))
          (set! ticks-object
                (jsx-create-ticks ticks-board
                                  (jsx-parents ticks-line 2)
                                  (js-object
                                   (vector (vector "ticksDistance" 2)
                                           (vector "majorHeight" 40)
                                           (vector "insertTicks" #f)
                                           (vector "drawLabels" #t)))))
          (set! ticks-board-ready? #t))
        (void (jsx-board-full-update! ticks-board))
        #t)))

;; init-transformation-board! : -> boolean?
;;   Build the JSXGraph transformation board once the browser assets have loaded.
(define (init-transformation-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless transformation-board-ready?
          (console-log "Transformation board")
          (set! transformation-board
                (jsx-create-board
                 transformation-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-8 8 8 -8])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! transformation-source
                (jsx-create-point transformation-board
                                  (jsx-parents 0 3)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "fillColor" "#2b6cb0")
                                                     (vector "size" 5)))))
          (set! transformation-object
                (jsx-create-transformation
                 transformation-board
                 (jsx-parents 2 0.5)
                 (js-object (vector (vector "type" "scale")))))
          (set! transformation-target
                (jsx-create-point transformation-board
                                  (jsx-parents transformation-source
                                               transformation-object)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "fillColor" "#d53f8c")
                                                     (vector "size" 5)))))
          (set! transformation-board-ready? #t))
        (void (jsx-board-full-update! transformation-board))
        #t)))

;; init-tracecurve-board! : -> boolean?
;;   Build the JSXGraph tracecurve board once the browser assets have loaded.
(define (init-tracecurve-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless tracecurve-board-ready?
          (console-log "Tracecurve board")
          (set! tracecurve-board
                (jsx-create-board
                 tracecurve-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-4 4 4 -4])
                          (vector "axis" #f)
                          (vector "keepaspectratio" #t)))))
          (set! tracecurve-circle
                (jsx-create-circle tracecurve-board
                                   (jsx-parents (jsx-parents 0 0)
                                                (jsx-parents 2 0))
                                   (js-object (vector (vector "strokeColor" "#2b6cb0")))))
          (set! tracecurve-point
                (jsx-create-point tracecurve-board
                                  (jsx-parents -3 1)
                                  (js-object (vector (vector "name" "P")
                                                     (vector "size" 4)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! tracecurve-glider
                (jsx-create-glider tracecurve-board
                                   (jsx-parents 2 1 tracecurve-circle)
                                   (js-object (vector (vector "name" "G")
                                                      (vector "size" 4)
                                                      (vector "fillColor" "#38a169")))))
          (set! tracecurve-segment
                (jsx-create-segment tracecurve-board
                                    (jsx-parents tracecurve-glider
                                                 tracecurve-point)
                                    (js-object (vector (vector "strokeColor" "#718096")))))
          (set! tracecurve-midpoint
                (jsx-create-midpoint tracecurve-board
                                     (jsx-parents tracecurve-segment)
                                     (js-object (vector (vector "name" "M")
                                                        (vector "size" 4)
                                                        (vector "fillColor" "#d53f8c")))))
          (set! tracecurve-object
                (jsx-create-tracecurve tracecurve-board
                                       (jsx-parents tracecurve-glider
                                                    tracecurve-midpoint)
                                       (js-object (vector (vector "strokeColor" "#d53f8c")
                                                          (vector "strokeWidth" 2)))))
          (set! tracecurve-board-ready? #t))
        (void (jsx-board-full-update! tracecurve-board))
        #t)))

;; init-parallelogram-board! : -> boolean?
;;   Build the JSXGraph parallelogram board once the browser assets have loaded.
(define (init-parallelogram-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless parallelogram-board-ready?
          (console-log "Parallelogram board")
          (set! parallelogram-board
                (jsx-create-board
                 parallelogram-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! parallelogram-p1
                (jsx-create-point parallelogram-board
                                  (jsx-parents -3 -4)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 5)))))
          (set! parallelogram-p2
                (jsx-create-point parallelogram-board
                                  (jsx-parents 3 -1)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 5)))))
          (set! parallelogram-p3
                (jsx-create-point parallelogram-board
                                  (jsx-parents -2 0)
                                  (js-object (vector (vector "name" "C")
                                                     (vector "size" 5)))))
          (set! parallelogram-object
                (jsx-create-parallelogram
                 parallelogram-board
                 (jsx-parents parallelogram-p1
                              parallelogram-p2
                              parallelogram-p3)
                 (js-object
                  (vector (vector "hasInnerPoints" #t)
                          (vector "parallelpoint"
                                  (js-object
                                   (vector (vector "size" 6)
                                           (vector "face" "<<>>"))))))))
          (set! parallelogram-board-ready? #t))
        (void (jsx-board-full-update! parallelogram-board))
        #t)))

;; init-reflexangle-board! : -> boolean?
;;   Build the JSXGraph reflex angle board once the browser assets have loaded.
(define (init-reflexangle-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless reflexangle-board-ready?
          (console-log "ReflexAngle board")
          (set! reflexangle-board
                (jsx-create-board
                 reflexangle-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-1 7 7 -1])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (set! reflexangle-p1
                (jsx-create-point reflexangle-board
                                  (jsx-parents 5 3)
                                  (js-object (vector (vector "name" "A")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! reflexangle-p2
                (jsx-create-point reflexangle-board
                                  (jsx-parents 1 1)
                                  (js-object (vector (vector "name" "B")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! reflexangle-p3
                (jsx-create-point reflexangle-board
                                  (jsx-parents 1 5)
                                  (js-object (vector (vector "name" "C")
                                                     (vector "size" 5)
                                                     (vector "fillColor" "#2b6cb0")))))
          (set! reflexangle-angle
                (jsx-create-reflexangle
                 reflexangle-board
                 (jsx-parents reflexangle-p1
                              reflexangle-p2
                              reflexangle-p3)
                 (js-object (vector (vector "radius" 2)
                                    (vector "strokeColor" "#d53f8c")
                                    (vector "fillColor" "#fed7d7")
                                    (vector "fillOpacity" 0.4)))))
          (set! reflexangle-text
                (jsx-create-text
                 reflexangle-board
                 (jsx-parents
                  4.2
                  4.8
                  (procedure->external
                   (lambda ()
                     (format "ReflexAngle board: angle = ~a rad."
                             (jsx-angle-value reflexangle-angle "rad")))))
                 (js-object
                  (vector (vector "fontSize" 14)
                          (vector "anchorX" "left")))))
          (set! reflexangle-board-ready? #t))
        (void (jsx-board-full-update! reflexangle-board))
        #t)))

;; init-bisectorlines-board! : -> boolean?
;;   Build the JSXGraph bisectorlines board once the browser assets have loaded.
(define (init-bisectorlines-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless bisectorlines-board-ready?
          (console-log "Bisectorlines board")
          (set! bisectorlines-board
                (jsx-create-board
                 bisectorlines-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-5 5 5 -5])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (define bisectorlines-p1
            (jsx-create-point bisectorlines-board
                              (jsx-parents -4 2)
                              (js-object (vector (vector "name" "A")
                                                 (vector "size" 4)
                                                 (vector "fillColor" "#2b6cb0")))))
          (define bisectorlines-p2
            (jsx-create-point bisectorlines-board
                              (jsx-parents 3 1)
                              (js-object (vector (vector "name" "B")
                                                 (vector "size" 4)
                                                 (vector "fillColor" "#2b6cb0")))))
          (define bisectorlines-p3
            (jsx-create-point bisectorlines-board
                              (jsx-parents -2 -3)
                              (js-object (vector (vector "name" "C")
                                                 (vector "size" 4)
                                                 (vector "fillColor" "#d53f8c")))))
          (define bisectorlines-p4
            (jsx-create-point bisectorlines-board
                              (jsx-parents 4 -2)
                              (js-object (vector (vector "name" "D")
                                                 (vector "size" 4)
                                                 (vector "fillColor" "#d53f8c")))))
          (set! bisectorlines-line-1
                (jsx-create-line bisectorlines-board
                                 (jsx-parents bisectorlines-p1 bisectorlines-p2)
                                 (js-object (vector (vector "strokeColor" "#2b6cb0")
                                                    (vector "strokeWidth" 2)))))
          (set! bisectorlines-line-2
                (jsx-create-line bisectorlines-board
                                 (jsx-parents bisectorlines-p3 bisectorlines-p4)
                                 (js-object (vector (vector "strokeColor" "#d53f8c")
                                                    (vector "strokeWidth" 2)))))
          (set! bisectorlines-object
                (jsx-create-bisectorlines
                 bisectorlines-board
                 (jsx-parents bisectorlines-line-1 bisectorlines-line-2)
                 (js-object (vector (vector "name" "BL")))))
          (set! bisectorlines-board-ready? #t))
        (void (jsx-board-full-update! bisectorlines-board))
        #t)))

;; init-perpendicularsegment-board! : -> boolean?
;;   Build the JSXGraph perpendicular segment board once the browser assets have loaded.
(define (init-perpendicularsegment-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless perpendicularsegment-board-ready?
          (console-log "PerpendicularSegment board")
          (set! perpendicularsegment-board
                (jsx-create-board
                 perpendicularsegment-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-5 5 5 -5])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (define perpendicularsegment-line-a
            (jsx-create-point perpendicularsegment-board
                              (jsx-parents -4 -1)
                              (js-object (vector (vector "name" "A")
                                                 (vector "size" 4)))))
          (define perpendicularsegment-line-b
            (jsx-create-point perpendicularsegment-board
                              (jsx-parents 4 1)
                              (js-object (vector (vector "name" "B")
                                                 (vector "size" 4)))))
          (define perpendicularsegment-point
            (jsx-create-point perpendicularsegment-board
                              (jsx-parents 0 3)
                              (js-object (vector (vector "name" "C")
                                                 (vector "size" 4)
                                                 (vector "fillColor" "#d53f8c")))))
          (define perpendicularsegment-line
            (jsx-create-line perpendicularsegment-board
                             (jsx-parents perpendicularsegment-line-a
                                          perpendicularsegment-line-b)
                             (js-object (vector (vector "strokeColor" "#2b6cb0")
                                                (vector "strokeWidth" 2)))))
          (set! perpendicularsegment-object
                (jsx-create-perpendicularsegment
                 perpendicularsegment-board
                 (jsx-parents perpendicularsegment-line
                              perpendicularsegment-point)
                 (js-object (vector (vector "strokeColor" "#d53f8c")
                                    (vector "strokeWidth" 2)))))
          (set! perpendicularsegment-board-ready? #t))
        (void (jsx-board-full-update! perpendicularsegment-board))
        #t)))

;; init-constructions-board! : -> boolean?
;;   Build the JSXGraph construction board once the browser assets have loaded.
(define (init-constructions-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless constructions-board-ready?
          (console-log "Constructions board")
          (set! constructions-board
                (jsx-create-board
                 constructions-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-8 8 8 -8])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (let ()
            (define conic-board-object
              (jsx-create-conic constructions-board
                                (jsx-parents 1 2 -4 0 0 0)))
            (define ellipse-a (jsx-create-point constructions-board
                                                (jsx-parents -4 4)
                                                (js-object (vector (vector "name" "A")
                                                                   (vector "size" 4)))))
            (define ellipse-b (jsx-create-point constructions-board
                                                (jsx-parents -1 5)
                                                (js-object (vector (vector "name" "B")
                                                                   (vector "size" 4)))))
            (define ellipse-c (jsx-create-point constructions-board
                                                (jsx-parents 2 4)
                                                (js-object (vector (vector "name" "C")
                                                                   (vector "size" 4)))))
            (define ellipse-board-object
              (jsx-create-ellipse constructions-board
                                  (jsx-parents ellipse-a ellipse-b ellipse-c)))
            (define functiongraph-board-object
              (jsx-create-functiongraph
               constructions-board
               (jsx-parents (lambda (x) (- (* 0.4 x x) (* 1.5 x)))
                            -3
                            4)))
            (define polygon-a (jsx-create-point constructions-board
                                               (jsx-parents -6 -4)
                                               (js-object (vector (vector "name" "D")
                                                                  (vector "size" 4)))))
            (define polygon-b (jsx-create-point constructions-board
                                               (jsx-parents -4 -1)
                                               (js-object (vector (vector "name" "E")
                                                                  (vector "size" 4)))))
            (define polygon-c (jsx-create-point constructions-board
                                               (jsx-parents -2 -4)
                                               (js-object (vector (vector "name" "F")
                                                                  (vector "size" 4)))))
            (define polygon-d (jsx-create-point constructions-board
                                               (jsx-parents -3 -6)
                                               (js-object (vector (vector "name" "G")
                                                                  (vector "size" 4)))))
            (define polygon-board-object
              (jsx-create-polygon constructions-board
                                  (jsx-parents polygon-a polygon-b polygon-c polygon-d)))
            (define midpoint-a (jsx-create-point constructions-board
                                                 (jsx-parents 3 5)
                                                 (js-object (vector (vector "name" "H")
                                                                    (vector "size" 4)))))
            (define midpoint-b (jsx-create-point constructions-board
                                                 (jsx-parents 5 2)
                                                 (js-object (vector (vector "name" "I")
                                                                    (vector "size" 4)))))
            (define midpoint-board-object
              (jsx-create-midpoint constructions-board
                                   (jsx-parents midpoint-a midpoint-b)))
            (define relation-a (jsx-create-point constructions-board
                                                 (jsx-parents 1 0)
                                                 (js-object (vector (vector "name" "J")
                                                                    (vector "size" 4)))))
            (define relation-b (jsx-create-point constructions-board
                                                 (jsx-parents 4 1)
                                                 (js-object (vector (vector "name" "K")
                                                                    (vector "size" 4)))))
            (define relation-c (jsx-create-point constructions-board
                                                 (jsx-parents 2 4)
                                                 (js-object (vector (vector "name" "L")
                                                                    (vector "size" 4)))))
            (define relation-d (jsx-create-point constructions-board
                                                 (jsx-parents 5 4)
                                                 (js-object (vector (vector "name" "M")
                                                                    (vector "size" 4)))))
            (define relation-e (jsx-create-point constructions-board
                                                 (jsx-parents 1 -3)
                                                 (js-object (vector (vector "name" "N")
                                                                    (vector "size" 4)))))
            (define relation-f (jsx-create-point constructions-board
                                                 (jsx-parents 4 -1)
                                                 (js-object (vector (vector "name" "O")
                                                                    (vector "size" 4)))))
            (define relation-line (jsx-create-line constructions-board
                                                   (jsx-parents relation-a relation-b)))
            (define relation-line-2 (jsx-create-line constructions-board
                                                     (jsx-parents relation-c relation-d)))
            (define relation-circle (jsx-create-circle constructions-board
                                                       (jsx-parents relation-e relation-f)))
            (define relation-parallel
              (jsx-create-parallel constructions-board
                                   (jsx-parents relation-line relation-c)))
            (define relation-perpendicular
              (jsx-create-perpendicular constructions-board
                                        (jsx-parents relation-line relation-d)))
            (define relation-reflection
              (jsx-create-reflection constructions-board
                                     (jsx-parents relation-c relation-line)))
            (define relation-bisector
              (jsx-create-bisector constructions-board
                                   (jsx-parents relation-a relation-c relation-e)))
            (define relation-normal
              (jsx-create-normal constructions-board
                                 (jsx-parents relation-circle relation-d)))
            (define relation-intersection
              (jsx-create-intersection constructions-board
                                       (jsx-parents relation-line relation-line-2)))
            (void conic-board-object)
            (void ellipse-board-object)
            (void functiongraph-board-object)
            (void polygon-board-object)
            (void midpoint-board-object)
            (void relation-parallel)
            (void relation-perpendicular)
            (void relation-reflection)
            (void relation-bisector)
            (void relation-normal)
            (void relation-intersection)
          )
          (set! constructions-board-ready? #t))
        (void (jsx-board-full-update! constructions-board))
        #t)))

;; init-widgets-board! : -> boolean?
;;   Build the JSXGraph widgets board once the browser assets have loaded.
(define (init-widgets-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless widgets-board-ready?
          (console-log "Widgets board")
          (set! widgets-board
                (jsx-create-board
                 widgets-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (let ()
            (define (widget-evaluate-expression x)
              (define expr (string-replace widget-expression "^" "**"))
              (define js-code
                (format "(function(x){ return (~a); })(~a)" expr x))
              (with-handlers ([exn:fail? (lambda (exn) 0)])
                (js-send/value (js-global-this) 'eval (vector js-code))))
            (define (widget-update-graph!)
              (when widget-graph
                (jsx-element-update! widget-graph)
                (if (and widget-graph-visible?
                         (jsx-checkbox-value widget-checkbox))
                    (jsx-element-show! widget-graph)
                    (jsx-element-hide! widget-graph))))
            (define (widget-update-button-label!)
              (when widget-button-label
                (jsx-element-set-label-text!
                 widget-button-label
                 (if widget-graph-visible?
                     "Hide graph"
                     "Show graph"))))
            (set! widget-button
              (jsx-create-button
               widgets-board
               (jsx-parents -4 4 "Click me"
                            (procedure->external
                             (lambda args
                               (set! widget-expression (jsx-input-value widget-input))
                               (set! widget-graph-visible? (not widget-graph-visible?))
                               (widget-update-graph!))))
               (js-object
                (vector (vector "name" "button")))))
            (console-log "Widgets board: button ready")
            (set! widget-button-label widget-button)
            (define widget-checkbox
              (jsx-create-checkbox widgets-board
                                   (jsx-parents -4 2 "Show me")
                                   (js-object
                                    (vector (vector "name" "checkbox")))))
            (console-log "Widgets board: checkbox ready")
            (define widget-input
              (jsx-create-input widgets-board
                                (jsx-parents -4 0 "Math.sin(x)" "f(x)=")
                                (js-object
                                 (vector (vector "cssStyle" "width: 5em")))))
            (console-log "Widgets board: input ready")
            (define widget-slider
              (jsx-create-slider widgets-board
                                 (jsx-parents (jsx-parents -5 -3)
                                              (jsx-parents 1 -3)
                                              (jsx-parents -2 0 2))
                                 (js-object
                                  (vector (vector "name" "t")
                                          (vector "strokeColor" "black")
                                          (vector "fillColor" "white")))))
            (console-log "Widgets board: slider ready")
            (define widget-text
              (jsx-create-text widgets-board
                               (jsx-parents -4 -5 "Widget board: enter a JavaScript expression, click the button to toggle the graph, and use the checkbox to show or hide it.")
                               (js-object
                                (vector (vector "fontSize" 14)
                                        (vector "anchorX" "left")))))
            (console-log "Widgets board: text ready")
            (set! widget-graph
                  (jsx-create-functiongraph
                   widgets-board
                   (jsx-parents widget-evaluate-expression -5 5)
                   (js-object
                    (vector (vector "strokeColor" "#2b6cb0")
                            (vector "strokeWidth" 3)))))
            (console-log "Widgets board: graph ready")
            (void widget-button)
            (void widget-checkbox)
            (void widget-input)
            (void widget-slider)
            (void widget-text)
            (void widget-graph)
            (widget-update-button-label!))
          (set! widgets-board-ready? #t))
        (void (jsx-board-full-update! widgets-board))
        #t)))

;; init-annotation-board! : -> boolean?
;;   Build the JSXGraph annotation board once the browser assets have loaded.
(define (init-annotation-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless annotation-board-ready?
          (console-log "Annotation board")
          (set! annotation-board
                (jsx-create-board
                 annotation-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #t)
                          (vector "keepaspectratio" #t)))))
          (let ()
            (set! annotation-slider
                  (jsx-create-slider
                   annotation-board
                   (jsx-parents (jsx-parents -5 -5)
                                (jsx-parents 2 -5)
                                (jsx-parents 0 0 5))
                   (js-object
                    (vector (vector "name" "v")
                            (vector "strokeColor" "black")
                            (vector "fillColor" "white")))))
            (set! annotation-text
                  (jsx-create-text
                   annotation-board
                   (jsx-parents
                    -5
                    4
                    (procedure->external
                     (lambda ()
                       (format "Annotation board: slider value = ~a.~nMove the slider below to update this text."
                               (jsx-slider-value annotation-slider)))))
                   (js-object
                    (vector (vector "fontSize" 14)
                            (vector "anchorX" "left")))))
            (define annotation-image
              (jsx-create-image
               annotation-board
               (jsx-parents
                "https://jsxgraph.org/jsxgraph/distrib/images/uccellino.jpg"
                (jsx-parents -4 -3)
                (jsx-parents 2 2))
               (js-object
                (vector (vector "name" "image")))))
            (void annotation-text)
            (void annotation-image))
          (set! annotation-board-ready? #t))
        (void (jsx-board-full-update! annotation-board))
        #t)))

;; init-view3d-board! : -> boolean?
;;   Build the JSXGraph view3d board once the browser assets have loaded.
(define (init-view3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless view3d-board-ready?
          (console-log "View3D board")
          (set! view3d-board
                (jsx-create-board
                 view3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! view3d-view
                (jsx-create-view3d
                 view3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (js-object
                  (vector (vector "projection" "parallel")
                          (vector "axesPosition" "none")
                          (vector "trackball"
                                  (js-object (vector (vector "enabled" #t))))))))
          (set! view3d-board-ready? #t))
        (void (jsx-board-full-update! view3d-board))
        #t)))

;; init-point3d-board! : -> boolean?
;;   Build the JSXGraph point3d board once the browser assets have loaded.
(define (init-point3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless point3d-board-ready?
          (console-log "Point3D board")
          (set! point3d-board
                (jsx-create-board
                 point3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! point3d-view
                (jsx-create-view3d
                 point3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! point3d-point
                (jsx-view3d-create-point3d
                 point3d-view
                 (vector 1 2 2)
                 (js-object
                  (vector (vector "name" "A")
                          (vector "size" 5)
                          (vector "fillColor" "#c53030")
                          (vector "strokeColor" "#9b2c2c")))))
          (set! point3d-board-ready? #t))
        (void (jsx-board-full-update! point3d-board))
        #t)))

;; init-line3d-board! : -> boolean?
;;   Build the JSXGraph line3d board once the browser assets have loaded.
(define (init-line3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless line3d-board-ready?
          (console-log "Line3D board")
          (set! line3d-board
                (jsx-create-board
                 line3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! line3d-view
                (jsx-create-view3d
                 line3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! line3d-point-1
                (jsx-view3d-create-point3d
                 line3d-view
                 (vector 1 3 3)
                 (js-object
                  (vector (vector "name" "A")
                          (vector "size" 4)
                          (vector "visible" #t)))))
          (set! line3d-point-2
                (jsx-view3d-create-point3d
                 line3d-view
                 (vector -3 -3 -3)
                 (js-object
                  (vector (vector "name" "B")
                          (vector "size" 4)
                          (vector "visible" #t)))))
          (set! line3d-line
                (jsx-view3d-create-line3d
                 line3d-view
                 (jsx-parents line3d-point-1 line3d-point-2)
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")
                          (vector "strokeWidth" 3)
                          (vector "point1"
                                  (js-object (vector (vector "visible" #t))))
                          (vector "point2"
                                  (js-object (vector (vector "visible" #t))))))))
          (set! line3d-board-ready? #t))
        (void (jsx-board-full-update! line3d-board))
        #t)))

;; init-circle3d-board! : -> boolean?
;;   Build the JSXGraph circle3d board once the browser assets have loaded.
(define (init-circle3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless circle3d-board-ready?
          (console-log "Circle3D board")
          (set! circle3d-board
                (jsx-create-board
                 circle3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! circle3d-view
                (jsx-create-view3d
                 circle3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! circle3d-center
                (jsx-view3d-create-point3d
                 circle3d-view
                 (vector 0 0 0)
                 (js-object
                  (vector (vector "name" "O")
                          (vector "size" 4)
                          (vector "fillColor" "#2b6cb0")
                          (vector "strokeColor" "#2c5282")))))
          (set! circle3d-point
                (jsx-view3d-create-point3d
                 circle3d-view
                 (vector 2 0 0)
                 (js-object
                  (vector (vector "name" "P")
                          (vector "size" 4)
                          (vector "fillColor" "#c53030")
                          (vector "strokeColor" "#9b2c2c")))))
          (set! circle3d-circle
                (jsx-view3d-create-circle3d
                 circle3d-view
                 (jsx-parents circle3d-center
                              (vector 0 0 0 1)
                              2)
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")
                          (vector "fillOpacity" 0.08)))))
          (set! circle3d-board-ready? #t))
        (void (jsx-board-full-update! circle3d-board))
        #t)))

;; init-plane3d-board! : -> boolean?
;;   Build the JSXGraph plane3d board once the browser assets have loaded.
(define (init-plane3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless plane3d-board-ready?
          (console-log "Plane3D board")
          (set! plane3d-board
                (jsx-create-board
                 plane3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! plane3d-view
                (jsx-create-view3d
                 plane3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! plane3d-point-1
                (jsx-view3d-create-point3d
                 plane3d-view
                 (vector -4 -2 -1)
                 (js-object
                  (vector (vector "name" "A")
                          (vector "size" 6)
                          (vector "fillColor" "#2b6cb0")
                          (vector "strokeColor" "#2c5282")))))
          (set! plane3d-point-2
                (jsx-view3d-create-point3d
                 plane3d-view
                 (vector 2 -1 1)
                 (js-object
                  (vector (vector "name" "B")
                          (vector "size" 6)
                          (vector "fillColor" "#c53030")
                          (vector "strokeColor" "#9b2c2c")))))
          (set! plane3d-point-3
                (jsx-view3d-create-point3d
                 plane3d-view
                 (vector 0 3 0)
                 (js-object
                  (vector (vector "name" "C")
                          (vector "size" 6)
                          (vector "fillColor" "#38a169")
                          (vector "strokeColor" "#276749")))))
          (set! plane3d-footprint
                (jsx-view3d-create
                 plane3d-view
                 "polygon3d"
                 (jsx-parents plane3d-point-1
                              plane3d-point-2
                              plane3d-point-3)
                 (js-object
                  (vector (vector "fillColor" "#f6e05e")
                          (vector "fillOpacity" 0.7)
                          (vector "strokeColor" "#975a16")
                          (vector "strokeWidth" 5)))))
          (set! plane3d-plane
                (jsx-view3d-create-plane3d
                 plane3d-view
                 (jsx-parents plane3d-point-1
                              plane3d-point-2
                              plane3d-point-3)
                 (js-object
                  (vector (vector "fillColor" "#f6ad55")
                          (vector "fillOpacity" 0.92)
                          (vector "strokeColor" "#b45309")
                          (vector "strokeWidth" 4)))))
          (set! plane3d-board-ready? #t))
        (void (jsx-board-full-update! plane3d-board))
        #t)))

;; init-axes3d-board! : -> boolean?
;;   Build the JSXGraph axes3d board once the browser assets have loaded.
(define (init-axes3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless axes3d-board-ready?
          (console-log "Axes3D board")
          (set! axes3d-board
                (jsx-create-board
                 axes3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! axes3d-view
                (jsx-create-view3d
                 axes3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! axes3d-axes
                (jsx-view3d-create-axes3d
                 axes3d-view
                 (jsx-parents)
                 (js-object
                  (vector (vector "axesPosition" "center")
                          (vector "strokeColor" "#2b6cb0")
                          (vector "fillOpacity" 0)
                          (vector "highlightFillOpacity" 0)
                          (vector "strokeOpacity" 0)
                          (vector "highlightStrokeOpacity" 0)))))
          (set! axes3d-board-ready? #t))
        (void (jsx-board-full-update! axes3d-board))
        #t)))

;; init-axis3d-board! : -> boolean?
;;   Build the JSXGraph axis3d board once the browser assets have loaded.
(define (init-axis3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless axis3d-board-ready?
          (console-log "Axis3D board")
          (set! axis3d-board
                (jsx-create-board
                 axis3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! axis3d-view
                (jsx-create-view3d
                 axis3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! axis3d-axis
                (jsx-view3d-create-axis3d
                 axis3d-view
                 (jsx-parents (vector -2 -2 -2)
                              (vector 3 3 3))
                 (js-object
                  (vector (vector "strokeColor" "#c53030")
                          (vector "strokeWidth" 3)))))
          (set! axis3d-board-ready? #t))
        (void (jsx-board-full-update! axis3d-board))
        #t)))

;; init-functiongraph3d-board! : -> boolean?
;;   Build the JSXGraph functiongraph3d board once the browser assets have loaded.
(define (init-functiongraph3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless functiongraph3d-board-ready?
          (console-log "Functiongraph3D board")
          (set! functiongraph3d-board
                (jsx-create-board
                 functiongraph3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! functiongraph3d-view
                (jsx-create-view3d
                 functiongraph3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! functiongraph3d-surface
                (jsx-view3d-create-functiongraph3d
                 functiongraph3d-view
                 (jsx-parents
                  (lambda (x y) (sin (/ (* x y) 4)))
                  (vector -5 5)
                  (vector -5 5))
                 (js-object
                  (vector (vector "strokeWidth" 0.5)
                          (vector "stepsU" 24)
                          (vector "stepsV" 24)))))
          (set! functiongraph3d-board-ready? #t))
        (void (jsx-board-full-update! functiongraph3d-board))
        #t)))

;; init-intersectioncircle3d-board! : -> boolean?
;;   Build the JSXGraph intersectioncircle3d board once the browser assets have loaded.
(define (init-intersectioncircle3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless intersectioncircle3d-board-ready?
          (console-log "IntersectionCircle3D board")
          (set! intersectioncircle3d-board
                (jsx-create-board
                 intersectioncircle3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! intersectioncircle3d-view
                (jsx-create-view3d
                 intersectioncircle3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! intersectioncircle3d-sphere-1
                (jsx-view3d-create-sphere3d
                 intersectioncircle3d-view
                 (jsx-parents (vector -1 0 0) 2)
                 (js-object
                  (vector (vector "fillColor" "#68d391")
                          (vector "fillOpacity" 0.25)))))
          (set! intersectioncircle3d-sphere-2
                (jsx-view3d-create-sphere3d
                 intersectioncircle3d-view
                 (jsx-parents (vector 1 0 0) 2)
                 (js-object
                  (vector (vector "fillColor" "#fc8181")
                          (vector "fillOpacity" 0.25)))))
          (set! intersectioncircle3d-intersection
                (jsx-view3d-create-intersectioncircle3d
                 intersectioncircle3d-view
                 (jsx-parents intersectioncircle3d-sphere-1
                              intersectioncircle3d-sphere-2)
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")
                          (vector "strokeWidth" 3)))))
          (set! intersectioncircle3d-board-ready? #t))
        (void (jsx-board-full-update! intersectioncircle3d-board))
        #t)))

;; init-intersectionline3d-board! : -> boolean?
;;   Build the JSXGraph intersectionline3d board once the browser assets have loaded.
(define (init-intersectionline3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless intersectionline3d-board-ready?
          (console-log "IntersectionLine3D board")
          (set! intersectionline3d-board
                (jsx-create-board
                 intersectionline3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! intersectionline3d-view
                (jsx-create-view3d
                 intersectionline3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! intersectionline3d-plane-1
                (jsx-view3d-create-plane3d
                 intersectionline3d-view
                 (jsx-parents (vector 2 2 0)
                              (vector 1 0 0)
                              (vector 0 1 0))
                 (js-object
                  (vector (vector "fillColor" "#68d391")
                          (vector "fillOpacity" 0.25)))))
          (set! intersectionline3d-plane-2
                (jsx-view3d-create-plane3d
                 intersectionline3d-view
                 (jsx-parents (vector 2 2 0)
                              (vector -2 1 1)
                              (vector 1 -2 1))
                 (js-object
                  (vector (vector "fillColor" "#fc8181")
                          (vector "fillOpacity" 0.25)))))
          (set! intersectionline3d-line
                (jsx-view3d-create-intersectionline3d
                 intersectionline3d-view
                 (jsx-parents intersectionline3d-plane-1
                              intersectionline3d-plane-2)
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")
                          (vector "strokeWidth" 3)))))
          (set! intersectionline3d-board-ready? #t))
        (void (jsx-board-full-update! intersectionline3d-board))
        #t)))

;; init-curve3d-board! : -> boolean?
;;   Build the JSXGraph curve3d board once the browser assets have loaded.
(define (init-curve3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless curve3d-board-ready?
          (console-log "Curve3D board")
          (set! curve3d-board
                (jsx-create-board
                 curve3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! curve3d-view
                (jsx-create-view3d
                 curve3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! curve3d-curve
                (jsx-view3d-create-curve3d
                 curve3d-view
                 (jsx-parents
                  (lambda (t) (cos t))
                  (lambda (t) (sin t))
                  (lambda (t) (/ t 4))
                  (vector 0 (* 2 pi)))
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")
                          (vector "strokeWidth" 3)))))
          (set! curve3d-board-ready? #t))
        (void (jsx-board-full-update! curve3d-board))
        #t)))

;; init-parametricsurface3d-board! : -> boolean?
;;   Build the JSXGraph parametricsurface3d board once the browser assets have loaded.
(define (init-parametricsurface3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless parametricsurface3d-board-ready?
          (console-log "ParametricSurface3D board")
          (set! parametricsurface3d-board
                (jsx-create-board
                 parametricsurface3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! parametricsurface3d-view
                (jsx-create-view3d
                 parametricsurface3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! parametricsurface3d-surface
                (jsx-view3d-create-parametricsurface3d
                 parametricsurface3d-view
                 (jsx-parents
                  (lambda (u v) (* 2 (sin u) (cos v)))
                  (lambda (u v) (* 2 (sin u) (sin v)))
                  (lambda (u v) (* 2 (cos u)))
                  (vector 0 (* 2 pi))
                  (vector 0 pi))
                 (js-object
                  (vector (vector "strokeColor" "#c53030")
                          (vector "stepsU" 24)
                          (vector "stepsV" 16)))))
          (set! parametricsurface3d-board-ready? #t))
        (void (jsx-board-full-update! parametricsurface3d-board))
        #t)))

;; init-polyhedron3d-board! : -> boolean?
;;   Build the JSXGraph polyhedron3d board once the browser assets have loaded.
(define (init-polyhedron3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless polyhedron3d-board-ready?
          (console-log "Polyhedron3D board")
          (set! polyhedron3d-board
                (jsx-create-board
                 polyhedron3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! polyhedron3d-view
                (jsx-create-view3d
                 polyhedron3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! polyhedron3d-object
                (jsx-view3d-create-polyhedron3d
                 polyhedron3d-view
                 (jsx-parents
                  (vector (vector -3 -3 -3)
                          (vector 3 -3 -3)
                          (vector 3 3 -3)
                          (vector -3 3 -3)
                          (vector -3 -3 3)
                          (vector 3 -3 3)
                          (vector 3 3 3)
                          (vector -3 3 3))
                  (vector (vector 0 1 2 3)
                          (vector 4 5 6 7)
                          (vector 0 1 5 4)
                          (vector 1 2 6 5)
                          (vector 2 3 7 6)
                          (vector 3 0 4 7)))
                 (js-object
                  (vector (vector "fillColorArray" (vector "#2b6cb0" "#c53030" "#38a169" "#d69e2e"))
                          (vector "fillOpacity" 0.7)
                          (vector "strokeColor" "#2d3748")
                          (vector "strokeWidth" 1)))))
          (set! polyhedron3d-board-ready? #t))
        (void (jsx-board-full-update! polyhedron3d-board))
        #t)))

;; init-text3d-board! : -> boolean?
;;   Build the JSXGraph text3d board once the browser assets have loaded.
(define (init-text3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless text3d-board-ready?
          (console-log "Text3D board")
          (set! text3d-board
                (jsx-create-board
                 text3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! text3d-view
                (jsx-create-view3d
                 text3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! text3d-object
                (jsx-view3d-create-text3d
                 text3d-view
                 (vector 0 0 0 "3D text")
                 (js-object
                  (vector (vector "anchorX" "middle")
                          (vector "anchorY" "middle")
                          (vector "fontSize" 18)
                          (vector "fillColor" "#c53030")))))
          (set! text3d-board-ready? #t))
        (void (jsx-board-full-update! text3d-board))
        #t)))

;; init-vectorfield3d-board! : -> boolean?
;;   Build the JSXGraph vectorfield3d board once the browser assets have loaded.
(define (init-vectorfield3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless vectorfield3d-board-ready?
          (console-log "Vectorfield3D board")
          (set! vectorfield3d-board
                (jsx-create-board
                 vectorfield3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! vectorfield3d-view
                (jsx-create-view3d
                 vectorfield3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! vectorfield3d-object
                (jsx-view3d-create-vectorfield3d
                 vectorfield3d-view
                 (jsx-parents
                  (vector
                   (lambda (x y z) (cos y))
                   (lambda (x y z) (sin x))
                   (lambda (x y z) z))
                  (vector -2 5 2)
                  (vector -2 5 2)
                  (vector -2 5 2))
                 (js-object
                  (vector (vector "strokeColor" "#2b6cb0")
                          (vector "scale" 0.5)))))
          (set! vectorfield3d-board-ready? #t))
        (void (jsx-board-full-update! vectorfield3d-board))
        #t)))

;; init-sphere3d-board! : -> boolean?
;;   Build the JSXGraph sphere3d board once the browser assets have loaded.
(define (init-sphere3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless sphere3d-board-ready?
          (console-log "Sphere3D board")
          (set! sphere3d-board
                (jsx-create-board
                 sphere3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! sphere3d-view
                (jsx-create-view3d
                 sphere3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! sphere3d-center
                (jsx-view3d-create-point3d
                 sphere3d-view
                 (vector 0 0 0)
                 (js-object
                  (vector (vector "name" "O")
                          (vector "size" 4)
                          (vector "fillColor" "#2b6cb0")
                          (vector "strokeColor" "#2c5282")))))
          (set! sphere3d-point
                (jsx-view3d-create-point3d
                 sphere3d-view
                 (vector 2 0 0)
                 (js-object
                  (vector (vector "name" "P")
                          (vector "size" 4)
                          (vector "fillColor" "#c53030")
                          (vector "strokeColor" "#9b2c2c")))))
          (set! sphere3d-object
                (jsx-view3d-create-sphere3d
                 sphere3d-view
                 (jsx-parents sphere3d-center
                              sphere3d-point)
                 (js-object
                  (vector (vector "fillColor" "#63b3ed")
                          (vector "fillOpacity" 0.45)
                          (vector "strokeColor" "#2b6cb0")
                          (vector "strokeWidth" 2)))))
          (set! sphere3d-board-ready? #t))
        (void (jsx-board-full-update! sphere3d-board))
        #t)))

;; init-surface3d-board! : -> boolean?
;;   Build the JSXGraph surface3d board once the browser assets have loaded.
(define (init-surface3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless surface3d-board-ready?
          (console-log "Surface3D board")
          (set! surface3d-board
                (jsx-create-board
                 surface3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! surface3d-view
                (jsx-create-view3d
                 surface3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! surface3d-object
                (jsx-view3d-create-surface3d
                 surface3d-view
                 (jsx-parents
                  (lambda (u v) (* 2 (cos u) (cos v)))
                  (lambda (u v) (* 1.5 (sin u) (cos v)))
                  (lambda (u v) (* 0.75 (sin v)))
                  (vector 0 (* 2 pi))
                  (vector -1.5 1.5))
                 (js-object
                  (vector (vector "strokeColor" "#c53030")
                          (vector "fillColor" "#fc8181")
                          (vector "fillOpacity" 0.55)
                          (vector "stepsU" 20)
                          (vector "stepsV" 18)))))
          (set! surface3d-board-ready? #t))
        (void (jsx-board-full-update! surface3d-board))
        #t)))

;; init-face3d-board! : -> boolean?
;;   Build the JSXGraph face3d board once the browser assets have loaded.
(define (init-face3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless face3d-board-ready?
          (console-log "Face3D board")
          (set! face3d-board
                (jsx-create-board
                 face3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! face3d-view
                (jsx-create-view3d
                 face3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! face3d-polyhedron
                (jsx-view3d-create-polyhedron3d
                 face3d-view
                 (jsx-parents
                  (vector (vector -3 -3 -3)
                          (vector 3 -3 -3)
                          (vector 3 3 -3)
                          (vector -3 3 -3)
                          (vector -3 -3 3)
                          (vector 3 -3 3)
                          (vector 3 3 3)
                          (vector -3 3 3))
                  (vector (vector 0 1 2 3)
                          (vector 4 5 6 7)
                          (vector 0 1 5 4)
                          (vector 1 2 6 5)
                          (vector 2 3 7 6)
                          (vector 3 0 4 7)))
                 (js-object
                  (vector (vector "fillColorArray" (vector "#2b6cb0" "#c53030" "#38a169" "#d69e2e"))
                          (vector "fillOpacity" 0.2)
                          (vector "strokeColor" "#2d3748")
                          (vector "strokeWidth" 1)))))
          (define face3d-def (js-ref (jsx-element-raw face3d-polyhedron) "def"))
          (set! face3d-object
                (jsx-view3d-create-face3d
                 face3d-view
                 (jsx-parents face3d-def 0)
                 (js-object
                  (vector (vector "strokeColor" "#805ad5")
                          (vector "fillColor" "#b794f4")
                          (vector "fillOpacity" 0.65)
                          (vector "strokeWidth" 2)))))
          (set! face3d-board-ready? #t))
        (void (jsx-board-full-update! face3d-board))
        #t)))

;; init-ticks3d-board! : -> boolean?
;;   Build the JSXGraph ticks3d board once the browser assets have loaded.
(define (init-ticks3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless ticks3d-board-ready?
          (console-log "Ticks3D board")
          (set! ticks3d-board
                (jsx-create-board
                 ticks3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! ticks3d-view
                (jsx-create-view3d
                 ticks3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! ticks3d-object
                (jsx-view3d-create-ticks3d
                 ticks3d-view
                 (jsx-parents (vector -4 0 0)
                              (vector 1 0 0)
                              6
                              (vector 0 0 1))
                 (js-object
                  (vector (vector "ticksDistance" 0.75)
                          (vector "strokeColor" "#2b6cb0")
                          (vector "strokeWidth" 2)))))
          (set! ticks3d-board-ready? #t))
        (void (jsx-board-full-update! ticks3d-board))
        #t)))

;; init-transformation3d-board! : -> boolean?
;;   Build the JSXGraph transformation3d board once the browser assets have loaded.
(define (init-transformation3d-board!)
  (define win (js-window-window))
  (define jxg (js-ref win "JXG"))
  (define jsxgraph (and (extern-present? jxg)
                        (js-ref jxg "JSXGraph")))
  (if (not (extern-present? jsxgraph))
      #f
      (let ()
        (unless transformation3d-board-ready?
          (console-log "Transformation3D board")
          (set! transformation3d-board
                (jsx-create-board
                 transformation3d-board-id
                 (js-object
                  (vector (vector "boundingbox" #[-6 6 6 -6])
                          (vector "axis" #f)
                          (vector "grid" #f)
                          (vector "keepaspectratio" #t)))))
          (set! transformation3d-view
                (jsx-create-view3d
                 transformation3d-board
                 (jsx-parents (vector -4 -3)
                              (vector 8 8)
                              (vector (vector -5 5)
                                      (vector -5 5)
                                      (vector -5 5)))
                 (gallery-3d-view-options)))
          (set! transformation3d-transform
                (jsx-view3d-create-transformation3d
                 transformation3d-view
                 (vector 1.5 0.75 0.5)
                 (js-object
                  (vector (vector "type" "translate")))))
          (set! transformation3d-point
                (jsx-view3d-create-point3d
                 transformation3d-view
                 (vector -2 -1 -1)
                 (js-object
                  (vector (vector "name" "A")
                          (vector "size" 5)
                          (vector "fillColor" "#2b6cb0")
                          (vector "strokeColor" "#2c5282")))))
          (set! transformation3d-object
                (jsx-view3d-create-point3d
                 transformation3d-view
                 (jsx-parents transformation3d-point
                              transformation3d-transform)
                 (js-object
                  (vector (vector "name" "B")
                          (vector "size" 5)
                          (vector "fillColor" "#c53030")
                          (vector "strokeColor" "#9b2c2c")))))
          (set! transformation3d-board-ready? #t))
        (void (jsx-board-full-update! transformation3d-board))
        #t)))

;; init-boards! : -> boolean?
;;   Build both gallery boards once the browser assets have loaded.
(define (init-boards!)
  (define geometry-ready? (init-geometry-board!))
  (define group-ready? (init-group-board!))
  (define chart-ready? (init-chart-board!))
  (define point-ready? (init-point-board!))
  (define line-ready? (init-line-board!))
  (define arc-ready? (init-arc-board!))
  (define angle-ready? (init-angle-board!))
  (define sector-ready? (init-sector-board!))
  (define arrowparallel-ready? (init-arrowparallel-board!))
  (define axis-ready? (init-axis-board!))
  (define segment-ready? (init-segment-board!))
  (define intersection-ready? (init-intersection-board!))
  (define orthogonal-ready? (init-orthogonal-board!))
  (define grid-ready? (init-grid-board!))
  (define boxplot-ready? (init-boxplot-board!))
  (define tangent-ready? (init-tangent-board!))
  (define tangentto-ready? (init-tangentto-board!))
  (define polarline-ready? (init-polarline-board!))
  (define polepoint-ready? (init-polepoint-board!))
  (define radicalaxis-ready? (init-radicalaxis-board!))
  (define circumcircle-ready? (init-circumcircle-board!))
  (define circumcirclearc-ready? (init-circumcirclearc-board!))
  (define circumcirclesector-ready? (init-circumcirclesector-board!))
  (define semicircle-ready? (init-semicircle-board!))
  (define majorarc-ready? (init-majorarc-board!))
  (define majorsector-ready? (init-majorsector-board!))
  (define curveintersection-ready? (init-curveintersection-board!))
  (define curvedifference-ready? (init-curvedifference-board!))
  (define curveunion-ready? (init-curveunion-board!))
  (define derivative-ready? (init-derivative-board!))
  (define integral-ready? (init-integral-board!))
  (define riemannsum-ready? (init-riemannsum-board!))
  (define slopefield-ready? (init-slopefield-board!))
  (define vectorfield-ready? (init-vectorfield-board!))
  (define implicitcurve-ready? (init-implicitcurve-board!))
  (define spline-ready? (init-spline-board!))
  (define cardinalspline-ready? (init-cardinalspline-board!))
  (define comb-ready? (init-comb-board!))
  (define metapostspline-ready? (init-metapostspline-board!))
  (define polygonalchain-ready? (init-polygonalchain-board!))
  (define regularpolygon-ready? (init-regularpolygon-board!))
  (define hyperbola-ready? (init-hyperbola-board!))
  (define parabola-ready? (init-parabola-board!))
  (define stepfunction-ready? (init-stepfunction-board!))
  (define inequality-ready? (init-inequality-board!))
  (define turtle-ready? (init-turtle-board!))
  (define incircle-ready? (init-incircle-board!))
  (define conic-ready? (init-conic-board!))
  (define ellipse-ready? (init-ellipse-board!))
  (define functiongraph-ready? (init-functiongraph-board!))
  (define curve-ready? (init-curve-board!))
  (define polygon-ready? (init-polygon-board!))
  (define arrow-ready? (init-arrow-board!))
  (define circle-ready? (init-circle-board!))
  (define glider-ready? (init-glider-board!))
  (define button-ready? (init-button-board!))
  (define legend-ready? (init-legend-board!))
  (define midpoint-ready? (init-midpoint-board!))
  (define parallel-ready? (init-parallel-board!))
  (define perpendicular-ready? (init-perpendicular-board!))
  (define reflection-ready? (init-reflection-board!))
  (define bisector-ready? (init-bisector-board!))
  (define checkbox-ready? (init-checkbox-board!))
  (define input-ready? (init-input-board!))
  (define slider-ready? (init-slider-board!))
  (define smartlabel-ready? (init-smartlabel-board!))
  (define text-ready? (init-text-board!))
  (define foreignobject-ready? (init-foreignobject-board!))
  (define tapemeasure-ready? (init-tapemeasure-board!))
  (define measurement-ready? (init-measurement-board!))
  (define circumcenter-ready? (init-circumcenter-board!))
  (define mirrorelement-ready? (init-mirrorelement-board!))
  (define mirrorpoint-ready? (init-mirrorpoint-board!))
  (define otherintersection-ready? (init-otherintersection-board!))
  (define orthogonalprojection-ready? (init-orthogonalprojection-board!))
  (define parallelpoint-ready? (init-parallelpoint-board!))
  (define perpendicularpoint-ready? (init-perpendicularpoint-board!))
  (define ticks-ready? (init-ticks-board!))
  (define transformation-ready? (init-transformation-board!))
  (define tracecurve-ready? (init-tracecurve-board!))
  (define parallelogram-ready? (init-parallelogram-board!))
  (define reflexangle-ready? (init-reflexangle-board!))
  (define bisectorlines-ready? (init-bisectorlines-board!))
  (define perpendicularsegment-ready? (init-perpendicularsegment-board!))
  (define widgets-ready? (init-widgets-board!))
  (define annotation-ready? (init-annotation-board!))
  (define view3d-ready? (init-view3d-board!))
  (define point3d-ready? (init-point3d-board!))
  (define line3d-ready? (init-line3d-board!))
  (define circle3d-ready? (init-circle3d-board!))
  (define plane3d-ready? (init-plane3d-board!))
  (define axes3d-ready? (init-axes3d-board!))
  (define axis3d-ready? (init-axis3d-board!))
  (define functiongraph3d-ready? (init-functiongraph3d-board!))
  (define intersectioncircle3d-ready? (init-intersectioncircle3d-board!))
  (define intersectionline3d-ready? (init-intersectionline3d-board!))
  (define curve3d-ready? (init-curve3d-board!))
  (define parametricsurface3d-ready? (init-parametricsurface3d-board!))
  (define surface3d-ready? (init-surface3d-board!))
  (define face3d-ready? (init-face3d-board!))
  (define transformation3d-ready? (init-transformation3d-board!))
  (define polyhedron3d-ready? (init-polyhedron3d-board!))
  (define text3d-ready? (init-text3d-board!))
  (define sphere3d-ready? (init-sphere3d-board!))
  (define ticks3d-ready? (init-ticks3d-board!))
  (define vectorfield3d-ready? (init-vectorfield3d-board!))
  (when (and geometry-ready? group-ready? chart-ready?
             point-ready? line-ready? arc-ready? angle-ready? sector-ready? arrow-ready? arrowparallel-ready? circle-ready? glider-ready? button-ready? legend-ready? axis-ready? segment-ready? intersection-ready? orthogonal-ready? grid-ready? boxplot-ready? tangent-ready? tangentto-ready? polarline-ready? polepoint-ready? radicalaxis-ready? circumcircle-ready? circumcirclearc-ready? circumcirclesector-ready? semicircle-ready? majorarc-ready? majorsector-ready? curveintersection-ready? curvedifference-ready? curveunion-ready? derivative-ready? integral-ready? incircle-ready? conic-ready? ellipse-ready? functiongraph-ready? curve-ready? polygon-ready? midpoint-ready? parallel-ready? perpendicular-ready? reflection-ready? bisector-ready? widgets-ready? annotation-ready?
             checkbox-ready? input-ready? slider-ready? smartlabel-ready? text-ready?
             foreignobject-ready? tapemeasure-ready? measurement-ready? circumcenter-ready? mirrorelement-ready?
             mirrorpoint-ready? otherintersection-ready? orthogonalprojection-ready? parallelpoint-ready? perpendicularpoint-ready?
             ticks-ready? transformation-ready? tracecurve-ready? parallelogram-ready? reflexangle-ready?
             bisectorlines-ready? perpendicularsegment-ready?
             riemannsum-ready? slopefield-ready? vectorfield-ready? implicitcurve-ready? spline-ready?
             cardinalspline-ready? comb-ready? metapostspline-ready? polygonalchain-ready? regularpolygon-ready?
             hyperbola-ready? parabola-ready? stepfunction-ready? inequality-ready? turtle-ready?
             view3d-ready? point3d-ready? line3d-ready? circle3d-ready? plane3d-ready?
             axes3d-ready? axis3d-ready? functiongraph3d-ready? intersectioncircle3d-ready? intersectionline3d-ready?
             curve3d-ready? parametricsurface3d-ready? surface3d-ready? face3d-ready? transformation3d-ready?
             polyhedron3d-ready? text3d-ready? sphere3d-ready?
             ticks3d-ready? vectorfield3d-ready?
             geometry-board-ready? group-board-ready? chart-board-ready?
             point-board-ready? line-board-ready? arc-board-ready? angle-board-ready? sector-board-ready? arrowparallel-board-ready? axis-board-ready? segment-board-ready? intersection-board-ready? grid-board-ready? boxplot-board-ready? tangent-board-ready? tangentto-board-ready? polarline-board-ready? polepoint-board-ready? radicalaxis-board-ready? circumcircle-board-ready? circumcirclearc-board-ready? circumcirclesector-board-ready? semicircle-board-ready? majorarc-board-ready? majorsector-board-ready? curveintersection-board-ready? curvedifference-board-ready? curveunion-board-ready? derivative-board-ready? integral-board-ready? riemannsum-board-ready? slopefield-board-ready? vectorfield-board-ready? implicitcurve-board-ready? spline-board-ready? incircle-board-ready? foreignobject-board-ready? tapemeasure-board-ready? measurement-board-ready? circumcenter-board-ready? mirrorelement-board-ready? ticks-board-ready? transformation-board-ready? tracecurve-board-ready? parallelogram-board-ready? reflexangle-board-ready? widgets-board-ready? annotation-board-ready?)
    (set-status! "Boards ready.")
    (set-summary! "Created the JSXGraph gallery boards, including the 3D surface and transform demos."))
  (and geometry-ready? group-ready? chart-ready?
             arrow-ready? arrowparallel-ready? circle-ready? glider-ready? button-ready? legend-ready? axis-ready? segment-ready? intersection-ready? grid-ready? boxplot-ready? tangent-ready? tangentto-ready? polarline-ready? polepoint-ready? radicalaxis-ready? circumcircle-ready? circumcirclearc-ready? circumcirclesector-ready? semicircle-ready? majorarc-ready? majorsector-ready? curveintersection-ready? curvedifference-ready? curveunion-ready? derivative-ready? integral-ready? incircle-ready? conic-ready? ellipse-ready? functiongraph-ready? curve-ready? polygon-ready? midpoint-ready? parallel-ready? perpendicular-ready? reflection-ready? bisector-ready? widgets-ready? annotation-ready?
             checkbox-ready? input-ready? slider-ready? smartlabel-ready? text-ready?
             riemannsum-ready? slopefield-ready? vectorfield-ready? implicitcurve-ready? spline-ready?
             cardinalspline-ready? comb-ready? metapostspline-ready? polygonalchain-ready? regularpolygon-ready?
             hyperbola-ready? parabola-ready? stepfunction-ready? inequality-ready? turtle-ready?
       geometry-board-ready? group-board-ready? chart-board-ready?
       arrowparallel-board-ready? axis-board-ready? segment-board-ready? intersection-board-ready? orthogonal-board-ready? grid-board-ready? boxplot-board-ready? tangent-board-ready? tangentto-board-ready? polarline-board-ready? polepoint-board-ready? radicalaxis-board-ready? circumcircle-board-ready? circumcirclearc-board-ready? circumcirclesector-board-ready? semicircle-board-ready? majorarc-board-ready? majorsector-board-ready? curveintersection-board-ready? curvedifference-board-ready? curveunion-board-ready? derivative-board-ready? integral-board-ready? riemannsum-board-ready? slopefield-board-ready? vectorfield-board-ready? implicitcurve-board-ready? spline-board-ready? incircle-board-ready? mirrorpoint-board-ready? otherintersection-board-ready? orthogonalprojection-board-ready? parallelpoint-board-ready? perpendicularpoint-board-ready? bisectorlines-ready? perpendicularsegment-ready? widgets-board-ready? annotation-board-ready?
       curve3d-ready? parametricsurface3d-ready? surface3d-ready? face3d-ready? transformation3d-ready?
       polyhedron3d-ready? text3d-ready? sphere3d-ready?
       ticks3d-ready? vectorfield3d-ready?))

;; refresh-gallery! : -> void?
;;   Force a redraw of the live gallery boards.
(define (refresh-gallery!)
  (when geometry-board
    (jsx-board-full-update! geometry-board))
  (when group-board
    (jsx-board-full-update! group-board))
  (when chart-board
    (jsx-board-full-update! chart-board))
  (when point-board
    (jsx-board-full-update! point-board))
  (when line-board
    (jsx-board-full-update! line-board))
  (when arc-board
    (jsx-board-full-update! arc-board))
  (when angle-board
    (jsx-board-full-update! angle-board))
  (when sector-board
    (jsx-board-full-update! sector-board))
  (when arrowparallel-board
    (jsx-board-full-update! arrowparallel-board))
  (when axis-board
    (jsx-board-full-update! axis-board))
  (when segment-board
    (jsx-board-full-update! segment-board))
  (when intersection-board
    (jsx-board-full-update! intersection-board))
  (when orthogonal-board
    (jsx-board-full-update! orthogonal-board))
  (when grid-board
    (jsx-board-full-update! grid-board))
  (when boxplot-board
    (jsx-board-full-update! boxplot-board))
  (when tangent-board
    (jsx-board-full-update! tangent-board))
  (when tangentto-board
    (jsx-board-full-update! tangentto-board))
  (when polarline-board
    (jsx-board-full-update! polarline-board))
  (when polepoint-board
    (jsx-board-full-update! polepoint-board))
  (when radicalaxis-board
    (jsx-board-full-update! radicalaxis-board))
  (when circumcircle-board
    (jsx-board-full-update! circumcircle-board))
  (when circumcirclearc-board
    (jsx-board-full-update! circumcirclearc-board))
  (when circumcirclesector-board
    (jsx-board-full-update! circumcirclesector-board))
  (when semicircle-board
    (jsx-board-full-update! semicircle-board))
  (when majorarc-board
    (jsx-board-full-update! majorarc-board))
  (when majorsector-board
    (jsx-board-full-update! majorsector-board))
  (when curveintersection-board
    (jsx-board-full-update! curveintersection-board))
  (when curvedifference-board
    (jsx-board-full-update! curvedifference-board))
  (when curveunion-board
    (jsx-board-full-update! curveunion-board))
  (when derivative-board
    (jsx-board-full-update! derivative-board))
  (when integral-board
    (jsx-board-full-update! integral-board))
  (when riemannsum-board
    (jsx-board-full-update! riemannsum-board))
  (when slopefield-board
    (jsx-board-full-update! slopefield-board))
  (when vectorfield-board
    (jsx-board-full-update! vectorfield-board))
  (when implicitcurve-board
    (jsx-board-full-update! implicitcurve-board))
  (when spline-board
    (jsx-board-full-update! spline-board))
  (when cardinalspline-board
    (jsx-board-full-update! cardinalspline-board))
  (when comb-board
    (jsx-board-full-update! comb-board))
  (when metapostspline-board
    (jsx-board-full-update! metapostspline-board))
  (when polygonalchain-board
    (jsx-board-full-update! polygonalchain-board))
  (when regularpolygon-board
    (jsx-board-full-update! regularpolygon-board))
  (when hyperbola-board
    (jsx-board-full-update! hyperbola-board))
  (when parabola-board
    (jsx-board-full-update! parabola-board))
  (when stepfunction-board
    (jsx-board-full-update! stepfunction-board))
  (when inequality-board
    (jsx-board-full-update! inequality-board))
  (when turtle-board
    (jsx-board-full-update! turtle-board))
  (when incircle-board
    (jsx-board-full-update! incircle-board))
  (when conic-board
    (jsx-board-full-update! conic-board))
  (when ellipse-board
    (jsx-board-full-update! ellipse-board))
  (when functiongraph-board
    (jsx-board-full-update! functiongraph-board))
  (when curve-board
    (jsx-board-full-update! curve-board))
  (when polygon-board
    (jsx-board-full-update! polygon-board))
  (when arrow-board
    (jsx-board-full-update! arrow-board))
  (when circle-board
    (jsx-board-full-update! circle-board))
  (when glider-board
    (jsx-board-full-update! glider-board))
  (when button-board
    (jsx-board-full-update! button-board))
  (when legend-board
    (jsx-board-full-update! legend-board))
  (when midpoint-board
    (jsx-board-full-update! midpoint-board))
  (when parallel-board
    (jsx-board-full-update! parallel-board))
  (when perpendicular-board
    (jsx-board-full-update! perpendicular-board))
  (when reflection-board
    (jsx-board-full-update! reflection-board))
  (when bisector-board
    (jsx-board-full-update! bisector-board))
  (when checkbox-board
    (jsx-board-full-update! checkbox-board))
  (when input-board
    (jsx-board-full-update! input-board))
  (when slider-board
    (jsx-board-full-update! slider-board))
  (when smartlabel-board
    (jsx-board-full-update! smartlabel-board))
  (when text-board
    (jsx-board-full-update! text-board))
  (when foreignobject-board
    (jsx-board-full-update! foreignobject-board))
  (when tapemeasure-board
    (jsx-board-full-update! tapemeasure-board))
  (when measurement-board
    (jsx-board-full-update! measurement-board))
  (when circumcenter-board
    (jsx-board-full-update! circumcenter-board))
  (when mirrorelement-board
    (jsx-board-full-update! mirrorelement-board))
  (when ticks-board
    (jsx-board-full-update! ticks-board))
  (when transformation-board
    (jsx-board-full-update! transformation-board))
  (when tracecurve-board
    (jsx-board-full-update! tracecurve-board))
  (when parallelogram-board
    (jsx-board-full-update! parallelogram-board))
  (when reflexangle-board
    (jsx-board-full-update! reflexangle-board))
  (when mirrorpoint-board
    (jsx-board-full-update! mirrorpoint-board))
  (when otherintersection-board
    (jsx-board-full-update! otherintersection-board))
  (when orthogonalprojection-board
    (jsx-board-full-update! orthogonalprojection-board))
  (when parallelpoint-board
    (jsx-board-full-update! parallelpoint-board))
  (when perpendicularpoint-board
    (jsx-board-full-update! perpendicularpoint-board))
  (when widgets-board
    (jsx-board-full-update! widgets-board))
  (when annotation-board
    (jsx-board-full-update! annotation-board))
  (when view3d-board
    (jsx-board-full-update! view3d-board))
  (when point3d-board
    (jsx-board-full-update! point3d-board))
  (when line3d-board
    (jsx-board-full-update! line3d-board))
  (when circle3d-board
    (jsx-board-full-update! circle3d-board))
  (when plane3d-board
    (jsx-board-full-update! plane3d-board))
  (when axes3d-board
    (jsx-board-full-update! axes3d-board))
  (when axis3d-board
    (jsx-board-full-update! axis3d-board))
  (when functiongraph3d-board
    (jsx-board-full-update! functiongraph3d-board))
  (when intersectioncircle3d-board
    (jsx-board-full-update! intersectioncircle3d-board))
  (when intersectionline3d-board
    (jsx-board-full-update! intersectionline3d-board))
  (when curve3d-board
    (jsx-board-full-update! curve3d-board))
  (when parametricsurface3d-board
    (jsx-board-full-update! parametricsurface3d-board))
  (when surface3d-board
    (jsx-board-full-update! surface3d-board))
  (when face3d-board
    (jsx-board-full-update! face3d-board))
  (when transformation3d-board
    (jsx-board-full-update! transformation3d-board))
  (when polyhedron3d-board
    (jsx-board-full-update! polyhedron3d-board))
  (when text3d-board
    (jsx-board-full-update! text3d-board))
  (when sphere3d-board
    (jsx-board-full-update! sphere3d-board))
  (when ticks3d-board
    (jsx-board-full-update! ticks3d-board))
  (when vectorfield3d-board
    (jsx-board-full-update! vectorfield3d-board))
  (set-status! "Boards refreshed.")
  (set-summary! "The live gallery boards were redrawn, including the new class examples."))

(define jsx-graph-gallery-app
  (window
   (container #:style "max-width: 1100px;"
    (vpanel
     (h1 "JSXGraph Gallery")
     (text "A small browser gallery for the jsx-graph wrapper.")
     (text "It shows the jsx-graph classes in several separate boards.")
     (gallery-headline "Composite Boards")
     (text "Geometry board: drag the free points and the arrow, slider, and smart label should update together.")
     (container #:id geometry-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Group board: drag P freely while Q follows it as the grouped point.")
     (container #:id group-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Chart board: the bars and legend should match the same data set.")
     (container #:id chart-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "Basic Geometry")
     (text "Point board: A is free, B is fixed, and C is restricted to the line through A and B.")
     (container #:id point-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Line board: the line, segment, and ray should each be visible on this board.")
     (container #:id line-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Arc board: the arc should follow the three draggable points.")
     (container #:id arc-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Angle board: the angle should follow the three draggable points.")
     (container #:id angle-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Sector board: the sector should follow the three draggable points.")
     (container #:id sector-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "Construction Helpers")
     (text "Widget board: the button, checkbox, input, and slider show the interactive widget wrappers.")
     (container #:id widgets-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Annotation board: text and image show the annotation-style wrappers.")
     (container #:id annotation-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Arrowparallel board: the three points and segment define a single arrowparallel example.")
     (container #:id arrowparallel-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Axis board: the x-axis and y-axis are shown as a dedicated axis example.")
     (container #:id axis-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Segment board: the segment should stay between the two draggable endpoints.")
     (container #:id segment-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Intersection board: the two lines should cross at the red intersection point.")
     (container #:id intersection-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Normal board: the normal line should stay perpendicular to the circle at the chosen point.")
     (container #:id orthogonal-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "Relation Helpers")
     (text "Grid board: a dedicated grid example should show a visible square mesh.")
     (container #:attrs '((style "max-width: 260px;")))
     (choice #:id "grid-style-choice"
             '(("standard" "Standard")
               ("fancy" "Fancy")
               ("extreme" "Extreme Fancy"))
             grid-mode
             (lambda (mode)
               (set! grid-mode mode)
               (refresh-grid-board!)))
     (container #:id grid-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Boxplot board: the boxplot should render from the quantiles and placement values.")
     (container #:id boxplot-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Tangent board: drag the glider on the curve and the tangent line should rotate with it.")
     (container #:id tangent-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "TangentTo board: drag the point and the two tangents to the circle should update.")
     (container #:id tangentto-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "PolarLine board: drag the point and the polar line should update against the conic.")
     (container #:id polarline-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "PolePoint board: drag the line and the pole point should update against the conic.")
     (container #:id polepoint-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "RadicalAxis board: the radical axis should separate the two circles shown; the circles intentionally have different sizes.")
     (container #:id radicalaxis-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Circumcircle board: the circle should pass through all three triangle points.")
     (container #:id circumcircle-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "CircumcircleArc board: the arc should follow the same three points on the circumcircle.")
     (container #:id circumcirclearc-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "CircumcircleSector board: the sector should follow the same three points on the circumcircle.")
     (container #:id circumcirclesector-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Semicircle board: the semicircle should span the diameter endpoints.")
     (container #:id semicircle-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "MajorArc board: the arc should show the longer path through the third point.")
     (container #:id majorarc-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "MajorSector board: the sector should show the larger sweep through the third point.")
     (container #:id majorsector-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "Curves and Analysis")
     (text "CurveIntersection board: the overlap of the two circles should be visible as the intersected path.")
     (container #:id curveintersection-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "CurveDifference board: the part of the left circle outside the right circle should remain visible.")
     (container #:id curvedifference-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "CurveUnion board: the union of the two circles should form one combined filled region.")
     (container #:id curveunion-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Derivative board: the derivative curve should follow the slope of the original function graph.")
     (container #:id derivative-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Integral board: the filled area should show the integral under the curve over the interval.")
     (container #:id integral-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Riemannsum board: the rectangles approximate the area under the curve, and the displayed value should track the sum.")
     (container #:id riemannsum-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Slopefield board: each short segment should reflect the local slope function.")
     (container #:id slopefield-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Vectorfield board: the arrows should reflect the chosen vector field.")
     (container #:id vectorfield-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "ImplicitCurve board: the zero set of the equation should appear as a smooth contour.")
     (container #:id implicitcurve-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Spline board: the spline should pass smoothly through the four control points.")
     (container #:id spline-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Cardinalspline board: drag the points and the tension slider to reshape the spline.")
     (container #:id cardinalspline-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Comb board: the comb should follow the two draggable points.")
     (container #:id comb-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Metapost spline board: the point cloud and control sliders should reshape the curve.")
     (container #:id metapostspline-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "PolygonalChain board: the chain should connect the draggable points in order.")
     (container #:id polygonalchain-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "RegularPolygon board: the polygon should stay regular as you move the base points.")
     (container #:id regularpolygon-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Hyperbola board: three points should define the hyperbola.")
     (container #:id hyperbola-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Parabola board: the point and directrix line should define the parabola.")
     (container #:id parabola-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Stepfunction board: the step values should match the x/y sample arrays.")
     (container #:id stepfunction-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Inequality board: the shaded region should match the side of the line inequality.")
     (container #:id inequality-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "Special Demos")
     (text "Turtle board: the turtle should trace a Koch curve from left to right.")
     (container #:id turtle-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Incircle board: the circle should touch all three sides of the triangle.")
     (container #:id incircle-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Conic board: the conic should be defined by the five draggable points.")
     (container #:id conic-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Ellipse board: the ellipse should follow its three defining points.")
     (container #:id ellipse-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Functiongraph board: the function graph should draw the quadratic curve.")
     (container #:id functiongraph-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Curve board: the data plot should pass through the sampled x/y points.")
     (container #:id curve-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Polygon board: the polygon should connect the four draggable vertices.")
     (container #:id polygon-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Arrow board: the arrow should follow the two draggable endpoints.")
     (container #:id arrow-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Circle board: the circle should remain centered on the first point and pass through the second.")
     (container #:id circle-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Glider board: A is the glider point and should stay on the circle while you drag it.")
     (container #:id glider-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "Interactive Widgets")
     (text "Button board: clicking the button should update the board summary text.")
     (container #:id button-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Legend board: the legend should track the bar chart data shown on the board.")
     (container #:id legend-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "Form Controls and Labels")
     (text "Midpoint board: the midpoint should stay centered between the two draggable endpoints.")
     (container #:id midpoint-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Parallel board: the second line should remain parallel to the base line through the third point.")
     (container #:id parallel-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Perpendicular board: the constructed line should stay perpendicular to the base line through the third point.")
     (container #:id perpendicular-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Reflection board: the reflected point D should stay mirrored across the base line.")
     (container #:id reflection-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Bisector board: the bisector should track the angle defined by the three points.")
     (container #:id bisector-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Checkbox board: tick the box to show or hide the red point.")
     (container #:id checkbox-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Input board: enter a JavaScript expression and the graph should update on this board.")
     (container #:id input-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Slider board: the slider should be visible on its own board, and it should be compact.")
     (container #:id slider-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Smartlabel board: the smartlabel should track the point on its own board.")
     (container #:id smartlabel-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Text board: the plain text object should appear on its own board.")
     (container #:id text-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "Helper Constructors")
     (text "Ticks board: the line should show tick marks and labels.")
     (container #:id ticks-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Transformation board: the transformed point should stay tied to its source point.")
     (container #:id transformation-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Tracecurve board: the traced curve should follow the moving glider and midpoint.")
     (container #:id tracecurve-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Parallelogram board: the fourth vertex should stay implied by the three draggable points.")
     (container #:id parallelogram-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "ReflexAngle board: the reflex angle should stay attached to the three points.")
     (container #:id reflexangle-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "Remaining Helpers")
     (text "ForeignObject board: the HTML box should appear at its position and size.")
     (container #:id foreignobject-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Tapemeasure board: the tape measure should span the two endpoints.")
     (container #:id tapemeasure-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Measurement board: the measurement should label the radius of the circle.")
     (container #:id measurement-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Circumcenter board: the triangle should show its circumcenter O.")
     (container #:id circumcenter-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "MirrorElement board: D should stay mirrored across M.")
     (container #:id mirrorelement-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "Composition Helpers")
     (text "Bisectorlines board: the bisector between two lines should stay visible.")
     (container #:id bisectorlines-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "PerpendicularSegment board: the segment should stay perpendicular to the line.")
     (container #:id perpendicularsegment-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "Point Relations")
     (text "MirrorPoint board: the reflected point should stay mirrored across the base points.")
     (container #:id mirrorpoint-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "OtherIntersection board: the secondary intersection should stay visible on the two circles.")
     (container #:id otherintersection-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Orthogonalprojection board: the projected point should stay perpendicular to the base line.")
     (container #:id orthogonalprojection-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Parallelpoint board: the constructed point should stay parallel to the base vector.")
     (container #:id parallelpoint-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "PerpendicularPoint board: the constructed point should stay perpendicular to the base line.")
     (container #:id perpendicularpoint-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "3D Views")
     (text "View3D board: the 3D cube should appear cleanly, without the 2D grid backdrop.")
     (container #:id view3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Point3D board: the red point should appear in the clean 3D view.")
     (container #:id point3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Line3D board: the line should connect the two 3D points in the clean 3D view.")
     (container #:id line3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Circle3D board: the circle should sit in the clean 3D view and follow its center and radius.")
     (container #:id circle3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Plane3D board: the plane should pass through the three 3D points in the clean 3D view.")
     (container #:id plane3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "3D Extras")
     (text "Axes3D board: the 3D axes should appear with a centered origin and no 2D backdrop grid.")
     (container #:id axes3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Axis3D board: the 3D axis should connect the two end points in the clean 3D view.")
     (container #:id axis3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Functiongraph3D board: the surface should follow the function of x and y in the clean 3D view.")
     (container #:id functiongraph3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "IntersectionCircle3D board: the circle should follow the two spheres in the clean 3D view.")
     (container #:id intersectioncircle3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "IntersectionLine3D board: the line should follow the two planes in the clean 3D view.")
     (container #:id intersectionline3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "3D Curves and Surfaces")
     (text "Curve3D board: the curve should trace a smooth 3D helix in the clean 3D view.")
     (container #:id curve3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "ParametricSurface3D board: the surface should show the spherical parameterization.")
     (container #:id parametricsurface3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "3D Surfaces and Transformations")
     (text "Surface3D board: the surface should appear using the runtime's parametric surface element.")
     (container #:id surface3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Face3D board: the highlighted face should follow the polyhedron in the clean 3D view.")
     (container #:id face3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Transformation3D board: point B should follow the 3D translate transform from A.")
     (container #:id transformation3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Polyhedron3D board: the cube should show all six faces in the clean 3D view.")
     (container #:id polyhedron3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Text3D board: the 3D text should appear at the origin of the clean 3D view.")
     (container #:id text3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Vectorfield3D board: the vector field should show arrows over the 3D cube.")
     (container #:id vectorfield3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (gallery-headline "3D Solids and Motion")
     (text "Sphere3D board: the sphere should follow the center point and radius point in the clean 3D view.")
     (container #:id sphere3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (text "Ticks3D board: the tick marks should sit on the 3D line in the clean 3D view.")
     (container #:id ticks3d-board-id
                #:class "jxgbox"
                #:attrs '((style "width: 720px; height: 420px;")))
     (hpanel
      (button "Refresh gallery" refresh-gallery!))
     (P @status)
     (P @summary)))))

(define app-renderer
  (render jsx-graph-gallery-app))

(mount-renderer! app-renderer)

(define (pump-init!)
  (cond
    [(init-boards!) (void)]
    [else
     (void
      (js-window-set-timeout/delay
       (procedure->external pump-init!)
       100.))]))

(define (main)
  (ensure-jsxgraph-assets!)
  (void (pump-init!)))

(main)
