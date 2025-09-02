# JSXGraph Notes

## Including JSXGraph in an HTML Page

To use JSXGraph in a web page, include both the stylesheet and the JavaScript library, then define a container for the board.

1. **Include the stylesheet** in the `<head>` section to apply default styles:
   ```html
   <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraph.css" />
   ```

2. **Include the JavaScript library** after the stylesheet:
   ```html
   <script src="https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js"></script>
   ```

3. **Add a container element** where the JSXGraph board will appear:
   ```html
   <div id="box" class="jxgbox" style="width: 500px; height: 400px;"></div>
   ```

4. **Initialize the board** with JavaScript, typically placed in a script block after the container:
   ```html
   <script>
     const board = JXG.JSXGraph.initBoard('box', {
       boundingbox: [-5, 5, 5, -5],
       axis: true
     });
   </script>
   ```

These steps load JSXGraph from a CDN and create a board ready for drawing geometric objects.

## Initializing JSXGraph from WebRacket

The `standard.ffi` library exposes JavaScript operations that let WebRacket
interact with JSXGraph without relying on `js-eval`. The example below mirrors
the JavaScript snippet above:

```racket
(define head (js-document-head))

(define container (js-create-element "div"))
(js-set-attribute! container "id" "box")
(js-set-attribute! container "class" "jxgbox")
(js-set-attribute! container "style" "width: 500px; height: 400px;")
(js-append-child! (js-document-body) container)

(define options
  (js-object
   (vector
    (vector "boundingbox" (vector -5 5 5 -5))
    (vector "axis" #t))))


(define (init-board _)
  (define jxg (js-var "JXG"))
  (define jsx (js-ref jxg "JSXGraph"))
  (define init-fn (js-ref jsx "initBoard"))
  (js-operator 'call (vector init-fn jsx "box" options)))

(define script (js-create-element "script"))
(js-set-attribute! script "src" "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js")
(js-add-event-listener! script "load" (procedure->external init-board))
(js-append-child! head script)
```

This code constructs the board setup using WebRacket primitives, showing how
`js-var`, `js-ref`, `js-operator`, and related functions remove the need for
evaluating raw JavaScript strings.
