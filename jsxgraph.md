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
