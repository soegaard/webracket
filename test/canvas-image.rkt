(define canvas (js-create-element "canvas"))
(js-set-canvas-width!  canvas 300)
(js-set-canvas-height! canvas 300)
(js-append-child! (js-document-body) canvas)

(define ctx (js-canvas-get-context canvas "2d" (js-undefined)))

(define img (js-create-element "img"))
(js-set-attribute! img "src" "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR4nGP4z8AAAAMBAQDJ/pLvAAAAAElFTkSuQmCC")
(js-set-attribute! img "style" "display:none")
(js-append-child! (js-document-body) img)

(js-canvas2d-draw-image-5 ctx img 0. 0. 300. 300.)
