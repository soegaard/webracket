(define (add-children elem children)
  (for ([child (in-list children)])
    (js-append-child! elem (sxml->dom child))))

(define (set-elem-attributes elem attrs)
  (for ([attr (in-list attrs)])
    (match attr
      [(list name value)
       (js-set-attribute! elem (symbol->string name) value)])))

(define (sxml->dom exp)
  (match exp
    [(? string? s)
     (js-create-text-node exp)]
    [(list tag (list '@ attrs ...) children ...)
     ;; Create a new element with the given tag.
     (define elem (js-create-element (symbol->string tag)))
     (set-elem-attributes elem attrs)
     (add-children elem children)
     elem]
    [(list tag children ...)
     ;; Create a new element with the given tag.
     (define elem (js-create-element (symbol->string tag)))
     (add-children elem children)
     elem]))


;; Clear previous content and set black background
(js-eval "document.body.innerHTML='';")
(js-set-attribute! (js-document-body)
                   "style" "background-color: black; margin:0; overflow:hidden;")

;; Add CSS for scrolling text
(define style-element (js-create-element "style"))

(js-append-child! style-element
  (js-create-text-node
   "@keyframes scroll-left {0% { left: 100%; } 100% { left: -100%; }}\n#scroll-text { position: fixed; top: 50%; left: 100%; transform: translateY(-50%); white-space: nowrap; color: white; font-size: 24px; animation: scroll-left 10s linear infinite; }"))

(js-append-child! (js-document-body) style-element)

;; Create scrolling text element
(define scroll-div (js-create-element "div"))
(js-set-attribute! scroll-div "id" "scroll-text")
(js-append-child! scroll-div (js-create-text-node "Welcome to WebRacket!"))
(js-append-child! (js-document-body) scroll-div)
