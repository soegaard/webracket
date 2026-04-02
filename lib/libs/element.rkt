#lang webracket

;;;
;;; Element wrappers
;;;

;; element-stringish->string : symbol? any/c -> string?
;;   Normalize a string-like wrapper argument to a browser string.
(define (element-stringish->string who v)
  (cond
    [(string? v) v]
    [(symbol? v) (symbol->string v)]
    [else (raise-argument-error who "(or/c string? symbol?)" v)]))

;; element-i32->boolean : integer? -> boolean?
;;   Convert a browser i32 flag to a boolean.
(define (element-i32->boolean v)
  (not (zero? v)))

;; element-prop-ref : element? string? -> any/c
;;   Read a property from an element.
(define (element-prop-ref element name)
  (js-send/value (js-var "Reflect") "get" (vector (element-unwrap element) name)))

;; element-prop-set! : element? string? any/c -> void?
;;   Write a property on an element.
(define (element-prop-set! element name value)
  (js-set! (element-unwrap element) name value)
  (void))

;; element-nodeish->value : any/c -> any/c
;;   Normalize a node-like value for DOM insertion helpers.
(define (element-nodeish->value value)
  (cond
    [(symbol? value) (symbol->string value)]
    [(node? value) (node-raw value)]
    [(element? value) (element-raw value)]
    [(text? value) (text-raw value)]
    [(attr? value) (attr-raw value)]
    [else value]))

;; element-id : element? -> (or/c #f string?)
;;   Read an element id.
(define (element-id element)
  (element-prop-ref element "id"))

;; element-set-id! : element? (or/c string? symbol?) -> void?
;;   Set an element id.
(define (element-set-id! element id)
  (js-set! (element-unwrap element)
           "id"
           (element-stringish->string 'element-set-id! id))
  (void))

;; element-class-name : element? -> (or/c #f string?)
;;   Read an element class name.
(define (element-class-name element)
  (element-prop-ref element "className"))

;; element-set-class-name! : element? (or/c string? symbol?) -> void?
;;   Set an element class name.
(define (element-set-class-name! element class-name)
  (js-set! (element-unwrap element)
           "className"
           (element-stringish->string 'element-set-class-name! class-name))
  (void))

;; element-class-list : element? -> (or/c #f dom-token-list?)
;;   Read an element class list.
(define (element-class-list element)
  (dom-token-list-wrap (js-ref/extern (element-unwrap element) "classList")))

;; element-tag-name : element? -> string?
;;   Read the element tag name.
(define (element-tag-name element)
  (element-prop-ref element "tagName"))

;; element-local-name : element? -> string?
;;   Read the element local name.
(define (element-local-name element)
  (element-prop-ref element "localName"))

;; element-namespace-uri : element? -> (or/c #f string?)
;;   Read the element namespace URI.
(define (element-namespace-uri element)
  (element-prop-ref element "namespaceURI"))

;; element-prefix : element? -> (or/c #f string?)
;;   Read the element namespace prefix.
(define (element-prefix element)
  (element-prop-ref element "prefix"))

;; element-is-connected? : element? -> boolean?
;;   Report whether the element is connected to a document.
(define (element-is-connected? element)
  (element-i32->boolean (element-prop-ref element "isConnected")))

;; append-child! : external? external? -> void?
;;   Append a child node.
(define (append-child! parent child)
  (js-append-child! (element-unwrap parent) (element-unwrap child))
  (void))

;; element-append! : element? any/c -> void?
;;   Append a node or text to an element.
(define (element-append! element child)
  (js-append! (element-unwrap element) (element-nodeish->value child))
  (void))

;; element-prepend! : element? any/c -> void?
;;   Prepend a node or text to an element.
(define (element-prepend! element child)
  (js-prepend! (element-unwrap element) (element-nodeish->value child))
  (void))

;; element-before! : element? any/c -> void?
;;   Insert a node or text before an element.
(define (element-before! element sibling)
  (js-before! (element-unwrap element) (element-nodeish->value sibling))
  (void))

;; element-after! : element? any/c -> void?
;;   Insert a node or text after an element.
(define (element-after! element sibling)
  (js-after! (element-unwrap element) (element-nodeish->value sibling))
  (void))

;; set-attribute! : external? (or/c string? symbol?) (or/c string? symbol?) -> void?
;;   Set an element attribute.
(define (set-attribute! element name value)
  (define name* (element-stringish->string 'set-attribute! name))
  (define value* (element-stringish->string 'set-attribute! value))
  (js-set-attribute! (element-unwrap element) name* value*)
  (void))

;; get-attribute : external? (or/c string? symbol?) -> (or/c #f string?)
;;   Read an element attribute.
(define (get-attribute element name)
  (define name* (element-stringish->string 'get-attribute name))
  (js-send/value (element-unwrap element) "getAttribute" (vector name*)))

;; element-has-attribute? : element? (or/c string? symbol?) -> boolean?
;;   Report whether an element has an attribute.
(define (element-has-attribute? element name)
  (define name* (element-stringish->string 'element-has-attribute? name))
  (element-i32->boolean (js-has-attribute (element-unwrap element) name*)))

;; element-has-attributes? : element? -> boolean?
;;   Report whether an element has any attributes.
(define (element-has-attributes? element)
  (element-i32->boolean (js-has-attributes (element-unwrap element))))

;; element-has-pointer-capture? : element? exact-nonnegative-integer? -> boolean?
;;   Report whether this element has pointer capture for a pointer id.
(define (element-has-pointer-capture? element pointer-id)
  (element-i32->boolean
   (js-has-pointer-capture (element-unwrap element) pointer-id)))

;; element-remove-attribute! : element? (or/c string? symbol?) -> void?
;;   Remove an attribute from an element.
(define (element-remove-attribute! element name)
  (define name* (element-stringish->string 'element-remove-attribute! name))
  (js-remove-attribute! (element-unwrap element) name*)
  (void))

;; element-remove-attribute-ns! : element? (or/c string? symbol?) (or/c string? symbol?) -> void?
;;   Remove a namespaced attribute from an element.
(define (element-remove-attribute-ns! element ns name)
  (define ns* (element-stringish->string 'element-remove-attribute-ns! ns))
  (define name* (element-stringish->string 'element-remove-attribute-ns! name))
  (js-remove-attribute-ns! (element-unwrap element) ns* name*)
  (void))

;; element-matches? : element? (or/c string? symbol?) -> boolean?
;;   Report whether an element matches a CSS selector.
(define (element-matches? element selector)
  (define selector* (element-stringish->string 'element-matches? selector))
  (element-i32->boolean (js-matches (element-unwrap element) selector*)))

;; element-closest : element? (or/c string? symbol?) -> (or/c #f element?)
;;   Find the closest ancestor matching a CSS selector.
(define (element-closest element selector)
  (define selector* (element-stringish->string 'element-closest selector))
  (element-wrap (js-closest (element-unwrap element) selector*)))

;; element-get-attribute-names : element? -> vector?
;;   Read the names of the element's attributes.
(define (element-get-attribute-names element)
  (js-send/value (element-unwrap element) "getAttributeNames" (vector)))

;; dom-token-list-value : dom-token-list? -> (or/c #f string?)
;;   Read the class list value.
(define (dom-token-list-value class-list)
  (js-ref/extern (dom-token-list-unwrap class-list) "value"))

;; dom-token-list-length : dom-token-list? -> exact-nonnegative-integer?
;;   Read the number of class tokens.
(define (dom-token-list-length class-list)
  (js-ref (dom-token-list-unwrap class-list) "length"))

;; dom-token-list-item : dom-token-list? exact-nonnegative-integer? -> (or/c #f string?)
;;   Read the class token at an index.
(define (dom-token-list-item class-list index)
  (js-send/value (dom-token-list-unwrap class-list) "item" (vector index)))

;; dom-token-list-contains? : dom-token-list? (or/c string? symbol?) -> boolean?
;;   Report whether a token is present in the class list.
(define (dom-token-list-contains? class-list token)
  (define token* (element-stringish->string 'dom-token-list-contains? token))
  (js-send/boolean (dom-token-list-unwrap class-list) "contains" (vector token*)))

;; dom-token-list-add! : dom-token-list? (or/c string? symbol?) ... -> void?
;;   Add one or more tokens to the class list.
(define (dom-token-list-add! class-list token . more-tokens)
  (define tokens (cons token more-tokens))
  (define token-values
    (list->vector (map (lambda (tok)
                         (element-stringish->string 'dom-token-list-add! tok))
                       tokens)))
  (js-send/extern/nullish (dom-token-list-unwrap class-list) "add" token-values)
  (void))

;; dom-token-list-remove! : dom-token-list? (or/c string? symbol?) ... -> void?
;;   Remove one or more tokens from the class list.
(define (dom-token-list-remove! class-list token . more-tokens)
  (define tokens (cons token more-tokens))
  (define token-values
    (list->vector (map (lambda (tok)
                         (element-stringish->string 'dom-token-list-remove! tok))
                       tokens)))
  (js-send/extern/nullish (dom-token-list-unwrap class-list) "remove" token-values)
  (void))

;; dom-token-list-toggle! : dom-token-list? (or/c string? symbol?) [force] -> boolean?
;;   Toggle a token, or force a specific state when provided.
(define (dom-token-list-toggle! class-list token [force #f])
  (define token* (element-stringish->string 'dom-token-list-toggle! token))
  (cond
    [(eq? force #f)
     (js-send/boolean (dom-token-list-unwrap class-list) "toggle" (vector token*))]
    [(procedure? force)
     (js-send/boolean (dom-token-list-unwrap class-list) "toggle"
                      (vector token* (if (force) #t #f)))]
    [else
     (js-send/boolean (dom-token-list-unwrap class-list) "toggle"
                      (vector token* (if force #t #f)))]))

;; dom-token-list-replace! : dom-token-list? (or/c string? symbol?) (or/c string? symbol?) -> boolean?
;;   Replace one class token with another.
(define (dom-token-list-replace! class-list old-token new-token)
  (define old-token* (element-stringish->string 'dom-token-list-replace! old-token))
  (define new-token* (element-stringish->string 'dom-token-list-replace! new-token))
  (js-send/boolean (dom-token-list-unwrap class-list) "replace" (vector old-token* new-token*)))

;; element-get-attribute-ns : element? (or/c string? symbol?) (or/c string? symbol?) -> (or/c #f string?)
;;   Read a namespaced element attribute.
(define (element-get-attribute-ns element ns name)
  (define ns* (element-stringish->string 'element-get-attribute-ns ns))
  (define name* (element-stringish->string 'element-get-attribute-ns name))
  (js-send/value (element-unwrap element) "getAttributeNS" (vector ns* name*)))

;; element-get-elements-by-class-name : element? (or/c string? symbol?) -> vector?
;;   Read descendant elements with matching class names.
(define (element-get-elements-by-class-name element class-name)
  (define class-name* (element-stringish->string 'element-get-elements-by-class-name class-name))
  (array-like->vector 'element-get-elements-by-class-name
                      (js-element-get-elements-by-class-name (element-unwrap element) class-name*)
                      element-wrap))

;; element-get-elements-by-tag-name : element? (or/c string? symbol?) -> vector?
;;   Read descendant elements with a matching tag name.
(define (element-get-elements-by-tag-name element tag-name)
  (define tag-name* (element-stringish->string 'element-get-elements-by-tag-name tag-name))
  (array-like->vector 'element-get-elements-by-tag-name
                      (js-element-get-elements-by-tag-name (element-unwrap element) tag-name*)
                      element-wrap))

;; element-get-elements-by-tag-name-ns : element? (or/c string? symbol?) (or/c string? symbol?) -> vector?
;;   Read descendant elements with a matching namespaced tag name.
(define (element-get-elements-by-tag-name-ns element ns tag-name)
  (define ns* (element-stringish->string 'element-get-elements-by-tag-name-ns ns))
  (define tag-name* (element-stringish->string 'element-get-elements-by-tag-name-ns tag-name))
  (array-like->vector 'element-get-elements-by-tag-name-ns
                      (js-element-get-elements-by-tag-name-ns (element-unwrap element) ns* tag-name*)
                      element-wrap))

;; element-children : element? -> vector?
;;   Read the child elements as wrapped elements.
(define (element-children element)
  (array-like->vector 'element-children
                      (element-prop-ref element "children")
                      element-wrap))

;; element-scroll-top : element? -> real?
;;   Read an element's vertical scroll offset.
(define (element-scroll-top element)
  (element-prop-ref element "scrollTop"))

;; element-set-scroll-top! : element? real? -> void?
;;   Set an element's vertical scroll offset.
(define (element-set-scroll-top! element value)
  (element-prop-set! element "scrollTop" value))

;; element-scroll-left : element? -> real?
;;   Read an element's horizontal scroll offset.
(define (element-scroll-left element)
  (element-prop-ref element "scrollLeft"))

;; element-set-scroll-left! : element? real? -> void?
;;   Set an element's horizontal scroll offset.
(define (element-set-scroll-left! element value)
  (element-prop-set! element "scrollLeft" value))

;; element-scroll-width : element? -> exact-nonnegative-integer?
;;   Read an element's scroll width.
(define (element-scroll-width element)
  (element-prop-ref element "scrollWidth"))

;; element-scroll-height : element? -> exact-nonnegative-integer?
;;   Read an element's scroll height.
(define (element-scroll-height element)
  (element-prop-ref element "scrollHeight"))

;; element-client-width : element? -> exact-nonnegative-integer?
;;   Read an element's client width.
(define (element-client-width element)
  (element-prop-ref element "clientWidth"))

;; element-client-height : element? -> exact-nonnegative-integer?
;;   Read an element's client height.
(define (element-client-height element)
  (element-prop-ref element "clientHeight"))

;; element-offset-width : element? -> exact-nonnegative-integer?
;;   Read an element's offset width.
(define (element-offset-width element)
  (element-prop-ref element "offsetWidth"))

;; element-offset-height : element? -> exact-nonnegative-integer?
;;   Read an element's offset height.
(define (element-offset-height element)
  (element-prop-ref element "offsetHeight"))

;; element-child-element-count : element? -> exact-nonnegative-integer?
;;   Read the number of child elements.
(define (element-child-element-count element)
  (element-prop-ref element "childElementCount"))

;; element-first-element-child : element? -> (or/c #f element?)
;;   Read the first child element, if any.
(define (element-first-element-child element)
  (element-wrap (element-prop-ref element "firstElementChild")))

;; element-last-element-child : element? -> (or/c #f element?)
;;   Read the last child element, if any.
(define (element-last-element-child element)
  (element-wrap (element-prop-ref element "lastElementChild")))

;; element-inner-html : element? -> string?
;;   Read an element's HTML contents.
(define (element-inner-html element)
  (element-prop-ref element "innerHTML"))

;; element-set-inner-html! : element? (or/c string? symbol?) -> void?
;;   Replace an element's HTML contents.
(define (element-set-inner-html! element html)
  (element-prop-set! element "innerHTML" (element-stringish->string 'element-set-inner-html! html)))

;; element-outer-html : element? -> string?
;;   Read an element's outer HTML.
(define (element-outer-html element)
  (element-prop-ref element "outerHTML"))

;; element-set-outer-html! : element? (or/c string? symbol?) -> void?
;;   Replace an element's outer HTML.
(define (element-set-outer-html! element html)
  (element-prop-set! element "outerHTML" (element-stringish->string 'element-set-outer-html! html)))

;; element-text-content : element? -> (or/c #f string?)
;;   Read an element's text content.
(define (element-text-content element)
  (element-prop-ref element "textContent"))

;; element-set-text-content! : element? (or/c string? symbol?) -> void?
;;   Replace an element's text content.
(define (element-set-text-content! element text)
  (element-prop-set! element "textContent" (element-stringish->string 'element-set-text-content! text)))

;; element-computed-style-map : element? -> external/raw
;;   Read the computed style map for an element.
(define (element-computed-style-map element)
  (js-computed-style-map (element-unwrap element)))

;; element-get-animations : element? -> external/raw
;;   Read animations affecting an element.
(define (element-get-animations element)
  (js-get-animations (element-unwrap element)))

;; element-attach-shadow! : element? any/c -> external/raw
;;   Attach a shadow root to an element.
(define (element-attach-shadow! element options)
  (js-attach-shadow! (element-unwrap element) options))

;; element-animate : element? any/c [any/c] -> external/raw
;;   Start an animation on an element.
(define (element-animate element keyframes [options #f])
  (js-animate (element-unwrap element)
              keyframes
              (if (eq? options #f) (js-undefined) options)))

;; element-get-attribute-node : element? (or/c string? symbol?) -> (or/c #f attr?)
;;   Read the attribute node for a given name.
(define (element-get-attribute-node element name)
  (define name* (element-stringish->string 'element-get-attribute-node name))
  (attr-wrap (js-send/value (element-unwrap element) "getAttributeNode" (vector name*))))

;; element-get-attribute-node-ns : element? (or/c string? symbol?) (or/c string? symbol?) -> (or/c #f attr?)
;;   Read the namespaced attribute node for an element.
(define (element-get-attribute-node-ns element ns name)
  (define ns* (element-stringish->string 'element-get-attribute-node-ns ns))
  (define name* (element-stringish->string 'element-get-attribute-node-ns name))
  (attr-wrap (js-send/value (element-unwrap element) "getAttributeNodeNS" (vector ns* name*))))

;; element-set-attribute-node! : element? any/c -> (or/c #f attr?)
;;   Attach an attribute node to an element.
(define (element-set-attribute-node! element node)
  (attr-wrap (js-set-attribute-node! (element-unwrap element) (element-nodeish->value node))))

;; element-set-attribute-node-ns! : element? any/c -> (or/c #f attr?)
;;   Attach a namespaced attribute node to an element.
(define (element-set-attribute-node-ns! element node)
  (attr-wrap (js-set-attribute-node-ns! (element-unwrap element) (element-nodeish->value node))))

;; element-remove-attribute-node! : element? any/c -> (or/c #f attr?)
;;   Remove an attribute node from an element.
(define (element-remove-attribute-node! element node)
  (attr-wrap (js-remove-attribute-node! (element-unwrap element) (element-nodeish->value node))))

;; get-bounding-client-rect : element? -> dom-rect?
;;   Read the element bounding box.
(define (get-bounding-client-rect element)
  (dom-rect-wrap (js-get-bounding-client-rect (element-unwrap element))))

;; get-client-rects : external? -> vector?
;;   Read the client rect list as wrapped DOMRect values.
(define (get-client-rects element)
  (array-like->vector 'get-client-rects
                      (js-get-client-rects (element-unwrap element))
                      dom-rect-wrap))

;; query-selector : external? (or/c string? symbol?) -> (or/c #f element?)
;;   Find the first matching descendant.
(define (query-selector element selector)
  (define selector* (element-stringish->string 'query-selector selector))
  (element-wrap (js-element-query-selector (element-unwrap element) selector*)))

;; query-selector-all : external? (or/c string? symbol?) -> vector?
;;   Find all matching descendants as wrapped elements.
(define (query-selector-all element selector)
  (define selector* (element-stringish->string 'query-selector-all selector))
  (array-like->vector 'query-selector-all
                      (js-element-query-selector-all (element-unwrap element) selector*)
                      element-wrap))

;; element-insert-adjacent-element! : element? (or/c string? symbol?) any/c -> (or/c #f element?)
;;   Insert an element relative to another element.
(define (element-insert-adjacent-element! element position child)
  (define position* (element-stringish->string 'element-insert-adjacent-element! position))
  (element-wrap
   (js-insert-adjacent-element! (element-unwrap element)
                                position*
                                (element-nodeish->value child))))

;; element-insert-adjacent-html! : element? (or/c string? symbol?) (or/c string? symbol?) -> void?
;;   Insert HTML relative to an element.
(define (element-insert-adjacent-html! element position html)
  (define position* (element-stringish->string 'element-insert-adjacent-html! position))
  (define html* (element-stringish->string 'element-insert-adjacent-html! html))
  (js-insert-adjacent-html! (element-unwrap element) position* html*)
  (void))

;; element-insert-adjacent-text! : element? (or/c string? symbol?) (or/c string? symbol?) -> void?
;;   Insert text relative to an element.
(define (element-insert-adjacent-text! element position text)
  (define position* (element-stringish->string 'element-insert-adjacent-text! position))
  (define text* (element-stringish->string 'element-insert-adjacent-text! text))
  (js-insert-adjacent-text! (element-unwrap element) position* text*)
  (void))

;; remove! : external? -> void?
;;   Remove the element from the DOM.
(define (remove! element)
  (js-remove! (element-unwrap element))
  (void))

;; replace-children! : external? external? -> void?
;;   Replace the child list with one node.
(define (replace-children! element child)
  (js-replace-children! (element-unwrap element) (element-unwrap child))
  (void))

;; replace-with! : external? external? -> void?
;;   Replace the element with another node.
(define (replace-with! element sibling)
  (js-replace-with! (element-unwrap element) (element-unwrap sibling))
  (void))

;; request-fullscreen : external? -> external/raw
;;   Request fullscreen for the element.
(define (request-fullscreen element)
  (js-request-fullscreen (element-unwrap element)))

;; request-pointer-lock : external? -> void?
;;   Request pointer lock.
(define (request-pointer-lock element)
  (js-request-pointer-lock (element-unwrap element))
  (void))

;; element-set-pointer-capture! : element? exact-nonnegative-integer? -> void?
;;   Capture pointer events for a pointer id.
(define (element-set-pointer-capture! element pointer-id)
  (js-set-pointer-capture! (element-unwrap element) pointer-id)
  (void))

;; element-release-pointer-capture! : element? exact-nonnegative-integer? -> void?
;;   Release pointer capture for a pointer id.
(define (element-release-pointer-capture! element pointer-id)
  (js-release-pointer-capture! (element-unwrap element) pointer-id)
  (void))

;; scroll! : external? real? real? -> void?
;;   Scroll an element to an absolute position.
(define (scroll! element x y)
  (js-scroll! (element-unwrap element) x y)
  (void))

;; scroll-by! : external? real? real? -> void?
;;   Scroll an element by a relative offset.
(define (scroll-by! element x y)
  (js-scroll-by! (element-unwrap element) x y)
  (void))

;; scroll-into-view! : element? [boolean?] -> void?
;;   Scroll ancestors until the element is visible.
(define (scroll-into-view! element [align-to-top? #t])
  (js-scroll-into-view! (element-unwrap element) (if align-to-top? 1 0))
  (void))

;; scroll-to! : external? real? real? -> void?
;;   Scroll an element to an absolute position.
(define (scroll-to! element x y)
  (js-scroll-to! (element-unwrap element) x y)
  (void))

;; set-attribute-ns! : external? (or/c string? symbol?) (or/c string? symbol?) (or/c string? symbol?) -> void?
;;   Set a namespaced attribute.
(define (set-attribute-ns! element ns name value)
  (define ns* (element-stringish->string 'set-attribute-ns! ns))
  (define name* (element-stringish->string 'set-attribute-ns! name))
  (define value* (element-stringish->string 'set-attribute-ns! value))
  (js-set-attribute-ns! (element-unwrap element) ns* name* value*)
  (void))

;; toggle-attribute! : external? (or/c string? symbol?) (or/c boolean? procedure?) -> boolean?
;;   Toggle an attribute, or force a specific state when provided.
(define (toggle-attribute! element name [force #f])
  (define name* (element-stringish->string 'toggle-attribute! name))
  (cond
    [(eq? force #f)
     (js-send/value (element-unwrap element) "toggleAttribute" (vector name*))]
    [(procedure? force)
     (element-i32->boolean
      (js-toggle-attribute! (element-unwrap element) name* (if (force) 1 0)))]
    [else
     (element-i32->boolean
      (js-toggle-attribute! (element-unwrap element) name* (if force 1 0)))]))
