#lang webracket

;;;
;;; Element wrappers
;;;

;; element-i32->boolean : integer? -> boolean?
;;   Convert a browser i32 flag to a boolean.
(define (element-i32->boolean v)
  (not (zero? v)))

;; append-child! : external? external? -> void?
;;   Append a child node.
(define (append-child! parent child)
  (js-append-child! (element-unwrap parent) (element-unwrap child))
  (void))

;; set-attribute! : external? string? string? -> void?
;;   Set an element attribute.
(define (set-attribute! element name value)
  (unless (string? name)
    (raise-argument-error 'set-attribute! "string?" name))
  (unless (string? value)
    (raise-argument-error 'set-attribute! "string?" value))
  (js-set-attribute! (element-unwrap element) name value)
  (void))

;; get-attribute : external? string? -> (or/c #f string?)
;;   Read an element attribute.
(define (get-attribute element name)
  (unless (string? name)
    (raise-argument-error 'get-attribute "string?" name))
  (js-send/value (element-unwrap element) "getAttribute" (vector name)))

;; get-bounding-client-rect : element? -> dom-rect?
;;   Read the element bounding box.
(define (get-bounding-client-rect element)
  (dom-rect-wrap (js-get-bounding-client-rect (element-unwrap element))))

;; get-client-rects : external? -> external/raw
;;   Read the client rect list.
(define (get-client-rects element)
  (js-get-client-rects (element-unwrap element)))

;; query-selector : external? string? -> (or/c #f element?)
;;   Find the first matching descendant.
(define (query-selector element selector)
  (element-wrap (js-element-query-selector (element-unwrap element) selector)))

;; query-selector-all : external? string? -> external/raw
;;   Find all matching descendants.
(define (query-selector-all element selector)
  (js-element-query-selector-all (element-unwrap element) selector))

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

;; set-attribute-ns! : external? string? string? string? -> void?
;;   Set a namespaced attribute.
(define (set-attribute-ns! element ns name value)
  (unless (string? ns)
    (raise-argument-error 'set-attribute-ns! "string?" ns))
  (unless (string? name)
    (raise-argument-error 'set-attribute-ns! "string?" name))
  (unless (string? value)
    (raise-argument-error 'set-attribute-ns! "string?" value))
  (js-set-attribute-ns! (element-unwrap element) ns name value)
  (void))

;; toggle-attribute! : external? string? [boolean?] -> boolean?
;;   Toggle an attribute, or force a specific state when provided.
(define (toggle-attribute! element name [force (void)])
  (unless (string? name)
    (raise-argument-error 'toggle-attribute! "string?" name))
  (if (void? force)
      (js-send/value (element-unwrap element) "toggleAttribute" (vector name))
      (element-i32->boolean (js-toggle-attribute! (element-unwrap element) name (if force 1 0)))))
