#lang racket/base

(require racket/contract/base)

;; Docs-only fake bindings for element Scribble links.
;;
;; These bindings are only for for-label use so examples and API entries
;; can link to the documented element identifiers.

(provide
  (struct-out element)
  (struct-out dom-element)
  (struct-out dom-token-list)
  (struct-out shadow-root)
  (struct-out dom-shadow-root)
  (struct-out node-list)
  (struct-out dom-node-list)
  (struct-out html-collection)
  (struct-out animation)
  (struct-out dom-animation)
  (struct-out computed-style-map)
  (struct-out dom-computed-style-map)
  element-id
  element-set-id!
  element-class-name
  element-set-class-name!
  element-class-list
  element-tag-name
  element-local-name
  element-namespace-uri
  element-prefix
  element-is-connected?
  element-add-event-listener!
  element-remove-event-listener!
  element-append!
  element-prepend!
  element-before!
  element-after!
  element-set-attribute!
  element-get-attribute
  element-has-attributes?
  element-has-attribute?
  element-has-attribute-ns?
  element-has-pointer-capture?
  element-remove-attribute!
  element-remove-attribute-ns!
  element-matches?
  element-closest
  element-get-attribute-names
  element-get-attribute-node
  element-get-attribute-node-ns
  element-set-attribute-node!
  element-set-attribute-node-ns!
  element-remove-attribute-node!
  element-parent-element
  element-previous-element-sibling
  element-next-element-sibling
  element-get-elements-by-class-name
  element-get-elements-by-tag-name
  element-get-elements-by-tag-name-ns
  element-children
  element-scroll-top
  element-set-scroll-top!
  element-scroll-left
  element-set-scroll-left!
  element-scroll-width
  element-scroll-height
  element-client-width
  element-client-height
  element-offset-width
  element-offset-height
  element-child-element-count
  element-first-element-child
  element-last-element-child
  element-inner-html
  element-set-inner-html!
  element-outer-html
  element-set-outer-html!
  element-text-content
  element-set-text-content!
  element-computed-style-map
  element-get-animations
  element-shadow-root
  element-attach-shadow!
  element-animate
  element-query-selector
  element-query-selector-all
  element-get-bounding-client-rect
  element-get-client-rects
  element-insert-adjacent-element!
  element-insert-adjacent-html!
  element-insert-adjacent-text!
  element-remove!
  element-replace-children!
  element-replace-with!
  element-request-fullscreen
  element-request-pointer-lock
  element-set-pointer-capture!
  element-release-pointer-capture!
  element-scroll!
  element-scroll-by!
  element-scroll-into-view!
  element-scroll-to!
  dom-token-list-value
  dom-token-list-length
  dom-token-list-item
  dom-token-list-contains?
  dom-token-list-add!
  dom-token-list-remove!
  dom-token-list-toggle!
  dom-token-list-replace!
  node-list-length
  node-list-item
  html-collection-length
  html-collection-item
  html-collection-named-item
  shadow-root-host
  shadow-root-mode
  shadow-root-delegates-focus?
  (for-label (all-defined-out)))

(struct element (raw) #:transparent)
(struct dom-element (raw) #:transparent)
(struct dom-token-list (raw) #:transparent)
(struct shadow-root (raw) #:transparent)
(struct dom-shadow-root (raw) #:transparent)
(struct node-list (raw) #:transparent)
(struct dom-node-list (raw) #:transparent)
(struct html-collection (raw) #:transparent)
(struct animation (raw) #:transparent)
(struct dom-animation (raw) #:transparent)
(struct computed-style-map (raw) #:transparent)
(struct dom-computed-style-map (raw) #:transparent)

(define element-id any/c)
(define element-set-id! any/c)
(define element-class-name any/c)
(define element-set-class-name! any/c)
(define element-class-list any/c)
(define element-tag-name any/c)
(define element-local-name any/c)
(define element-namespace-uri any/c)
(define element-prefix any/c)
(define element-is-connected? any/c)
(define element-add-event-listener! any/c)
(define element-remove-event-listener! any/c)
(define element-append! any/c)
(define element-prepend! any/c)
(define element-before! any/c)
(define element-after! any/c)
(define element-set-attribute! any/c)
(define element-get-attribute any/c)
(define element-has-attributes? any/c)
(define element-has-attribute? any/c)
(define element-has-attribute-ns? any/c)
(define element-has-pointer-capture? any/c)
(define element-remove-attribute! any/c)
(define element-remove-attribute-ns! any/c)
(define element-matches? any/c)
(define element-closest any/c)
(define element-get-attribute-names any/c)
(define element-get-attribute-node any/c)
(define element-get-attribute-node-ns any/c)
(define element-set-attribute-node! any/c)
(define element-set-attribute-node-ns! any/c)
(define element-remove-attribute-node! any/c)
(define element-parent-element any/c)
(define element-previous-element-sibling any/c)
(define element-next-element-sibling any/c)
(define element-get-elements-by-class-name any/c)
(define element-get-elements-by-tag-name any/c)
(define element-get-elements-by-tag-name-ns any/c)
(define element-children any/c)
(define element-scroll-top any/c)
(define element-set-scroll-top! any/c)
(define element-scroll-left any/c)
(define element-set-scroll-left! any/c)
(define element-scroll-width any/c)
(define element-scroll-height any/c)
(define element-client-width any/c)
(define element-client-height any/c)
(define element-offset-width any/c)
(define element-offset-height any/c)
(define element-child-element-count any/c)
(define element-first-element-child any/c)
(define element-last-element-child any/c)
(define element-inner-html any/c)
(define element-set-inner-html! any/c)
(define element-outer-html any/c)
(define element-set-outer-html! any/c)
(define element-text-content any/c)
(define element-set-text-content! any/c)
(define element-computed-style-map any/c)
(define element-get-animations any/c)
(define element-shadow-root any/c)
(define element-attach-shadow! any/c)
(define element-animate any/c)
(define element-query-selector any/c)
(define element-query-selector-all any/c)
(define element-get-bounding-client-rect any/c)
(define element-get-client-rects any/c)
(define element-insert-adjacent-element! any/c)
(define element-insert-adjacent-html! any/c)
(define element-insert-adjacent-text! any/c)
(define element-remove! any/c)
(define element-replace-children! any/c)
(define element-replace-with! any/c)
(define element-request-fullscreen any/c)
(define element-request-pointer-lock any/c)
(define element-set-pointer-capture! any/c)
(define element-release-pointer-capture! any/c)
(define element-scroll! any/c)
(define element-scroll-by! any/c)
(define element-scroll-into-view! any/c)
(define element-scroll-to! any/c)
(define dom-token-list-value any/c)
(define dom-token-list-length any/c)
(define dom-token-list-item any/c)
(define dom-token-list-contains? any/c)
(define dom-token-list-add! any/c)
(define dom-token-list-remove! any/c)
(define dom-token-list-toggle! any/c)
(define dom-token-list-replace! any/c)
(define node-list-length any/c)
(define node-list-item any/c)
(define html-collection-length any/c)
(define html-collection-item any/c)
(define html-collection-named-item any/c)
(define shadow-root-host any/c)
(define shadow-root-mode any/c)
(define shadow-root-delegates-focus? any/c)
