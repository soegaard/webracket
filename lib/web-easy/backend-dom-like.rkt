#lang webracket

;;;
;;; web-easy DOM-like Backend
;;;

;; Backend representation and primitive operations for the in-memory DOM-like node model.
;;
;; Exports:
;;   dom-node                DOM-like node constructor.
;;   dom-node?               Predicate for DOM-like nodes.
;;   dom-node-tag            Access node tag symbol.
;;   dom-node-attrs          Access node attributes alist.
;;   dom-node-children       Access child node list.
;;   dom-node-text           Access node text content.
;;   dom-node-on-click       Access node click callback.
;;   dom-node-on-change      Access node change callback.
;;   set-dom-node-tag!       Mutate node tag.
;;   set-dom-node-attrs!     Mutate node attributes.
;;   set-dom-node-children!  Mutate node children.
;;   set-dom-node-text!      Mutate node text content.
;;   set-dom-node-on-click!  Mutate node click callback.
;;   set-dom-node-on-change! Mutate node change callback.
;;   dom-node-native         Return host-native node handle (#f in DOM-like backend).
;;   backend-append-child!   Append child to parent node.
;;   backend-set-single-child!  Replace node children with a single child.
;;   backend-replace-children! Replace node children with a child list.
;;   backend-mount-root!     Mount a root node into a host container (no-op here).
;;   backend-scrollspy-observe-scroll!  Register scroll observer callback (no-op here).
;;   backend-scrollspy-scroll-into-view! Scroll section node into view (no-op here).
;;   backend-scrollspy-active-id  Compute active scrollspy id from section bindings.
;;   backend-set-timeout!    Register a timeout callback (no-op here).
;;   backend-clear-timeout!  Clear timeout callback handle (no-op here).

(define-values
  (dom-node
   dom-node?
   dom-node-tag
   dom-node-attrs
   dom-node-children
   dom-node-text
   dom-node-on-click
   dom-node-on-change
   set-dom-node-tag!
   set-dom-node-attrs!
   set-dom-node-children!
   set-dom-node-text!
   set-dom-node-on-click!
   set-dom-node-on-change!
   dom-node-native
   backend-append-child!
   backend-set-single-child!
   backend-replace-children!
   backend-mount-root!
   backend-scrollspy-observe-scroll!
   backend-scrollspy-scroll-into-view!
   backend-scrollspy-active-id
   backend-set-timeout!
   backend-clear-timeout!)
  (let ()
    (struct dom-node (tag attrs children text on-click on-change) #:mutable #:transparent)

    ;; dom-node-native : dom-node? -> any/c
    ;;   Return #f because this backend has no browser-native node.
    (define (dom-node-native _n)
      #f)

    ;; backend-append-child! : dom-node? dom-node? -> void?
    ;;   Append child to parent's child list.
    (define (backend-append-child! parent child)
      (set-dom-node-children!
       parent
       (append (dom-node-children parent) (list child))))

    ;; backend-set-single-child! : dom-node? dom-node? -> void?
    ;;   Replace node children with a single child.
    (define (backend-set-single-child! parent child)
      (set-dom-node-children! parent (list child)))

    ;; backend-replace-children! : dom-node? list? -> void?
    ;;   Replace node children with children list.
    (define (backend-replace-children! parent children)
      (set-dom-node-children! parent children))

    ;; backend-mount-root! : dom-node? [any/c] -> void?
    ;;   No-op mount hook for the in-memory DOM-like backend.
    ;;   Optional parameter _container defaults to #f.
    (define (backend-mount-root! _root [_container #f])
      (void))

    ;; backend-scrollspy-observe-scroll! : dom-node? (-> void?) (-> (-> void?) void?) -> void?
    ;;   No-op scroll observer registration for non-browser backend.
    (define (backend-scrollspy-observe-scroll! _container _callback _register-cleanup!)
      (void))

    ;; backend-scrollspy-scroll-into-view! : dom-node? -> void?
    ;;   No-op section scrolling in non-browser backend.
    (define (backend-scrollspy-scroll-into-view! _section-node)
      (void))

    ;; backend-scrollspy-active-id : list? -> any/c
    ;;   Return first section id (fallback behavior for non-browser backend).
    (define (backend-scrollspy-active-id section-bindings)
      (if (pair? section-bindings)
          (caar section-bindings)
          #f))

    ;; backend-set-timeout! : number? (-> void?) -> any/c
    ;;   Return #f in DOM-like backend because there is no async timer host.
    (define (backend-set-timeout! _duration-ms _callback)
      #f)

    ;; backend-clear-timeout! : any/c -> void?
    ;;   No-op timeout cleanup in DOM-like backend.
    (define (backend-clear-timeout! _handle)
      (void))

    (values dom-node
            dom-node?
            dom-node-tag
            dom-node-attrs
            dom-node-children
            dom-node-text
            dom-node-on-click
            dom-node-on-change
            set-dom-node-tag!
            set-dom-node-attrs!
            set-dom-node-children!
            set-dom-node-text!
            set-dom-node-on-click!
            set-dom-node-on-change!
            dom-node-native
            backend-append-child!
            backend-set-single-child!
            backend-replace-children!
            backend-mount-root!
            backend-scrollspy-observe-scroll!
            backend-scrollspy-scroll-into-view!
            backend-scrollspy-active-id
            backend-set-timeout!
            backend-clear-timeout!)))
