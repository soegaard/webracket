#lang webracket

;;;
;;; web-easy DOM-like Backend
;;;

;; Backend representation and primitive operations for the in-memory DOM-like node model.
;;
;; Exports:
;;   view-node                            DOM-like node constructor.
;;   view-node?                           Predicate for DOM-like nodes.
;;   view-node-tag                        Access node tag symbol.
;;   view-node-attrs                      Access node attributes alist.
;;   view-node-children                   Access child node list.
;;   view-node-text                       Access node text content.
;;   view-node-on-click                   Access node click callback.
;;   view-node-on-change                  Access node change callback.
;;   view-node-event-handlers             Access generic event callback alist.
;;   set-view-node-tag!                   Mutate node tag.
;;   set-view-node-attrs!                 Mutate node attributes.
;;   set-view-node-children!              Mutate node children.
;;   set-view-node-text!                  Mutate node text content.
;;   set-view-node-on-click!              Mutate node click callback.
;;   set-view-node-on-change!             Mutate node change callback.
;;   set-view-node-event-handlers!        Mutate generic event callback alist.
;;   view-node-native                     Return host-native node handle (#f in DOM-like backend).
;;   backend-append-child!               Append child to parent node.
;;   backend-set-single-child!           Replace node children with a single child.
;;   backend-replace-children!           Replace node children with a child list.
;;   backend-mount-root!                 Mount a root node into a host container (no-op here).
;;   backend-scrollspy-observe-scroll!   Register scroll observer callback (no-op here).
;;   backend-scrollspy-scroll-into-view! Scroll section node into view (no-op here).
;;   backend-scrollspy-active-id         Compute active scrollspy id from section bindings.
;;   backend-set-timeout!                Register a timeout callback (no-op here).
;;   backend-clear-timeout!              Clear timeout callback handle (no-op here).

(define-values
  (view-node
   view-node?
   view-node-tag
   view-node-attrs
   view-node-children
   view-node-text
   view-node-on-click
   view-node-on-change
   view-node-event-handlers
   set-view-node-tag!
   set-view-node-attrs!
   set-view-node-children!
   set-view-node-text!
   set-view-node-on-click!
   set-view-node-on-change!
   set-view-node-event-handlers!
   view-node-native
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
    (struct view-node-record (tag attrs children text on-click on-change event-handlers)
      #:mutable
      #:transparent)

    ;; view-node : symbol? list? list? any/c any/c any/c [list?] -> view-node?
    ;;   Construct a DOM-like node with optional generic event callback alist.
    (define (view-node tag attrs children text on-click on-change [event-handlers '()])
      (view-node-record tag attrs children text on-click on-change event-handlers))

    ;; view-node? : any/c -> boolean?
    ;;   Check whether v is a DOM-like node.
    (define (view-node? v)
      (view-node-record? v))

    ;; view-node-tag : view-node? -> any/c
    ;;   Access node tag.
    (define (view-node-tag n)
      (view-node-record-tag n))

    ;; view-node-attrs : view-node? -> list?
    ;;   Access node attrs.
    (define (view-node-attrs n)
      (view-node-record-attrs n))

    ;; view-node-children : view-node? -> list?
    ;;   Access node children.
    (define (view-node-children n)
      (view-node-record-children n))

    ;; view-node-text : view-node? -> any/c
    ;;   Access node text content.
    (define (view-node-text n)
      (view-node-record-text n))

    ;; view-node-on-click : view-node? -> any/c
    ;;   Access node click callback.
    (define (view-node-on-click n)
      (view-node-record-on-click n))

    ;; view-node-on-change : view-node? -> any/c
    ;;   Access node change callback.
    (define (view-node-on-change n)
      (view-node-record-on-change n))

    ;; view-node-event-handlers : view-node? -> list?
    ;;   Access generic event callback alist.
    (define (view-node-event-handlers n)
      (view-node-record-event-handlers n))

    ;; set-view-node-tag! : view-node? any/c -> void?
    ;;   Mutate node tag.
    (define (set-view-node-tag! n tag)
      (set-view-node-record-tag! n tag))

    ;; set-view-node-attrs! : view-node? list? -> void?
    ;;   Mutate node attrs.
    (define (set-view-node-attrs! n attrs)
      (set-view-node-record-attrs! n attrs))

    ;; set-view-node-children! : view-node? list? -> void?
    ;;   Mutate node children.
    (define (set-view-node-children! n children)
      (set-view-node-record-children! n children))

    ;; set-view-node-text! : view-node? any/c -> void?
    ;;   Mutate node text content.
    (define (set-view-node-text! n text)
      (set-view-node-record-text! n text))

    ;; set-view-node-on-click! : view-node? any/c -> void?
    ;;   Mutate node click callback.
    (define (set-view-node-on-click! n on-click)
      (set-view-node-record-on-click! n on-click))

    ;; set-view-node-on-change! : view-node? any/c -> void?
    ;;   Mutate node change callback.
    (define (set-view-node-on-change! n on-change)
      (set-view-node-record-on-change! n on-change))

    ;; set-view-node-event-handlers! : view-node? list? -> void?
    ;;   Mutate generic event callback alist.
    (define (set-view-node-event-handlers! n event-handlers)
      (set-view-node-record-event-handlers! n event-handlers))

    ;; view-node-native : view-node? -> any/c
    ;;   Return #f because this backend has no browser-native node.
    (define (view-node-native _n)
      #f)

    ;; backend-append-child! : view-node? view-node? -> void?
    ;;   Append child to parent's child list.
    (define (backend-append-child! parent child)
      (set-view-node-children!
       parent
       (append (view-node-children parent) (list child))))

    ;; backend-set-single-child! : view-node? view-node? -> void?
    ;;   Replace node children with a single child.
    (define (backend-set-single-child! parent child)
      (set-view-node-children! parent (list child)))

    ;; backend-replace-children! : view-node? list? -> void?
    ;;   Replace node children with children list.
    (define (backend-replace-children! parent children)
      (set-view-node-children! parent children))

    ;; backend-mount-root! : view-node? [any/c] -> void?
    ;;   No-op mount hook for the in-memory DOM-like backend.
    ;;   Optional parameter _container defaults to #f.
    (define (backend-mount-root! _root [_container #f])
      (void))

    ;; backend-scrollspy-observe-scroll! : view-node? (-> void?) (-> (-> void?) void?) -> void?
    ;;   No-op scroll observer registration for non-browser backend.
    (define (backend-scrollspy-observe-scroll! _container _callback _register-cleanup!)
      (void))

    ;; backend-scrollspy-scroll-into-view! : view-node? -> void?
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

    (values view-node
            view-node?
            view-node-tag
            view-node-attrs
            view-node-children
            view-node-text
            view-node-on-click
            view-node-on-change
            view-node-event-handlers
            set-view-node-tag!
            set-view-node-attrs!
            set-view-node-children!
            set-view-node-text!
            set-view-node-on-click!
            set-view-node-on-change!
            set-view-node-event-handlers!
            view-node-native
            backend-append-child!
            backend-set-single-child!
            backend-replace-children!
            backend-mount-root!
            backend-scrollspy-observe-scroll!
            backend-scrollspy-scroll-into-view!
            backend-scrollspy-active-id
            backend-set-timeout!
            backend-clear-timeout!)))
