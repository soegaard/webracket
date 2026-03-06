;;;
;;; web-easy Test Body
;;;

;; Shared test definitions and assertions included by both test entrypoints.

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal label)))

(define (check-exn thunk label)
  (define raised? #f)
  (with-handlers ([exn:fail? (lambda (_e)
                               (set! raised? #t)
                               #t)])
    (thunk)
    #f)
  (unless raised?
    (error 'check-exn label)))

(define (node-child n idx)
  (list-ref (dom-node-children n) idx))

(define (node-texts nodes)
  (map dom-node-text nodes))

(define (node-attr n key)
  (define p (assq key (dom-node-attrs n)))
  (if p (cdr p) #f))

(define (node-child-by-role n role)
  (let loop ([children (dom-node-children n)])
    (cond
      [(null? children)
       (error 'node-child-by-role "missing child role")]
      [else
       (define child (car children))
       (if (equal? (node-attr child 'role) role)
           child
           (loop (cdr children)))])))

(define (node-child-by-widget n widget)
  (let loop ([children (dom-node-children n)])
    (cond
      [(null? children)
       (error 'node-child-by-widget "missing child data-we-widget")]
      [else
       (define child (car children))
       (if (equal? (node-attr child 'data-we-widget) widget)
           child
           (loop (cdr children)))])))

;; obs-update notifies observers
(define o (obs 0))
(define seen '())
(define (watcher v)
  (set! seen (cons v seen)))
(obs-observe! o watcher)
(check-equal (obs-update! o add1) 1 "obs-update result")
(check-equal (obs-peek o) 1 "obs-peek after update")
(check-equal seen '(1) "observer received update")

;; derived observable is read-only
(define base (obs 10))
(define derived (obs-map base add1))
(check-exn (lambda ()
             (obs-update! derived add1))
           "derived observable update")

;; ~#> filters observable updates
(define @n (@ 0))
(define @even (~#> @n even?))
(check-equal (obs-peek @even) 0 "filtered observable initial value")
(:= @n 1)
(check-equal (obs-peek @even) 0 "filtered observable ignores non-matching updates")
(:= @n 2)
(check-equal (obs-peek @even) 2 "filtered observable accepts matching updates")

;; obs-filter supports explicit default value
(define @n2 (@ 1))
(define @even-default (obs-filter @n2 even? 'none))
(check-equal (obs-peek @even-default) 'none "filtered observable default when initial value fails predicate")
(:= @n2 3)
(check-equal (obs-peek @even-default) 'none "filtered observable default remains on non-matching updates")
(:= @n2 4)
(check-equal (obs-peek @even-default) 4 "filtered observable updates when predicate matches")

;; ~#> with always-false predicate never updates after initialization
(define @n3 (@ 5))
(define @never (~#> @n3 (lambda (_v) #f)))
(check-equal (obs-peek @never) #f "always-false filter initial value")
(:= @n3 6)
(check-equal (obs-peek @never) #f "always-false filter remains unchanged after update")

;; λ<~ returns a thunk that updates an observable
(define @counter-for-thunk (@ 10))
(define inc-counter! (λ<~ @counter-for-thunk add1))
(check-equal (obs-peek @counter-for-thunk) 10 "lambda-update thunk initial value")
(inc-counter!)
(check-equal (obs-peek @counter-for-thunk) 11 "lambda-update thunk updates observable")

;; render text reacts to observable changes and button click mutates
(define @count (@ 0))
(define r
  (render
   (window
    (vpanel
     (text (~> @count number->string))
     (button "+" (lambda () (<~ @count add1)))))))
(define root (renderer-root r))
(define panel (node-child root 0))
(define label-node (node-child panel 0))
(define plus-node (node-child panel 1))
(define root-style-node (node-child root 1))
(check-equal (node-attr root 'data-we-widget) "window" "window data-we-widget attr")
(check-equal (node-attr panel 'data-we-widget) "vpanel" "vpanel data-we-widget attr")
(check-equal (node-attr panel 'class) "we-vpanel" "vpanel base class")
(check-equal (node-attr label-node 'data-we-widget) "text" "text data-we-widget attr")
(check-equal (dom-node-tag root-style-node) 'style "window includes shared style node")
(check-equal (dom-node-text label-node) "0" "initial text")
(check-equal (node-attr plus-node 'data-we-widget) "button" "button data-we-widget attr")
(check-equal (node-attr plus-node 'class) "we-button" "button base class")
(dom-node-click! plus-node)
(check-equal (dom-node-text label-node) "1" "text after one click")
(dom-node-click! plus-node)
(check-equal (dom-node-text label-node) "2" "text after two clicks")

;; λ<~ can be used directly as a button action
(define @count-lambda (@ 0))
(define r-lambda
  (render
   (window
    (vpanel
     (text (~> @count-lambda number->string))
     (button "inc" (λ<~ @count-lambda add1))))))
(define panel-lambda (node-child (renderer-root r-lambda) 0))
(define label-node-lambda (node-child panel-lambda 0))
(define plus-node-lambda (node-child panel-lambda 1))
(check-equal (dom-node-text label-node-lambda) "0" "lambda-action initial text")
(dom-node-click! plus-node-lambda)
(check-equal (dom-node-text label-node-lambda) "1" "lambda-action text after click")

;; hpanel renders with class-based row layout
(define r-hpanel
  (render
   (window
    (hpanel
     (text "left")
     (text "right")))))
(define hpanel-node (node-child (renderer-root r-hpanel) 0))
(check-equal (node-attr hpanel-node 'data-we-widget) "hpanel" "hpanel data-we-widget attr")
(check-equal (node-attr hpanel-node 'class) "we-hpanel" "hpanel base class")

;; layout primitives render expected widget/class attrs and basic structure
(define r-layout-primitives
  (render
   (window
    (container
     (stack
      (inline
       (text "left")
       (spacer 2)
       (text "right"))
      (grid 2
            (text "a")
            (text "b")))))))
(define container-node (node-child (renderer-root r-layout-primitives) 0))
(define stack-layout-node (node-child container-node 0))
(define inline-layout-node (node-child stack-layout-node 0))
(define grid-layout-node (node-child stack-layout-node 1))
(define spacer-layout-node (node-child inline-layout-node 1))
(check-equal (node-attr container-node 'data-we-widget) "container" "container data-we-widget attr")
(check-equal (node-attr container-node 'class) "we-container" "container class")
(check-equal (node-attr stack-layout-node 'data-we-widget) "stack" "stack data-we-widget attr")
(check-equal (node-attr stack-layout-node 'class) "we-stack" "stack class")
(check-equal (node-attr inline-layout-node 'data-we-widget) "inline" "inline data-we-widget attr")
(check-equal (node-attr inline-layout-node 'class) "we-inline" "inline class")
(check-equal (node-attr grid-layout-node 'data-we-widget) "grid" "grid data-we-widget attr")
(check-equal (node-attr grid-layout-node 'class) "we-grid" "grid class")
(check-equal (node-attr spacer-layout-node 'data-we-widget) "spacer" "spacer data-we-widget attr")
(check-equal (node-attr spacer-layout-node 'class) "we-spacer" "spacer class")
(check-equal (node-attr spacer-layout-node 'style) "flex-grow:2;" "spacer grow style")
(check-equal (node-attr grid-layout-node 'style) "--we-grid-columns:repeat(2,minmax(0,1fr));" "grid columns style")
(check-equal (length (dom-node-children grid-layout-node)) 2 "grid child count")

;; navigation-bar renders navigation container with child actions
(define r-navigation-bar
  (render
   (window
    (navigation-bar
     (button "home" (lambda () (void)))
     (button "docs" (lambda () (void)))))))
(define navigation-bar-node (node-child (renderer-root r-navigation-bar) 0))
(check-equal (node-attr navigation-bar-node 'data-we-widget) "navigation-bar" "navigation-bar data-we-widget attr")
(check-equal (node-attr navigation-bar-node 'class) "we-navigation-bar" "navigation-bar base class")
(check-equal (node-attr navigation-bar-node 'role) 'navigation "navigation-bar role attr")

;; navigation-bar supports orientation + collapsed state with toggle expansion
(define r-navigation-bar-collapsed
  (render
   (window
    (navigation-bar 'vertical #t 'always
                    (button "one" (lambda () (void)))
                    (button "two" (lambda () (void)))))))
(define navigation-bar-collapsed-node (node-child (renderer-root r-navigation-bar-collapsed) 0))
(define navigation-bar-toggle-node (node-child navigation-bar-collapsed-node 0))
(check-equal (node-attr navigation-bar-collapsed-node 'class)
             "we-navigation-bar is-vertical is-collapsed"
             "navigation-bar collapsed + vertical class")
(check-equal (node-attr navigation-bar-toggle-node 'aria-expanded)
             "false"
             "navigation-bar toggle aria-expanded false when collapsed")
(dom-node-click! navigation-bar-toggle-node)
(check-equal (node-attr navigation-bar-collapsed-node 'class)
             "we-navigation-bar is-vertical"
             "navigation-bar class after toggle expand")
(check-equal (node-attr navigation-bar-toggle-node 'aria-expanded)
             "true"
             "navigation-bar toggle aria-expanded true when expanded")

;; close-button renders class/aria semantics and action callback
(define @close-count (@ 0))
(define r-close-button
  (render
   (window
    (vpanel
     (close-button (lambda ()
                     (:= @close-count (+ (obs-peek @close-count) 1)))
                   "Close panel")))))
(define close-button-node (node-child (node-child (renderer-root r-close-button) 0) 0))
(check-equal (node-attr close-button-node 'data-we-widget) "close-button" "close-button data-we-widget attr")
(check-equal (node-attr close-button-node 'class) "we-close-button" "close-button class")
(check-equal (node-attr close-button-node 'aria-label) "Close panel" "close-button aria-label")
(dom-node-click! close-button-node)
(check-equal (obs-peek @close-count) 1 "close-button action callback")

;; placeholder renders shape classes and width attribute from observables
(define @placeholder-shape (@ 'text))
(define @placeholder-width (@ "10em"))
(define r-placeholder
  (render
   (window
    (vpanel
     (placeholder @placeholder-shape @placeholder-width)))))
(define placeholder-node (node-child (node-child (renderer-root r-placeholder) 0) 0))
(check-equal (node-attr placeholder-node 'data-we-widget) "placeholder" "placeholder data-we-widget attr")
(check-equal (node-attr placeholder-node 'width) "10em" "placeholder width attr")
(check-equal (node-attr placeholder-node 'class) "we-placeholder we-placeholder-text" "placeholder initial class")
(:= @placeholder-shape 'circle)
(check-equal (node-attr placeholder-node 'class) "we-placeholder we-placeholder-circle" "placeholder circle class")

;; offcanvas toggles open class and side class from observables
(define @off-open (@ #f))
(define @off-side (@ 'end))
(define r-offcanvas
  (render
   (window
    (vpanel
     (offcanvas @off-open
                (lambda ()
                  (:= @off-open #f))
                @off-side
                (text "body"))))))
(define off-root-node (node-child (node-child (renderer-root r-offcanvas) 0) 0))
(define off-panel-node (node-child-by-widget off-root-node "offcanvas-panel"))
(check-equal (node-attr off-root-node 'class) "we-offcanvas" "offcanvas initial class")
(check-equal (node-attr off-panel-node 'class) "we-offcanvas-panel is-end" "offcanvas panel initial side")
(:= @off-open #t)
(check-equal (node-attr off-root-node 'class) "we-offcanvas is-open" "offcanvas open class")
(:= @off-side 'start)
(check-equal (node-attr off-panel-node 'class) "we-offcanvas-panel is-start" "offcanvas panel start side")

;; carousel updates index via prev/next button actions
(define @carousel-index (@ 0))
(define carousel-items
  (list (list 0 "a" (text "A"))
        (list 1 "b" (text "B"))
        (list 2 "c" (text "C"))))
(define r-carousel
  (render
   (window
    (vpanel
     (carousel carousel-items
               @carousel-index
               (lambda (next-index)
                 (:= @carousel-index next-index)))))))
(define carousel-node (node-child (node-child (renderer-root r-carousel) 0) 0))
(define carousel-controls (node-child-by-widget carousel-node "carousel-controls"))
(define carousel-next (node-child-by-widget carousel-controls "carousel-next"))
(dom-node-click! carousel-next)
(check-equal (obs-peek @carousel-index) 1 "carousel next action")

;; carousel with wrap disabled keeps index at boundary and marks next as disabled
(define @carousel-index-no-wrap (@ 2))
(define r-carousel-no-wrap
  (render
   (window
    (vpanel
     (carousel carousel-items
               @carousel-index-no-wrap
               (lambda (next-index)
                 (:= @carousel-index-no-wrap next-index))
               #f
               #f)))))
(define carousel-node-no-wrap (node-child (node-child (renderer-root r-carousel-no-wrap) 0) 0))
(define carousel-controls-no-wrap (node-child-by-widget carousel-node-no-wrap "carousel-controls"))
(define carousel-next-no-wrap (node-child-by-widget carousel-controls-no-wrap "carousel-next"))
(dom-node-click! carousel-next-no-wrap)
(check-equal (obs-peek @carousel-index-no-wrap) 2 "carousel no-wrap keeps last index on next")
(check-equal (node-attr carousel-next-no-wrap 'aria-disabled) "true" "carousel no-wrap marks next disabled")

;; scrollspy marks selected item current and updates via click action
(define @scroll-current (@ 'home))
(define scroll-sections
  (list (list 'home "Home")
        (list 'docs "Docs")
        (list 'api "API")))
(define r-scrollspy
  (render
   (window
    (vpanel
     (scrollspy scroll-sections
                @scroll-current
                (lambda (id)
                  (:= @scroll-current id)))))))
(define scrollspy-node (node-child (node-child (renderer-root r-scrollspy) 0) 0))
(define scrollspy-nav (node-child scrollspy-node 0))
(define scrollspy-docs (node-child scrollspy-nav 1))
(dom-node-click! scrollspy-docs)
(check-equal (obs-peek @scroll-current) 'docs "scrollspy click action")
(define scrollspy-node/after (node-child (node-child (renderer-root r-scrollspy) 0) 0))
(define scrollspy-nav/after (node-child scrollspy-node/after 0))
(define scrollspy-docs/after (node-child scrollspy-nav/after 1))
(check-equal (node-attr scrollspy-docs/after 'class) "we-scrollspy-item is-current" "scrollspy current class")

;; button-group renders grouped actions and preserves button callbacks
(define @group-count (@ 0))
(define r-button-group
  (render
   (window
    (vpanel
     (button-group
      (button "-" (lambda () (:= @group-count (- (obs-peek @group-count) 1))))
      (button "+" (lambda () (:= @group-count (+ (obs-peek @group-count) 1))))))))))
(define button-group-node (node-child (node-child (renderer-root r-button-group) 0) 0))
(define dec-node (node-child button-group-node 0))
(define inc-node (node-child button-group-node 1))
(check-equal (node-attr button-group-node 'data-we-widget) "button-group" "button-group data-we-widget attr")
(check-equal (node-attr button-group-node 'class) "we-button-group" "button-group base class")
(dom-node-click! inc-node)
(check-equal (obs-peek @group-count) 1 "button-group increment action")
(dom-node-click! dec-node)
(check-equal (obs-peek @group-count) 0 "button-group decrement action")

;; button-toolbar renders grouped controls and routes button actions
(define @left-count  (@ 0))
(define @right-count (@ 10))
(define r-button-toolbar
  (render
   (window
    (vpanel
     (button-toolbar
      (button-group
       (button "L+" (lambda () (:= @left-count (+ (obs-peek @left-count) 1)))))
      (button-group
       (button "R-" (lambda () (:= @right-count (- (obs-peek @right-count) 1))))))))))
(define button-toolbar-node (node-child (node-child (renderer-root r-button-toolbar) 0) 0))
(define left-group-node (node-child button-toolbar-node 0))
(define right-group-node (node-child button-toolbar-node 1))
(define left-plus-node (node-child left-group-node 0))
(define right-minus-node (node-child right-group-node 0))
(check-equal (node-attr button-toolbar-node 'data-we-widget) "button-toolbar" "button-toolbar data-we-widget attr")
(check-equal (node-attr button-toolbar-node 'class) "we-button-toolbar" "button-toolbar base class")
(dom-node-click! left-plus-node)
(check-equal (obs-peek @left-count) 1 "button-toolbar left action")
(dom-node-click! right-minus-node)
(check-equal (obs-peek @right-count) 9 "button-toolbar right action")

;; alert renders severity classes and updates text/role from observables
(define @alert-text (@ "Saved"))
(define @alert-level (@ 'success))
(define r-alert
  (render
   (window
    (vpanel
     (alert @alert-text @alert-level)))))
(define alert-node (node-child (node-child (renderer-root r-alert) 0) 0))
(check-equal (node-attr alert-node 'data-we-widget) "alert" "alert data-we-widget attr")
(check-equal (node-attr alert-node 'class) "we-alert we-alert-success" "alert success class")
(check-equal (node-attr alert-node 'role) 'status "alert success role")
(check-equal (node-attr alert-node 'aria-live) "polite" "alert success aria-live")
(check-equal (dom-node-text alert-node) "Saved" "alert initial text")
(:= @alert-text "Disk almost full")
(:= @alert-level 'warn)
(check-equal (node-attr alert-node 'class) "we-alert we-alert-warn" "alert warn class")
(check-equal (node-attr alert-node 'role) 'alert "alert warn role")
(check-equal (node-attr alert-node 'aria-live) "assertive" "alert warn aria-live")
(check-equal (dom-node-text alert-node) "Disk almost full" "alert text after update")

;; badge renders level classes and updates text from observables
(define @badge-text (@ "beta"))
(define @badge-level (@ 'info))
(define r-badge
  (render
   (window
    (vpanel
     (badge @badge-text @badge-level)))))
(define badge-node (node-child (node-child (renderer-root r-badge) 0) 0))
(check-equal (node-attr badge-node 'data-we-widget) "badge" "badge data-we-widget attr")
(check-equal (node-attr badge-node 'class) "we-badge we-badge-info" "badge info class")
(check-equal (dom-node-text badge-node) "beta" "badge initial text")
(:= @badge-text "stable")
(:= @badge-level 'success)
(check-equal (node-attr badge-node 'class) "we-badge we-badge-success" "badge success class")
(check-equal (dom-node-text badge-node) "stable" "badge text after update")

;; spinner renders icon/label and updates observable label text
(define @spinner-label (@ "Loading..."))
(define r-spinner
  (render
   (window
    (vpanel
     (spinner @spinner-label)))))
(define spinner-node (node-child (node-child (renderer-root r-spinner) 0) 0))
(define spinner-icon-node (node-child spinner-node 0))
(define spinner-label-node (node-child spinner-node 1))
(check-equal (node-attr spinner-node 'data-we-widget) "spinner" "spinner data-we-widget attr")
(check-equal (node-attr spinner-node 'class) "we-spinner" "spinner base class")
(check-equal (node-attr spinner-icon-node 'class) "we-spinner-icon" "spinner icon class")
(check-equal (node-attr spinner-label-node 'class) "we-spinner-label" "spinner label class")
(check-equal (dom-node-text spinner-label-node) "Loading..." "spinner initial label text")
(:= @spinner-label "Syncing")
(check-equal (dom-node-text spinner-label-node) "Syncing" "spinner label updates from observable")

;; toast renders as non-modal notification and closes via dismiss action
(define @toast-open (@ #t))
(define @toast-text (@ "Build complete"))
(define @toast-level (@ 'success))
(define r-toast
  (render
   (window
    (vpanel
     (toast @toast-open
            (lambda () (:= @toast-open #f))
            @toast-text
            @toast-level)))))
(define toast-node (node-child (node-child (renderer-root r-toast) 0) 0))
(define toast-message-node (node-child toast-node 0))
(define toast-close-node (node-child toast-node 1))
(check-equal (node-attr toast-node 'data-we-widget) "toast" "toast data-we-widget attr")
(check-equal (node-attr toast-node 'class) "we-toast we-toast-success is-open" "toast initial class")
(check-equal (node-attr toast-node 'aria-hidden) "false" "toast initially visible")
(check-equal (dom-node-text toast-message-node) "Build complete" "toast message text")
(:= @toast-level 'error)
(check-equal (node-attr toast-node 'class) "we-toast we-toast-error is-open" "toast error class")
(dom-node-click! toast-close-node)
(check-equal (obs-peek @toast-open) #f "toast close action updates open state")
(check-equal (node-attr toast-node 'aria-hidden) "true" "toast hidden after close")

;; toast supports optional title and non-dismissible mode
(define @toast-open-2 (@ #t))
(define @toast-title (@ "Sync"))
(define r-toast-2
  (render
   (window
    (vpanel
     (toast @toast-open-2
            (lambda () (:= @toast-open-2 #f))
            "Please wait"
            'info
            @toast-title
            #f)))))
(define toast-node-2 (node-child (node-child (renderer-root r-toast-2) 0) 0))
(define toast-title-node-2 (node-child toast-node-2 0))
(define toast-message-node-2 (node-child toast-node-2 1))
(check-equal (node-attr toast-title-node-2 'class) "we-toast-title" "toast title class")
(check-equal (dom-node-text toast-title-node-2) "Sync" "toast title initial text")
(check-equal (dom-node-text toast-message-node-2) "Please wait" "toast message initial text")
(check-equal (length (dom-node-children toast-node-2)) 2 "toast non-dismissible has no close button")
(:= @toast-title "Syncing")
(define toast-title-node-2-after (node-child toast-node-2 0))
(check-equal (dom-node-text toast-title-node-2-after) "Syncing" "toast title observable update")

;; modal renders modal semantics and updates open/aria state from observable
(define @modal-open (@ #f))
(define r-modal
  (render
   (window
    (vpanel
     (modal @modal-open
            (lambda ()
              (:= @modal-open #f))
            (text "Modal body"))))))
(define modal-node (node-child (node-child (renderer-root r-modal) 0) 0))
(check-equal (node-attr modal-node 'data-we-widget) "modal" "modal data-we-widget attr")
(check-equal (node-attr modal-node 'class) "we-modal" "modal base class")
(check-equal (node-attr modal-node 'aria-hidden) "true" "modal hidden initially")
(:= @modal-open #t)
(check-equal (node-attr modal-node 'class) "we-modal is-open" "modal open class")
(check-equal (node-attr modal-node 'aria-hidden) "false" "modal visible aria")

;; pagination renders page buttons and updates current page on click
(define @page (@ 2))
(define r-pagination
  (render
   (window
    (vpanel
     (pagination 4 @page (lambda (new-page) (:= @page new-page)))))))
(define pagination-node (node-child (node-child (renderer-root r-pagination) 0) 0))
(define (find-page-button-by-label children label)
  (let loop ([rest children])
    (cond
      [(null? rest) #f]
      [else
       (define child (car rest))
       (if (and (equal? (node-attr child 'data-we-widget) "page-button")
                (string=? (dom-node-text child) label))
           child
           (loop (cdr rest)))])))
(define pagination-buttons (dom-node-children pagination-node))
(define first-button (find-page-button-by-label pagination-buttons "First"))
(define page1-button (find-page-button-by-label pagination-buttons "1"))
(define page2-button (find-page-button-by-label pagination-buttons "2"))
(define next-button (find-page-button-by-label pagination-buttons "Next"))
(check-equal (node-attr pagination-node 'data-we-widget) "pagination" "pagination data-we-widget attr")
(check-equal (node-attr first-button 'class) "we-page-btn" "pagination first button present")
(check-equal (node-attr page2-button 'class) "we-page-btn is-current" "pagination current page class")
(check-equal (node-attr page2-button 'aria-current) "page" "pagination current page aria")
(dom-node-click! next-button)
(check-equal (obs-peek @page) 3 "pagination next button updates page")
(dom-node-click! page1-button)
(check-equal (obs-peek @page) 1 "pagination page button updates page")
(define pagination-buttons-after (dom-node-children pagination-node))
(define prev-button-after (find-page-button-by-label pagination-buttons-after "Prev"))
(check-equal (node-attr prev-button-after 'class) "we-page-btn is-disabled" "pagination prev disabled at first page")

;; pagination compact mode adds ellipsis + first/last controls for large page-count
(define @page-large (@ 10))
(define r-pagination-large
  (render
   (window
    (vpanel
     (pagination 20 @page-large (lambda (new-page) (:= @page-large new-page)))))))
(define pagination-large-node (node-child (node-child (renderer-root r-pagination-large) 0) 0))
(define pagination-large-children (dom-node-children pagination-large-node))
(define ellipsis-nodes
  (filter (lambda (child)
            (equal? (node-attr child 'data-we-widget) "page-ellipsis"))
          pagination-large-children))
(define last-button-large (find-page-button-by-label pagination-large-children "Last"))
(check-equal (length ellipsis-nodes) 2 "pagination compact mode renders two ellipses")
(dom-node-click! last-button-large)
(check-equal (obs-peek @page-large) 20 "pagination last button jumps to final page")

;; breadcrumb renders separators and updates current item class after navigation
(define @crumb-current (@ 'docs))
(define r-breadcrumb
  (render
   (window
    (vpanel
     (breadcrumb '((home "Home") (docs "Docs") (api "API"))
                 @crumb-current
                 (lambda (new-id) (:= @crumb-current new-id)))))))
(define breadcrumb-node (node-child (node-child (renderer-root r-breadcrumb) 0) 0))
(define breadcrumb-children (dom-node-children breadcrumb-node))
(define breadcrumb-home (list-ref breadcrumb-children 0))
(define breadcrumb-docs (list-ref breadcrumb-children 2))
(define breadcrumb-api (list-ref breadcrumb-children 4))
(check-equal (node-attr breadcrumb-node 'data-we-widget) "breadcrumb" "breadcrumb data-we-widget attr")
(check-equal (node-attr breadcrumb-docs 'class) "we-breadcrumb-item is-current" "breadcrumb current item class")
(check-equal (node-attr breadcrumb-docs 'aria-current) "page" "breadcrumb current aria")
(dom-node-click! breadcrumb-home)
(check-equal (obs-peek @crumb-current) 'home "breadcrumb click updates current id")
(define breadcrumb-children-after (dom-node-children breadcrumb-node))
(define breadcrumb-home-after (list-ref breadcrumb-children-after 0))
(define breadcrumb-docs-after (list-ref breadcrumb-children-after 2))
(check-equal (node-attr breadcrumb-home-after 'class) "we-breadcrumb-item is-current" "breadcrumb home class after click")
(check-equal (node-attr breadcrumb-docs-after 'class) "we-breadcrumb-item" "breadcrumb docs class after click")
(check-equal (dom-node-text breadcrumb-api) "API" "breadcrumb trailing label text")

;; list-group renders current item and updates selection on click
(define @list-group-current (@ 'b))
(define r-list-group
  (render
   (window
    (vpanel
     (list-group '((a "Alpha") (b "Beta") (c "Gamma"))
                 @list-group-current
                 (lambda (new-id) (:= @list-group-current new-id)))))))
(define list-group-node (node-child (node-child (renderer-root r-list-group) 0) 0))
(define list-group-items (dom-node-children list-group-node))
(define list-group-a (list-ref list-group-items 0))
(define list-group-b (list-ref list-group-items 1))
(check-equal (node-attr list-group-node 'data-we-widget) "list-group" "list-group data-we-widget attr")
(check-equal (node-attr list-group-b 'class) "we-list-group-item is-current" "list-group current class")
(dom-node-click! list-group-a)
(check-equal (obs-peek @list-group-current) 'a "list-group click updates current id")
(define list-group-items-after (dom-node-children list-group-node))
(define list-group-a-after (list-ref list-group-items-after 0))
(check-equal (node-attr list-group-a-after 'class) "we-list-group-item is-current" "list-group class after selection")

;; collapse toggles visibility class and aria-hidden from observable state
(define @collapse-open (@ #f))
(define r-collapse
  (render
   (window
    (vpanel
     (collapse @collapse-open
               (text "secret-panel"))))))
(define collapse-node (node-child (node-child (renderer-root r-collapse) 0) 0))
(define collapse-child (node-child collapse-node 0))
(check-equal (node-attr collapse-node 'data-we-widget) "collapse" "collapse data-we-widget attr")
(check-equal (node-attr collapse-node 'class) "we-collapse" "collapse initial class")
(check-equal (node-attr collapse-node 'aria-hidden) "true" "collapse initially hidden")
(check-equal (dom-node-text collapse-child) "secret-panel" "collapse child rendered")
(:= @collapse-open #t)
(check-equal (node-attr collapse-node 'class) "we-collapse is-open" "collapse open class")
(check-equal (node-attr collapse-node 'aria-hidden) "false" "collapse open aria")
(:= @collapse-open #f)
(check-equal (node-attr collapse-node 'class) "we-collapse" "collapse closed class after toggle")
(check-equal (node-attr collapse-node 'aria-hidden) "true" "collapse closed aria after toggle")

;; accordion toggles selected section and updates collapse state
(define @accordion-selected (@ 'overview))
(define r-accordion
  (render
   (window
    (accordion @accordion-selected
               (list (list 'overview "Overview" (text "overview-body"))
                     (list 'details "Details" (text "details-body")))))))
(define accordion-root (node-child (renderer-root r-accordion) 0))
(define accordion-section-0 (node-child accordion-root 0))
(define accordion-section-1 (node-child accordion-root 1))
(define accordion-button-0 (node-child accordion-section-0 0))
(define accordion-button-1 (node-child accordion-section-1 0))
(define accordion-collapse-0 (node-child accordion-section-0 1))
(define accordion-collapse-1 (node-child accordion-section-1 1))
(check-equal (node-attr accordion-root 'data-we-widget) "accordion" "accordion root data-we-widget attr")
(check-equal (node-attr accordion-button-0 'data-we-widget) "accordion-trigger" "accordion first header widget tag")
(check-equal (node-attr accordion-collapse-0 'class) "we-collapse is-open" "accordion first section initially open")
(check-equal (node-attr accordion-collapse-1 'class) "we-collapse" "accordion second section initially closed")
(dom-node-click! accordion-button-1)
(check-equal (obs-peek @accordion-selected) 'details "accordion click selects details")
(check-equal (node-attr accordion-collapse-0 'class) "we-collapse" "accordion first section closes after switching")
(check-equal (node-attr accordion-collapse-1 'class) "we-collapse is-open" "accordion second section opens after switching")
(dom-node-click! accordion-button-1)
(check-equal (obs-peek @accordion-selected) #f "accordion click on selected section clears selection")
(check-equal (node-attr accordion-collapse-1 'class) "we-collapse" "accordion selected section closes on second click")
(dom-node-keydown! accordion-button-0 "ArrowDown")
(check-equal (obs-peek @accordion-selected) 'details "accordion ArrowDown moves to next section")
(dom-node-keydown! accordion-button-1 "ArrowUp")
(check-equal (obs-peek @accordion-selected) 'overview "accordion ArrowUp moves to previous section")
(dom-node-keydown! accordion-button-0 "End")
(check-equal (obs-peek @accordion-selected) 'details "accordion End selects last section")
(dom-node-keydown! accordion-button-1 "Home")
(check-equal (obs-peek @accordion-selected) 'overview "accordion Home selects first section")
(dom-node-keydown! accordion-button-0 "Enter")
(check-equal (obs-peek @accordion-selected) #f "accordion Enter toggles selected section closed")

;; dialog opens from trigger and closes on Escape via on-close callback
(define @dialog-open (@ #f))
(define @dialog-status (@ "idle"))
(define r-dialog
  (render
   (window
    (vpanel
     (button "open-dialog"
             (lambda ()
               (:= @dialog-open #t)
               (:= @dialog-status "open")))
     (dialog @dialog-open
             (lambda ()
               (:= @dialog-open #f)
               (:= @dialog-status "esc-close"))
             (vpanel
              (text "Delete project?")
              (button "cancel"
                      (lambda ()
                        (:= @dialog-open #f)
                        (:= @dialog-status "cancel")))))
     (text (~> @dialog-status
               (lambda (status)
                 (string-append "dialog-status:" status))))))))
(define dialog-panel-parent (node-child (renderer-root r-dialog) 0))
(define open-dialog-button (node-child dialog-panel-parent 0))
(define dialog-node (node-child dialog-panel-parent 1))
(define dialog-panel-node (node-child dialog-node 0))
(define dialog-first-text (node-child dialog-panel-node 0))
(define dialog-status-node (node-child dialog-panel-parent 2))
(check-equal (dom-node-tag dialog-node) 'dialog "dialog tag")
(check-equal (node-attr dialog-node 'role) 'dialog "dialog role attr")
(check-equal (node-attr dialog-node 'data-we-widget) "dialog" "dialog data-we-widget attr")
(check-equal (node-attr dialog-panel-node 'data-we-widget) "dialog-panel" "dialog panel data-we-widget attr")
(check-equal (node-attr dialog-node 'class) "we-dialog" "dialog initial class")
(check-equal (node-attr dialog-panel-node 'class) "we-dialog-panel" "dialog panel class")
(check-equal (node-attr dialog-panel-node 'aria-describedby)
             (node-attr dialog-first-text 'id)
             "dialog panel aria-describedby points at first text child id")
(check-equal (node-attr dialog-node 'aria-hidden) "true" "dialog initially hidden")
(check-equal (dom-node-text dialog-status-node) "dialog-status:idle" "dialog status initial")
(dom-node-click! open-dialog-button)
(check-equal (node-attr dialog-node 'class) "we-dialog is-open" "dialog open class")
(check-equal (node-attr dialog-node 'aria-hidden) "false" "dialog visible after open button")
(check-equal (dom-node-text dialog-status-node) "dialog-status:open" "dialog status after open")
(dom-node-keydown! dialog-node "Escape")
(check-equal (obs-peek @dialog-open) #f "dialog open state after Escape")
(check-equal (node-attr dialog-node 'class) "we-dialog" "dialog class after Escape close")
(check-equal (node-attr dialog-node 'aria-hidden) "true" "dialog hidden after Escape")
(check-equal (dom-node-text dialog-status-node) "dialog-status:esc-close" "dialog status after Escape close")

;; renderer-destroy stops future updates
(define @count2 (@ 0))
(define r2
  (render
   (window
    (vpanel
     (text (~> @count2 number->string))))))
(define label-node2 (node-child (node-child (renderer-root r2) 0) 0))
(check-equal (dom-node-text label-node2) "0" "initial text before destroy")
(renderer-destroy r2)
(<~ @count2 add1)
(check-equal (dom-node-text label-node2) "0" "text unchanged after destroy")

;; list-view supports insert, delete, and reorder with keyed node reuse
(define @items (@ '((1 . "a") (2 . "b") (3 . "c"))))
(define r3
  (render
   (window
    (vpanel
     (list-view @items
                (lambda (_key entry)
                  (text (cdr entry)))
                car)))))
(define list-container
  (node-child (node-child (renderer-root r3) 0) 0))
(check-equal (node-attr list-container 'data-we-widget) "list-view" "list-view data-we-widget attr")
(check-equal (node-attr list-container 'class) "we-list-view" "list-view base class")
(define children0 (dom-node-children list-container))
(define n1 (list-ref children0 0))
(define n2 (list-ref children0 1))
(define n3 (list-ref children0 2))
(check-equal (node-texts children0) '("a" "b" "c") "list-view initial order")

;; Reorder should reuse keyed nodes.
(:= @items '((3 . "c") (1 . "a") (2 . "b")))
(define children1 (dom-node-children list-container))
(check-equal (node-texts children1) '("c" "a" "b") "list-view reordered texts")
(check-equal (eq? (list-ref children1 0) n3) #t "list-view reuse key 3")
(check-equal (eq? (list-ref children1 1) n1) #t "list-view reuse key 1")
(check-equal (eq? (list-ref children1 2) n2) #t "list-view reuse key 2")

;; Insert should add a new node.
(:= @items '((3 . "c") (4 . "d") (1 . "a") (2 . "b")))
(define children2 (dom-node-children list-container))
(check-equal (node-texts children2) '("c" "d" "a" "b") "list-view insert")

;; Delete should remove missing key while preserving others.
(:= @items '((4 . "d") (2 . "b")))
(define children3 (dom-node-children list-container))
(check-equal (node-texts children3) '("d" "b") "list-view delete")
(check-equal (eq? (list-ref children3 1) n2) #t "list-view preserve surviving key")

;; list-view regression for parity dynamic-list sequence
(define @parity-items (@ '((a . 0) (b . 0))))
(define r3b
  (render
   (window
    (vpanel
     (list-view @parity-items
                (lambda (_key entry)
                  (text (string-append (symbol->string (car entry))
                                       ":"
                                       (number->string (cdr entry))))
                  )
                car)))))
(define parity-list-container
  (node-child (node-child (renderer-root r3b) 0) 0))
(define parity-children0 (dom-node-children parity-list-container))
(define parity-a-node (list-ref parity-children0 0))
(define parity-b-node (list-ref parity-children0 1))
(check-equal (node-texts parity-children0) '("a:0" "b:0") "parity list initial state")

;; inc-a
(:= @parity-items '((a . 1) (b . 0)))
(define parity-children1 (dom-node-children parity-list-container))
(check-equal (node-texts parity-children1) '("a:1" "b:0") "parity list after inc-a")
(define parity-a-node* (list-ref parity-children1 0))

;; reorder
(:= @parity-items '((b . 0) (a . 1)))
(define parity-children2 (dom-node-children parity-list-container))
(check-equal (node-texts parity-children2) '("b:0" "a:1") "parity list after reorder")
(check-equal (eq? (list-ref parity-children2 0) parity-b-node) #t "parity list reuses b node after reorder")
(check-equal (eq? (list-ref parity-children2 1) parity-a-node*) #t "parity list reuses a node after reorder")

;; add-c
(:= @parity-items '((b . 0) (a . 1) (c . 0)))
(define parity-children3 (dom-node-children parity-list-container))
(define parity-c-node (list-ref parity-children3 2))
(check-equal (node-texts parity-children3) '("b:0" "a:1" "c:0") "parity list after add-c")
(check-equal (eq? (list-ref parity-children3 1) parity-a-node*) #t "parity list keeps a node after add-c")

;; drop-b
(:= @parity-items '((a . 1) (c . 0)))
(define parity-children4 (dom-node-children parity-list-container))
(check-equal (node-texts parity-children4) '("a:1" "c:0") "parity list after drop-b")
(check-equal (eq? (list-ref parity-children4 0) parity-a-node*) #t "parity list keeps a node after drop-b")
(check-equal (eq? (list-ref parity-children4 1) parity-c-node) #t "parity list keeps c node after drop-b")

;; input supports two-way observable binding
(define @name (@ "alice"))
(define r4
  (render
   (window
    (vpanel
     (input @name (lambda (new-value) (:= @name new-value)))))))
(define input-node (node-child (node-child (renderer-root r4) 0) 0))
(check-equal (node-attr input-node 'value) "alice" "input initial value")
(check-equal (node-attr input-node 'data-we-widget) "input" "input data-we-widget attr")
(check-equal (node-attr input-node 'class) "we-input" "input base class")
(dom-node-change! input-node "bob")
(check-equal (obs-peek @name) "bob" "input change updates observable")
(check-equal (node-attr input-node 'value) "bob" "input value after user change")
(:= @name "carol")
(check-equal (node-attr input-node 'value) "carol" "observable update reflects in input")

;; input Enter callback invokes optional on-enter action
(define @submitted (@ 0))
(define @name-enter (@ "draft"))
(define r4b
  (render
   (window
    (vpanel
     (input @name-enter
            (lambda (new-value) (:= @name-enter new-value))
            (lambda () (<~ @submitted add1)))))))
(define input-enter-node (node-child (node-child (renderer-root r4b) 0) 0))
(check-equal (obs-peek @submitted) 0 "input enter initial submitted count")
(dom-node-keydown! input-enter-node "Escape")
(check-equal (obs-peek @submitted) 0 "input non-enter key does not submit")
(dom-node-keydown! input-enter-node "Enter")
(check-equal (obs-peek @submitted) 1 "input enter key submits once")

;; checkbox supports two-way observable binding
(define @enabled (@ #f))
(define r5
  (render
   (window
    (vpanel
     (checkbox @enabled (lambda (new-value) (:= @enabled new-value)))))))
(define checkbox-node (node-child (node-child (renderer-root r5) 0) 0))
(check-equal (node-attr checkbox-node 'checked) #f "checkbox initial value")
(check-equal (node-attr checkbox-node 'data-we-widget) "checkbox" "checkbox data-we-widget attr")
(check-equal (node-attr checkbox-node 'class) "we-checkbox" "checkbox base class")
(dom-node-toggle! checkbox-node #t)
(check-equal (obs-peek @enabled) #t "checkbox toggle updates observable")
(check-equal (node-attr checkbox-node 'checked) #t "checkbox attr after toggle")
(:= @enabled #f)
(check-equal (node-attr checkbox-node 'checked) #f "observable update reflects in checkbox")

;; choice supports two-way observable binding
(define @color (@ 'red))
(define r6
  (render
   (window
    (vpanel
     (choice '(red green blue)
             @color
             (lambda (new-value) (:= @color new-value)))))))
(define choice-node (node-child (node-child (renderer-root r6) 0) 0))
(check-equal (node-attr choice-node 'selected) 'red "choice initial selected value")
(check-equal (node-attr choice-node 'data-we-widget) "choice" "choice data-we-widget attr")
(check-equal (node-attr choice-node 'class) "we-choice" "choice base class")
(dom-node-select! choice-node 'green)
(check-equal (obs-peek @color) 'green "choice select updates observable")
(check-equal (node-attr choice-node 'selected) 'green "choice attr after selection")
(:= @color 'blue)
(check-equal (node-attr choice-node 'selected) 'blue "observable update reflects in choice")

;; slider supports two-way observable binding
(define @level (@ 25))
(define r7
  (render
   (window
    (vpanel
     (slider @level
             (lambda (new-value) (:= @level new-value))
             0
             100)))))
(define slider-node (node-child (node-child (renderer-root r7) 0) 0))
(check-equal (node-attr slider-node 'min) 0 "slider min attr")
(check-equal (node-attr slider-node 'max) 100 "slider max attr")
(check-equal (node-attr slider-node 'value) 25 "slider initial value")
(check-equal (node-attr slider-node 'data-we-widget) "slider" "slider data-we-widget attr")
(check-equal (node-attr slider-node 'class) "we-slider" "slider base class")
(dom-node-slide! slider-node 70)
(check-equal (obs-peek @level) 70 "slider slide updates observable")
(check-equal (node-attr slider-node 'value) 70 "slider attr after slide")
(:= @level 5)
(check-equal (node-attr slider-node 'value) 5 "observable update reflects in slider")

;; progress supports one-way observable binding
(define @percent (@ 10))
(define r8
  (render
   (window
    (vpanel
     (progress @percent 0 100)))))
(define progress-node (node-child (node-child (renderer-root r8) 0) 0))
(check-equal (node-attr progress-node 'min) 0 "progress min attr")
(check-equal (node-attr progress-node 'max) 100 "progress max attr")
(check-equal (node-attr progress-node 'value) 10 "progress initial value")
(check-equal (node-attr progress-node 'data-we-widget) "progress" "progress data-we-widget attr")
(check-equal (node-attr progress-node 'class) "we-progress we-progress-info" "progress base class")
(:= @percent 65)
(check-equal (node-attr progress-node 'value) 65 "progress reflects observable update")

;; progress supports variant classes
(define @progress-variant (@ 'warn))
(define r8b
  (render
   (window
    (vpanel
     (progress 30 0 100 @progress-variant)))))
(define progress-node-2 (node-child (node-child (renderer-root r8b) 0) 0))
(check-equal (node-attr progress-node-2 'class) "we-progress we-progress-warn" "progress warn variant class")
(:= @progress-variant 'success)
(check-equal (node-attr progress-node-2 'class) "we-progress we-progress-success" "progress success variant class")
(:= @progress-variant 'mystery)
(check-equal (node-attr progress-node-2 'class) "we-progress we-progress-info" "progress unknown variant falls back to info class")

;; group supports labeled container semantics
(define @group-label (@ "Settings"))
(define r9
  (render
   (window
    (vpanel
     (group @group-label
            (text "inside"))))))
(define group-node (node-child (node-child (renderer-root r9) 0) 0))
(define group-legend-node (node-child group-node 0))
(define group-content-node (node-child group-node 1))
(check-equal (dom-node-tag group-node) 'group "group node tag")
(check-equal (node-attr group-node 'data-we-widget) "group" "group data-we-widget attr")
(check-equal (node-attr group-node 'class) "we-group" "group base class")
(check-equal (dom-node-tag group-legend-node) 'legend "group legend node tag")
(check-equal (dom-node-text group-legend-node) "Settings" "group initial legend text")
(check-equal (dom-node-text group-content-node) "inside" "group child render")
(:= @group-label "Advanced")
(check-equal (dom-node-text group-legend-node) "Advanced" "group legend reflects observable update")

;; if-view switches branch on observable condition
(define @show-then (@ #t))
(define r10
  (render
   (window
    (vpanel
     (if-view @show-then
              (text "then")
              (text "else"))))))
(define if-container (node-child (node-child (renderer-root r10) 0) 0))
(check-equal (node-attr if-container 'data-we-widget) "if-view" "if-view data-we-widget attr")
(check-equal (node-attr if-container 'class) "we-if-view" "if-view base class")
(check-equal (dom-node-text (node-child if-container 0)) "then" "if-view initial then branch")
(:= @show-then #f)
(check-equal (dom-node-text (node-child if-container 0)) "else" "if-view switched else branch")

;; cond-view picks first truthy clause and updates when clause observables change
(define @c1 (@ #f))
(define @c2 (@ #t))
(define r11
  (render
   (window
    (vpanel
     (cond-view (list (cons @c1 (text "one"))
                      (cons @c2 (text "two")))
                (text "none"))))))
(define cond-container (node-child (node-child (renderer-root r11) 0) 0))
(check-equal (node-attr cond-container 'data-we-widget) "cond-view" "cond-view data-we-widget attr")
(check-equal (node-attr cond-container 'class) "we-cond-view" "cond-view base class")
(check-equal (dom-node-text (node-child cond-container 0)) "two" "cond-view initial selected clause")
(:= @c1 #t)
(check-equal (dom-node-text (node-child cond-container 0)) "one" "cond-view first truthy clause wins")
(:= @c1 #f)
(:= @c2 #f)
(check-equal (dom-node-text (node-child cond-container 0)) "none" "cond-view else fallback")

;; case-view picks matching literal and updates on observable value changes
(define @mode (@ 'a))
(define r12
  (render
   (window
    (vpanel
     (case-view @mode
                (list (cons '(a) (text "mode-a"))
                      (cons '(b c) (text "mode-bc")))
                (text "mode-other"))))))
(define case-container (node-child (node-child (renderer-root r12) 0) 0))
(check-equal (node-attr case-container 'data-we-widget) "case-view" "case-view data-we-widget attr")
(check-equal (node-attr case-container 'class) "we-case-view" "case-view base class")
(check-equal (dom-node-text (node-child case-container 0)) "mode-a" "case-view initial clause")
(:= @mode 'c)
(check-equal (dom-node-text (node-child case-container 0)) "mode-bc" "case-view matching multi-literal clause")
(:= @mode 'z)
(check-equal (dom-node-text (node-child case-container 0)) "mode-other" "case-view else fallback")

;; spacer renders as an empty spacer node
(define r13
  (render
   (window
    (vpanel
     (spacer)))))
(define spacer-node (node-child (node-child (renderer-root r13) 0) 0))
(check-equal (dom-node-tag spacer-node) 'spacer "spacer node tag")
(check-equal (node-attr spacer-node 'data-we-widget) "spacer" "spacer data-we-widget attr")

;; table supports static columns and reactive row updates
(define @rows (@ '(10 20)))
(define r14
  (render
   (window
    (vpanel
     (table '(value) @rows)))))
(define table-node (node-child (node-child (renderer-root r14) 0) 0))
(check-equal (dom-node-tag table-node) 'table "table node tag")
(check-equal (node-attr table-node 'data-we-widget) "table" "table data-we-widget attr")
(check-equal (node-attr table-node 'columns) '(value) "table columns")
(define table-header-row (node-child table-node 0))
(define table-header-cell (node-child table-header-row 0))
(check-equal (node-attr table-header-row 'data-we-widget) "table-row" "table header row data-we-widget attr")
(check-equal (node-attr table-header-cell 'data-we-widget) "table-header-cell" "table header cell data-we-widget attr")
(check-equal (node-attr table-node 'class) "we-table we-density-normal" "table normal density class")
(check-equal (node-attr table-header-cell 'class) "we-table-header-cell we-density-normal we-align-left" "table header cell normal class")
(define (row-cell-texts row-node)
  (map dom-node-text (dom-node-children row-node)))
(check-equal (map row-cell-texts (dom-node-children table-node))
             '(("value") ("10") ("20"))
             "table initial rows")
(:= @rows '(30 40 50))
(check-equal (map row-cell-texts (dom-node-children table-node))
             '(("value") ("30") ("40") ("50"))
             "table reactive rows update")
(define table-data-row (node-child table-node 1))
(define table-data-cell (node-child table-data-row 0))
(check-equal (node-attr table-data-row 'data-we-widget) "table-row" "table data row data-we-widget attr")
(check-equal (node-attr table-data-cell 'data-we-widget) "table-data-cell" "table data cell data-we-widget attr")
(check-equal (node-attr table-data-cell 'class) "we-table-data-cell we-density-normal we-align-left" "table data cell normal class")

;; table supports compact density style
(define r14b
  (render
   (window
    (vpanel
     (table '(k v) '(("a" 1)) 'compact)))))
(define table-node-compact (node-child (node-child (renderer-root r14b) 0) 0))
(check-equal (node-attr table-node-compact 'density) 'compact "table compact density attr")
(check-equal (node-attr table-node-compact 'class) "we-table we-density-compact" "table compact density class")

;; table supports per-column alignment in column specs: (list label align)
(define r14c
  (render
   (window
    (vpanel
     (table '(("name" left) ("count" right) ("state" center))
            '(("alpha" 12 "ok")))))))
(define table-node-aligned (node-child (node-child (renderer-root r14c) 0) 0))
(define table-aligned-header-row (node-child table-node-aligned 0))
(define table-aligned-data-row (node-child table-node-aligned 1))
(check-equal (node-attr (node-child table-aligned-header-row 0) 'class)
             "we-table-header-cell we-density-normal we-align-left"
             "table aligned header left class")
(check-equal (node-attr (node-child table-aligned-header-row 1) 'class)
             "we-table-header-cell we-density-normal we-align-right"
             "table aligned header right class")
(check-equal (node-attr (node-child table-aligned-header-row 2) 'class)
             "we-table-header-cell we-density-normal we-align-center"
             "table aligned header center class")
(check-equal (node-attr (node-child table-aligned-data-row 1) 'class)
             "we-table-data-cell we-density-normal we-align-right"
             "table aligned data cell right class")

;; observable-view switches rendered child when data changes
(define @ov (@ "one"))
(define r15
  (render
   (window
    (vpanel
     (observable-view @ov
                      (lambda (v) (text v)))))))
(define ov-container (node-child (node-child (renderer-root r15) 0) 0))
(define ov-child0 (node-child ov-container 0))
(check-equal (node-attr ov-container 'data-we-widget) "observable-view" "observable-view data-we-widget attr")
(check-equal (node-attr ov-container 'class) "we-observable-view" "observable-view base class")
(check-equal (dom-node-text ov-child0) "one" "observable-view initial child")
(:= @ov "two")
(define ov-child1 (node-child ov-container 0))
(check-equal (dom-node-text ov-child1) "two" "observable-view updated child")
(check-equal (eq? ov-child0 ov-child1) #f "observable-view rerenders when value changes")

;; observable-view equal-proc can suppress redundant rerenders
(define @ov2 (@ "A"))
(define r16
  (render
   (window
    (vpanel
     (observable-view @ov2
                      (lambda (v) (text v))
                      (lambda (a b) (string-ci=? a b)))))))
(define ov2-container (node-child (node-child (renderer-root r16) 0) 0))
(define ov2-child0 (node-child ov2-container 0))
(:= @ov2 "a")
(define ov2-child1 (node-child ov2-container 0))
(check-equal (eq? ov2-child0 ov2-child1) #t "observable-view equal-proc suppresses rerender")

;; radios supports two-way observable binding
(define @radio (@ 'left))
(define r17
  (render
   (window
    (vpanel
     (radios '(left right center)
             @radio
             (lambda (new-value) (:= @radio new-value)))))))
(define radios-node (node-child (node-child (renderer-root r17) 0) 0))
(check-equal (dom-node-tag radios-node) 'radios "radios node tag")
(check-equal (node-attr radios-node 'selected) 'left "radios initial selected")
(check-equal (node-attr radios-node 'data-we-widget) "radios" "radios data-we-widget attr")
(check-equal (node-attr radios-node 'class) "we-radios" "radios base class")
(dom-node-radio-select! radios-node 'right)
(check-equal (obs-peek @radio) 'right "radios select updates observable")
(check-equal (node-attr radios-node 'selected) 'right "radios attr after selection")
(:= @radio 'center)
(check-equal (node-attr radios-node 'selected) 'center "observable update reflects in radios")

;; image supports static and observable source updates
(define r18
  (render
   (window
    (vpanel
     (image "a.png")))))
(define image-node1 (node-child (node-child (renderer-root r18) 0) 0))
(check-equal (dom-node-tag image-node1) 'image "image node tag")
(check-equal (node-attr image-node1 'src) "a.png" "image static src")
(check-equal (node-attr image-node1 'data-we-widget) "image" "image data-we-widget attr")
(check-equal (node-attr image-node1 'class) "we-image" "image base class")

(define @img-src (@ "x.png"))
(define r19
  (render
   (window
    (vpanel
     (image @img-src)))))
(define image-node2 (node-child (node-child (renderer-root r19) 0) 0))
(check-equal (node-attr image-node2 'src) "x.png" "image observable initial src")
(:= @img-src "y.png")
(check-equal (node-attr image-node2 'src) "y.png" "image observable updated src")

;; image supports optional width/height attributes
(define r19b
  (render
   (window
    (vpanel
     (image "size.png" 64 32)))))
(define image-node3 (node-child (node-child (renderer-root r19b) 0) 0))
(check-equal (node-attr image-node3 'src) "size.png" "image sized src")
(check-equal (node-attr image-node3 'width) 64 "image optional width attr")
(check-equal (node-attr image-node3 'height) 32 "image optional height attr")

;; dropdown renders single-trigger menu and item actions update selected id
(define @dropdown-label (@ "Actions"))
(define @dropdown-selected (@ 'none))
(define r19c
  (render
   (window
    (vpanel
     (dropdown @dropdown-label
               '((open "Open") (save "Save"))
               (lambda (new-id)
                 (:= @dropdown-selected new-id)))))))
(define dropdown-node (node-child (node-child (renderer-root r19c) 0) 0))
(define dropdown-menu-node (node-child dropdown-node 0))
(define dropdown-label-node (node-child dropdown-menu-node 0))
(define dropdown-popup-node (node-child dropdown-menu-node 1))
(define dropdown-open-item (node-child dropdown-popup-node 0))
(define dropdown-save-item (node-child dropdown-popup-node 1))
(check-equal (node-attr dropdown-node 'data-we-widget) "dropdown" "dropdown data-we-widget attr")
(check-equal (node-attr dropdown-node 'class) "we-dropdown" "dropdown base class")
(check-equal (node-attr dropdown-popup-node 'class) "we-menu-popup" "dropdown popup initial class")
(dom-node-click! dropdown-label-node)
(check-equal (node-attr dropdown-popup-node 'class) "we-menu-popup is-open" "dropdown popup open class")
(dom-node-click! dropdown-save-item)
(check-equal (obs-peek @dropdown-selected) 'save "dropdown item click updates selected id")
(check-equal (node-attr dropdown-popup-node 'class) "we-menu-popup" "dropdown popup closes after item click")
(check-equal (dom-node-text dropdown-open-item) "Open" "dropdown first item label")
(:= @dropdown-label "More")
(check-equal (dom-node-text dropdown-label-node) "More" "dropdown label observable update")

;; tooltip wraps trigger content and tracks observable message text
(define @tooltip-message (@ "Click to run"))
(define r19tooltip
  (render
   (window
    (vpanel
     (tooltip @tooltip-message
              (button "run" (lambda () (void))))))))
(define tooltip-node (node-child (node-child (renderer-root r19tooltip) 0) 0))
(define tooltip-trigger-node (node-child tooltip-node 0))
(define tooltip-child-node (node-child tooltip-trigger-node 0))
(define tooltip-bubble-node (node-child tooltip-node 1))
(check-equal (node-attr tooltip-node 'data-we-widget) "tooltip" "tooltip data-we-widget attr")
(check-equal (node-attr tooltip-node 'class) "we-tooltip" "tooltip base class")
(check-equal (node-attr tooltip-trigger-node 'data-we-widget) "tooltip-trigger" "tooltip trigger data-we-widget attr")
(check-equal (node-attr tooltip-bubble-node 'data-we-widget) "tooltip-bubble" "tooltip bubble data-we-widget attr")
(check-equal (node-attr tooltip-bubble-node 'role) 'tooltip "tooltip bubble role attr")
(check-equal (dom-node-text tooltip-bubble-node) "Click to run" "tooltip initial message")
(check-equal (node-attr tooltip-child-node 'aria-describedby)
             (node-attr tooltip-bubble-node 'id)
             "tooltip child aria-describedby references bubble id")
(:= @tooltip-message "Click to run selected plan")
(check-equal (dom-node-text tooltip-bubble-node) "Click to run selected plan" "tooltip observable message update")

;; popover toggles panel class/aria state and updates observable label
(define @popover-label (@ "Actions"))
(define r19popover
  (render
   (window
    (vpanel
     (popover @popover-label
              (text "deploy-body")
              (button "confirm" (lambda () (void))))))))
(define popover-node (node-child (node-child (renderer-root r19popover) 0) 0))
(define popover-trigger-node (node-child popover-node 0))
(define popover-backdrop-node (node-child popover-node 1))
(define popover-panel-node (node-child popover-node 2))
(check-equal (node-attr popover-node 'data-we-widget) "popover" "popover data-we-widget attr")
(check-equal (node-attr popover-node 'class) "we-popover" "popover base class")
(check-equal (node-attr popover-trigger-node 'data-we-widget) "popover-trigger" "popover trigger data-we-widget attr")
(check-equal (node-attr popover-backdrop-node 'data-we-widget) "popover-backdrop" "popover backdrop data-we-widget attr")
(check-equal (node-attr popover-panel-node 'data-we-widget) "popover-panel" "popover panel data-we-widget attr")
(check-equal (node-attr popover-backdrop-node 'class) "we-popover-backdrop" "popover initial backdrop class")
(check-equal (node-attr popover-panel-node 'class) "we-popover-panel" "popover initial panel class")
(check-equal (node-attr popover-backdrop-node 'aria-hidden) "true" "popover backdrop initially hidden")
(check-equal (node-attr popover-panel-node 'aria-hidden) "true" "popover panel initially hidden")
(dom-node-click! popover-trigger-node)
(check-equal (node-attr popover-backdrop-node 'class) "we-popover-backdrop is-open" "popover backdrop class after open")
(check-equal (node-attr popover-panel-node 'class) "we-popover-panel is-open" "popover panel class after open")
(check-equal (node-attr popover-panel-node 'aria-hidden) "false" "popover panel visible after open")
(dom-node-click! popover-backdrop-node)
(check-equal (node-attr popover-panel-node 'class) "we-popover-panel" "popover panel class after backdrop close")
(check-equal (node-attr popover-panel-node 'aria-hidden) "true" "popover panel hidden after backdrop close")
(dom-node-click! popover-trigger-node)
(dom-node-keydown! popover-panel-node "Escape")
(check-equal (node-attr popover-panel-node 'class) "we-popover-panel" "popover panel class after Escape close")
(check-equal (node-attr popover-panel-node 'aria-hidden) "true" "popover panel hidden after Escape")
(:= @popover-label "More")
(check-equal (dom-node-text popover-trigger-node) "More" "popover trigger observable label update")

;; card renders optional header/footer and body children
(define @card-title (@ "Profile"))
(define @card-footer (@ "updated now"))
(define r19d
  (render
   (window
    (vpanel
     (card @card-title
           @card-footer
           (text "body-line"))))))
(define card-node (node-child (node-child (renderer-root r19d) 0) 0))
(define card-header-node (node-child card-node 0))
(define card-body-node (node-child card-node 1))
(define card-footer-node (node-child card-node 2))
(define card-body-child (node-child card-body-node 0))
(check-equal (node-attr card-node 'data-we-widget) "card" "card data-we-widget attr")
(check-equal (node-attr card-node 'class) "we-card" "card base class")
(check-equal (node-attr card-header-node 'class) "we-card-header" "card header class")
(check-equal (node-attr card-body-node 'class) "we-card-body" "card body class")
(check-equal (node-attr card-footer-node 'class) "we-card-footer" "card footer class")
(check-equal (dom-node-text card-header-node) "Profile" "card initial header text")
(check-equal (dom-node-text card-body-child) "body-line" "card body child text")
(check-equal (dom-node-text card-footer-node) "updated now" "card initial footer text")
(:= @card-title "Account")
(:= @card-footer "saved")
(define card-header-node-after (node-child card-node 0))
(define card-footer-node-after (node-child card-node 2))
(check-equal (dom-node-text card-header-node-after) "Account" "card header observable update")
(check-equal (dom-node-text card-footer-node-after) "saved" "card footer observable update")

;; menu-bar/menu/menu-item render and menu-item action
(define @menu-label (@ "File"))
(define @item-label (@ "Open"))
(define @menu-clicks (@ 0))
(define r20
  (render
   (window
    (vpanel
     (menu-bar
      (menu @menu-label
            (menu-item @item-label
                       (lambda () (<~ @menu-clicks add1)))))))))
(define menu-bar-node (node-child (node-child (renderer-root r20) 0) 0))
(define menu-node (node-child menu-bar-node 0))
(define menu-label-node (node-child menu-node 0))
(define menu-popup-node (node-child menu-node 1))
(define menu-item-node (node-child menu-popup-node 0))
(check-equal (dom-node-tag menu-bar-node) 'menu-bar "menu-bar tag")
(check-equal (dom-node-tag menu-node) 'menu "menu tag")
(check-equal (dom-node-tag menu-label-node) 'button "menu label node tag")
(check-equal (dom-node-tag menu-popup-node) 'vpanel "menu popup node tag")
(check-equal (dom-node-tag menu-item-node) 'menu-item "menu-item tag")
(check-equal (node-attr menu-bar-node 'data-we-widget) "menu-bar" "menu-bar data-we-widget attr")
(check-equal (node-attr menu-node 'data-we-widget) "menu" "menu data-we-widget attr")
(check-equal (node-attr menu-item-node 'data-we-widget) "menu-item" "menu-item data-we-widget attr")
(check-equal (node-attr menu-bar-node 'role) 'menubar "menu-bar role attr")
(check-equal (node-attr menu-popup-node 'role) 'menu "menu popup role attr")
(check-equal (node-attr menu-label-node 'role) 'button "menu label role attr")
(check-equal (node-attr menu-label-node 'data-we-widget) "menu-label" "menu label data-we-widget attr")
(check-equal (node-attr menu-popup-node 'data-we-widget) "menu-popup" "menu popup data-we-widget attr")
(check-equal (node-attr menu-label-node 'class) "we-menu-label" "menu label base class")
(check-equal (node-attr menu-popup-node 'class) "we-menu-popup" "menu popup initial class")
(check-equal (node-attr menu-label-node 'aria-haspopup) "menu" "menu label popup attr")
(check-equal (node-attr menu-label-node 'aria-expanded) "false" "menu initial collapsed attr")
(check-equal (node-attr menu-item-node 'role) 'menuitem "menu-item role attr")
(check-equal (node-attr menu-item-node 'tabindex) 0 "menu-item tabindex attr")
(check-equal (dom-node-text menu-label-node) "File" "menu initial label")
(check-equal (dom-node-text menu-item-node) "Open" "menu-item initial label")
(dom-node-click! menu-label-node)
(check-equal (node-attr menu-label-node 'aria-expanded) "true" "menu label expands on click")
(check-equal (node-attr menu-popup-node 'class) "we-menu-popup is-open" "menu popup open class")
(dom-node-click! menu-item-node)
(check-equal (obs-peek @menu-clicks) 1 "menu-item action invoked")
(check-equal (node-attr menu-label-node 'aria-expanded) "false" "menu closes after menu-item action")
(check-equal (node-attr menu-popup-node 'class) "we-menu-popup" "menu popup class after close")
(:= @menu-label "Edit")
(:= @item-label "Copy")
(check-equal (dom-node-text menu-label-node) "Edit" "menu label observable update")
(check-equal (dom-node-text menu-item-node) "Copy" "menu-item label observable update")
(dom-node-keydown! menu-label-node "ArrowDown")
(check-equal (node-attr menu-label-node 'aria-expanded) "true" "menu opens on ArrowDown")
(check-equal (node-attr menu-popup-node 'class) "we-menu-popup is-open" "menu popup open class on ArrowDown")
(dom-node-click! menu-item-node)
(check-equal (obs-peek @menu-clicks) 2 "menu-item action still works after label updates")
(:= @menu-label "View")
(:= @item-label "Paste")
(check-equal (dom-node-text menu-label-node) "View" "menu label second observable update")
(check-equal (dom-node-text menu-item-node) "Paste" "menu-item label second observable update")
(dom-node-keydown! menu-label-node "Escape")
(check-equal (node-attr menu-label-node 'aria-expanded) "false" "menu closes on Escape")
(check-equal (node-attr menu-popup-node 'class) "we-menu-popup" "menu popup class on Escape close")
(dom-node-keydown! menu-label-node "Enter")
(check-equal (node-attr menu-label-node 'aria-expanded) "true" "menu opens on Enter")
(dom-node-click! menu-item-node)
(check-equal (obs-peek @menu-clicks) 3 "menu-item action still works after second label updates")
(dom-node-keydown! menu-label-node " ")
(check-equal (node-attr menu-label-node 'aria-expanded) "true" "menu stays open on Space toggle open")
(dom-node-keydown! menu-item-node "Enter")
(check-equal (obs-peek @menu-clicks) 4 "menu-item Enter key invokes action")
(dom-node-keydown! menu-item-node " ")
(check-equal (obs-peek @menu-clicks) 5 "menu-item Space key invokes action")
(dom-node-keydown! menu-item-node "Escape")
(check-equal (node-attr menu-label-node 'aria-expanded) "false" "menu-item Escape closes popup")
(check-equal (node-attr menu-popup-node 'class) "we-menu-popup" "menu popup class after menu-item Escape")

;; case-view handles repeated transitions among matching and fallback clauses
(:= @mode 'b)
(check-equal (dom-node-text (node-child case-container 0)) "mode-bc" "case-view transition to b")
(:= @mode 'a)
(check-equal (dom-node-text (node-child case-container 0)) "mode-a" "case-view transition back to a")
(:= @mode 'unknown)
(check-equal (dom-node-text (node-child case-container 0)) "mode-other" "case-view transition back to fallback")

;; tab-panel switches tab content based on selected observable
(define @tab (@ 'info))
(define r21
  (render
   (window
    (vpanel
     (tab-panel @tab
                (list (cons 'info (text "Info tab"))
                      (cons 'settings (text "Settings tab"))
                      (cons 'about (text "About tab"))))))))
(define tab-panel-node (node-child (node-child (renderer-root r21) 0) 0))
(define tab-buttons-node (node-child-by-role tab-panel-node 'tablist))
(define tab-content-node (node-child-by-role tab-panel-node 'tabpanel))
(define tab-button-0 (node-child tab-buttons-node 0))
(define tab-button-1 (node-child tab-buttons-node 1))
(define tab-button-2 (node-child tab-buttons-node 2))
(check-equal (dom-node-tag tab-panel-node) 'tab-panel "tab-panel node tag")
(check-equal (node-attr tab-panel-node 'selected) 'info "tab-panel initial selected tab")
(check-equal (node-attr tab-panel-node 'data-we-widget) "tab-panel" "tab-panel data-we-widget attr")
(check-equal (dom-node-tag tab-buttons-node) 'div "tab-panel header node tag")
(check-equal (length (dom-node-children tab-panel-node)) 2 "tab-panel has only tablist and tabpanel children")
(check-equal (node-attr tab-buttons-node 'data-we-widget) "tab-list" "tab-list data-we-widget attr")
(check-equal (node-attr tab-content-node 'data-we-widget) "tab-content" "tab-content data-we-widget attr")
(check-equal (length (dom-node-children tab-buttons-node)) 3 "tab-panel header button count")
(check-equal (dom-node-text (node-child tab-content-node 0)) "Info tab" "tab-panel initial child")
(check-equal (node-attr tab-button-0 'aria-selected) #t "tab-panel initial selected header attr")
(check-equal (node-attr tab-button-0 'data-we-widget) "tab-button" "tab-button data-we-widget attr")
(check-equal (node-attr tab-button-1 'aria-selected) #f "tab-panel initial unselected header attr")
(check-equal (node-attr tab-button-0 'class) "we-tab-btn is-selected" "tab-panel initial selected class")
(:= @tab 'about)
(check-equal (node-attr tab-panel-node 'selected) 'about "tab-panel selected updates")
(check-equal (dom-node-text (node-child tab-content-node 0)) "About tab" "tab-panel selected child")
;; Click tab header button to switch selected tab via observable binding.
(dom-node-click! tab-button-1)
(check-equal (node-attr tab-panel-node 'selected) 'settings "tab-panel selected updates from header click")
(check-equal (dom-node-text (node-child tab-content-node 0)) "Settings tab" "tab-panel selected child from header click")
;; Keyboard arrows/home/end switch tabs.
(dom-node-keydown! tab-button-1 "ArrowRight")
(check-equal (node-attr tab-panel-node 'selected) 'about "tab-panel selected updates from ArrowRight")
(dom-node-keydown! tab-button-2 "ArrowLeft")
(check-equal (node-attr tab-panel-node 'selected) 'settings "tab-panel selected updates from ArrowLeft")
(dom-node-keydown! tab-button-1 "End")
(check-equal (node-attr tab-panel-node 'selected) 'about "tab-panel selected updates from End")
(dom-node-keydown! tab-button-2 "Home")
(check-equal (node-attr tab-panel-node 'selected) 'info "tab-panel selected updates from Home")
(:= @tab 'missing)
(check-equal (node-attr tab-panel-node 'selected) 'missing "tab-panel selected tracks unmatched value")
(check-equal (dom-node-text (node-child tab-content-node 0)) "Info tab" "tab-panel unmatched fallback to first tab")
(check-equal (node-attr tab-button-0 'aria-selected) #f "tab-panel unmatched value clears selected header attr")
(check-equal (node-attr tab-button-0 'class) "we-tab-btn" "tab-panel unmatched value clears selected class")

;; tab-panel supports disabled tabs and keyboard navigation skips disabled entries
(define @tab-disabled (@ 'left))
(define r21b
  (render
   (window
    (vpanel
     (tab-panel @tab-disabled
                (list (list 'left  (text "Left tab") #f)
                      (list 'middle (text "Middle tab") #t)
                      (list 'right (text "Right tab") #f)))))))
(define tab-panel-disabled-node (node-child (node-child (renderer-root r21b) 0) 0))
(define tab-disabled-buttons-node (node-child-by-role tab-panel-disabled-node 'tablist))
(define tab-disabled-content-node (node-child-by-role tab-panel-disabled-node 'tabpanel))
(define tab-disabled-button-left (node-child tab-disabled-buttons-node 0))
(define tab-disabled-button-middle (node-child tab-disabled-buttons-node 1))
(define tab-disabled-button-right (node-child tab-disabled-buttons-node 2))
(check-equal (node-attr tab-disabled-button-middle 'aria-disabled) #t "tab-panel disabled tab attr")
(check-equal (node-attr tab-disabled-button-middle 'class) "we-tab-btn is-disabled" "tab-panel disabled tab class")
(check-equal (dom-node-text (node-child tab-disabled-content-node 0)) "Left tab" "tab-panel disabled initial content")
(dom-node-click! tab-disabled-button-middle)
(check-equal (node-attr tab-panel-disabled-node 'selected) 'left "tab-panel disabled click ignored")
(dom-node-keydown! tab-disabled-button-left "ArrowRight")
(check-equal (node-attr tab-panel-disabled-node 'selected) 'right "tab-panel ArrowRight skips disabled tab")
(dom-node-keydown! tab-disabled-button-right "ArrowLeft")
(check-equal (node-attr tab-panel-disabled-node 'selected) 'left "tab-panel ArrowLeft skips disabled tab")
(dom-node-keydown! tab-disabled-button-left "End")
(check-equal (node-attr tab-panel-disabled-node 'selected) 'right "tab-panel End selects last enabled tab")
(dom-node-keydown! tab-disabled-button-right "Home")
(check-equal (node-attr tab-panel-disabled-node 'selected) 'left "tab-panel Home selects first enabled tab")

;; tab-panel rejects malformed tab entry shapes
(check-exn (lambda ()
             (render
              (window
               (vpanel
                (tab-panel (@ 'a) (list 'a))))))
           "tab-panel rejects non-pair/non-list tab entry")
(check-exn (lambda ()
             (render
              (window
               (vpanel
                (tab-panel (@ 'a) (list (list 'a)))))))
           "tab-panel rejects arity-1 list entry")
(check-exn (lambda ()
             (render
              (window
               (vpanel
                (tab-panel (@ 'a) (list (list 'a (text "A") #f 'extra)))))))
           "tab-panel rejects arity-4 list entry")

;; renderer-destroy prevents nested if-view/list-view updates after teardown
(define @show-nested (@ #t))
(define @nested-items (@ '((1 . "x") (2 . "y"))))
(define r22
  (render
   (window
    (vpanel
     (if-view @show-nested
              (list-view @nested-items
                         (lambda (_key entry)
                           (text (cdr entry)))
                         car)
              (text "hidden"))))))
(define nested-if-container (node-child (node-child (renderer-root r22) 0) 0))
(define nested-list-node (node-child nested-if-container 0))
(check-equal (node-texts (dom-node-children nested-list-node))
             '("x" "y")
             "nested if-view/list-view initial content")
(renderer-destroy r22)
(:= @nested-items '((3 . "z")))
(:= @show-nested #f)
(check-equal (dom-node-tag (node-child nested-if-container 0)) 'div "nested if-view/list-view tag unchanged after destroy")
(check-equal (node-texts (dom-node-children nested-list-node))
             '("x" "y")
             "nested if-view/list-view content unchanged after destroy")

;; link renders href and download/target attrs
(define r23-link
  (render
   (window
    (vpanel
     (link "download" "/tmp/demo.css" #t "_blank")))))
(define link-node (node-child (node-child (renderer-root r23-link) 0) 0))
(check-equal (node-attr link-node 'data-we-widget) "link" "link data-we-widget attr")
(check-equal (node-attr link-node 'class) "we-link" "link class")
(check-equal (node-attr link-node 'href) "/tmp/demo.css" "link href attr")
(check-equal (node-attr link-node 'download) "download" "link download attr")
(check-equal (node-attr link-node 'target) "_blank" "link target attr")
(check-equal (dom-node-text link-node) "download" "link label text")

;; toolbar/toolbar-group/divider render expected classes and attrs
(define r24-layout
  (render
   (window
    (vpanel
     (toolbar
      (toolbar-group
       (button "a" (lambda () (void))))
      (divider 'vertical)
      (toolbar-group
       (button "b" (lambda () (void)))))))))
(define toolbar-node (node-child (node-child (renderer-root r24-layout) 0) 0))
(define toolbar-group-left (node-child toolbar-node 0))
(define divider-node (node-child toolbar-node 1))
(check-equal (node-attr toolbar-node 'data-we-widget) "toolbar" "toolbar data-we-widget attr")
(check-equal (node-attr toolbar-node 'class) "we-toolbar" "toolbar class")
(check-equal (node-attr toolbar-group-left 'data-we-widget) "toolbar-group" "toolbar-group data-we-widget attr")
(check-equal (node-attr divider-node 'data-we-widget) "divider" "divider data-we-widget attr")
(check-equal (node-attr divider-node 'aria-orientation) "vertical" "divider vertical orientation")

;; input supports direct attrs list on constructor
(define r25-input
  (render
   (window
    (vpanel
     (input "alice"
            (lambda (_v) (void))
            #f
            '((placeholder "Your name")
              (autocomplete "name")))))))
(define input-node-attrs (node-child (node-child (renderer-root r25-input) 0) 0))
(check-equal (node-attr input-node-attrs 'placeholder) "Your name" "input placeholder attr")
(check-equal (node-attr input-node-attrs 'autocomplete) "name" "input autocomplete attr")

;; choice supports (id label) entries and decodes change selection back to id.
(define @role-selected (@ 'admin))
(define @role-last (@ 'none))
(define r26-choice
  (render
   (window
    (vpanel
     (choice '((admin "Admin")
               (editor "Editor"))
             @role-selected
             (lambda (v)
               (:= @role-last v)))))))
(define choice-node-labeled (node-child (node-child (renderer-root r26-choice) 0) 0))
(check-equal (node-attr choice-node-labeled 'selected) 'admin "choice selected keeps raw id value")
(dom-node-select! choice-node-labeled "editor")
(check-equal (obs-peek @role-last) 'editor "choice decodes selected id from label mapping")

;; button and menu-item icon slots render icon children.
(define r27-icons
  (render
   (window
    (vpanel
     (button "Save" (lambda () (void)) "💾" "›")
     (menu-bar
      (menu "File"
            (menu-item "Open" (lambda () (void)) "📂" "›")))))))
(define icon-button-node (node-child (node-child (renderer-root r27-icons) 0) 0))
(define icon-menu-item-node (node-child-by-widget (node-child-by-widget (node-child (node-child (renderer-root r27-icons) 0) 1) "menu") "menu-popup"))
(define icon-menu-item-child (node-child icon-menu-item-node 0))
(check-equal (length (dom-node-children icon-button-node)) 3 "button icon slots add leading + label + trailing nodes")
(check-equal (node-attr (node-child icon-button-node 0) 'data-we-widget) "button-icon" "button leading icon widget")
(check-equal (length (dom-node-children icon-menu-item-child)) 3 "menu-item icon slots add leading + label + trailing nodes")

;; card variants attach variant classes and headerless suppresses header node.
(define r28-card
  (render
   (window
    (vpanel
     (card "T" "F" '(compact flat headerless)
           (text "body"))))))
(define card-node-variants (node-child (node-child (renderer-root r28-card) 0) 0))
(check-equal (node-attr card-node-variants 'class) "we-card we-card-compact we-card-flat" "card variants class")
(check-equal (length (dom-node-children card-node-variants)) 2 "headerless card renders body + footer only")

;; theme-token runtime API stores and returns token values.
(theme-token-set! 'we-bg "#111111")
(check-equal (theme-token-ref 'we-bg) "#111111" "theme-token set/ref single value")
(theme-token-set-many! '((we-fg . "#eeeeee")
                         (we-border . "#333333")))
(check-equal (theme-token-ref 'we-fg) "#eeeeee" "theme-token set-many fg")
(check-equal (theme-token-ref 'we-border) "#333333" "theme-token set-many border")

(displayln "web-easy tests passed")
