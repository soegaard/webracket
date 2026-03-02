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
(check-equal (dom-node-text label-node) "0" "initial text")
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
(:= @percent 65)
(check-equal (node-attr progress-node 'value) 65 "progress reflects observable update")

;; group supports labeled container semantics
(define @group-label (@ "Settings"))
(define r9
  (render
   (window
    (vpanel
     (group @group-label
            (text "inside"))))))
(define group-node (node-child (node-child (renderer-root r9) 0) 0))
(check-equal (dom-node-tag group-node) 'group "group node tag")
(check-equal (node-attr group-node 'label) "Settings" "group initial label")
(check-equal (dom-node-text (node-child group-node 0)) "inside" "group child render")
(:= @group-label "Advanced")
(check-equal (node-attr group-node 'label) "Advanced" "group label reflects observable update")

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

;; table supports static columns and reactive row updates
(define @rows (@ '(10 20)))
(define r14
  (render
   (window
    (vpanel
     (table '(value) @rows)))))
(define table-node (node-child (node-child (renderer-root r14) 0) 0))
(check-equal (dom-node-tag table-node) 'table "table node tag")
(check-equal (node-attr table-node 'columns) '(value) "table columns")
(check-equal (map dom-node-text (dom-node-children table-node))
             '("10" "20")
             "table initial rows")
(:= @rows '(30 40 50))
(check-equal (map dom-node-text (dom-node-children table-node))
             '("30" "40" "50")
             "table reactive rows update")

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
(define menu-item-node (node-child menu-node 0))
(check-equal (dom-node-tag menu-bar-node) 'menu-bar "menu-bar tag")
(check-equal (dom-node-tag menu-node) 'menu "menu tag")
(check-equal (dom-node-tag menu-item-node) 'menu-item "menu-item tag")
(check-equal (node-attr menu-node 'label) "File" "menu initial label")
(check-equal (dom-node-text menu-item-node) "Open" "menu-item initial label")
(dom-node-click! menu-item-node)
(check-equal (obs-peek @menu-clicks) 1 "menu-item action invoked")
(:= @menu-label "Edit")
(:= @item-label "Copy")
(check-equal (node-attr menu-node 'label) "Edit" "menu label observable update")
(check-equal (dom-node-text menu-item-node) "Copy" "menu-item label observable update")
(dom-node-click! menu-item-node)
(check-equal (obs-peek @menu-clicks) 2 "menu-item action still works after label updates")
(:= @menu-label "View")
(:= @item-label "Paste")
(check-equal (node-attr menu-node 'label) "View" "menu label second observable update")
(check-equal (dom-node-text menu-item-node) "Paste" "menu-item label second observable update")

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
(check-equal (dom-node-tag tab-buttons-node) 'div "tab-panel header node tag")
(check-equal (length (dom-node-children tab-buttons-node)) 3 "tab-panel header button count")
(check-equal (dom-node-text (node-child tab-content-node 0)) "Info tab" "tab-panel initial child")
(check-equal (node-attr tab-button-0 'aria-selected) #t "tab-panel initial selected header attr")
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

(displayln "web-easy tests passed")
