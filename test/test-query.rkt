;;;
;;; query wrapper smoke test
;;;

;; Minimal smoke test for `(include-lib query)`.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --browser test-query.rkt

(include-lib query)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal (format "~a: got ~s want ~s" label got want))))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(define (check-exn thunk label)
  (define raised? #f)
  (with-handlers ([exn:fail? (lambda (_e)
                               (set! raised? #t)
                               #t)])
    (thunk)
    #f)
  (unless raised?
    (error 'check-exn label)))

(define body (document-body))
(define page
  (sxml->dom
   '(div (@ (id "query-root"))
         (p (@ (id "text-host")))
         (div (@ (id "query-host"))
              (h1 (@ (id "hw")))
              (div (@ (class "card") (id "card-1"))
                   (span (@ (class "name")) "Alpha")
                   (span (@ (class "tag")) "One"))
              (div (@ (class "card") (id "card-2"))
                   (span (@ (class "name")) "Beta")
                   (span (@ (class "tag")) "Two"))
              (div (@ (id "tree"))
                   (section (@ (id "branch-a"))
                            (p (@ (class "leaf")) "Leaf A")
                            (p (@ (class "leaf")) "Leaf B"))
                   (section (@ (id "branch-b"))
                            (p (@ (class "leaf")) "Leaf C")))))))

(element-append! body page)

(define hw-elem (document-get-element-by-id "hw"))
(define card-1 (document-get-element-by-id "card-1"))
(define tree ($ "#tree"))
(define sel ($ "#hw"))
(define card-sel ($ ".card"))
(define leaf-sel ($ ".leaf"))
(define text-sel ($ "#text-host"))
(define click-count 0)

(define (handle-click _evt)
  (set! click-count (add1 click-count)))

(element-set-text-content! ($first sel) "Hello World!")
(element-set-text-content! ($first text-sel) "Text node child")
(element-set-attribute! hw-elem 'data-track "yes")
(element-set-class-name! hw-elem "card active")

(list
 (list "query include-lib"
       (let ()
         (check-equal (safe-list-ref '(a b c) 2) 'c "safe list ref")
         (check-equal (safe-vector-ref #(7 8 9) 1) 8 "safe vector ref")
         (check-true ($selection? sel) "selection wrapper")
         (check-equal ($length sel) 1 "selection length")
         (check-equal (element-id hw-elem) "hw" "selected id")
         (check-equal (element-tag-name hw-elem) "H1" "selected tag")
         (check-equal (element-text-content hw-elem) "Hello World!" "element-text-content binding")
         (check-equal (.text sel) "Hello World!" "selection text")
         (check-equal (.text text-sel) "Text node append")
         (check-equal ($attr sel "id") "hw" "selection attr")
         (check-equal (.attr sel 'data-track) "yes" "chainable selection attr")
         (check-equal ($has-attr? sel "id") #t "selection has attr")
         (check-equal (.has-attr? sel 'data-track) #t "chainable selection has attr")
         (check-equal ($attr (list->$selection '()) "id") #f "empty selection attr")
         (check-equal ($has-attr? (list->$selection '()) "id") #f "empty selection has attr")
         (check-equal ($class-list sel) (element-class-list hw-elem) "selection class list")
         (check-true ($has-class? sel "card") "selection has class")
         (check-true (.has-class? sel 'active) "chainable selection has class")
         (check-equal ($add-class! sel "featured") sel "selection add class returns selection")
         (check-equal (.add-class! sel "featured") sel "chainable add class returns selection")
         (check-equal ($remove-class! sel "active") sel "selection remove class returns selection")
         (check-equal (.remove-class! sel "featured") sel "chainable remove class returns selection")
         (check-equal ($has-class? (list->$selection '()) "card") #f "empty selection has class")
         (check-equal ($length card-sel) 3 "card selection length")
         (check-true ($selection? ($find card-sel ".name")) "find returns selection")
         (check-equal ($length ($find card-sel ".name")) 2 "find descendant count")
         (check-equal (element-text-content ($first ($find card-sel ".name"))) "Alpha" "find first descendant")
         (check-equal (element-text-content ($last ($find card-sel ".name"))) "Beta" "find last descendant")
         (check-true ($selection? (.find card-sel ".tag")) "chainable find returns selection")
         (check-equal ($length (.find card-sel ".tag")) 2 "chainable find descendant count")
         (check-equal ($length ($filter (lambda (node)
                                          (equal? (element-id node) "card-2"))
                                        card-sel))
                      1
                      "selection filter count")
         (check-equal (element-id ($first ($filter (lambda (node)
                                                     (equal? (element-id node) "card-2"))
                                                   card-sel)))
                      "card-2"
                      "selection filter first")
         (check-equal ($length (.filter card-sel
                                        (lambda (node)
                                          (equal? (element-id node) "card-2"))))
                      1
                      "chainable filter count")
         (check-equal ($on "click" handle-click sel) sel "selection on returns selection")
         (check-equal (.on sel "click" handle-click) sel "chainable on returns selection")
         (check-equal click-count 0 "click count before dispatch")
         (check-true (js-eval "document.getElementById('hw').dispatchEvent(new Event('click', { bubbles: true }))")
                     "dispatch click event")
         (check-equal click-count 1 "click count after dispatch")
         (check-equal (.off sel "click" handle-click) sel "chainable off returns selection")
         (check-equal ($off "click" handle-click sel) sel "selection off returns selection")
         (check-true (js-eval "document.getElementById('hw').dispatchEvent(new Event('click', { bubbles: true }))")
                     "dispatch click event after remove")
         (check-equal click-count 1 "click count after remove")
         (check-equal ($length ($where (lambda (node)
                                         (equal? (element-id node) "card-2"))
                                       card-sel))
                      1
                      "selection where count")
         (check-equal ($length (.where card-sel
                                       (lambda (node)
                                         (equal? (element-id node) "card-2"))))
                      1
                      "chainable where count")
         (check-equal ($length ($not (lambda (node)
                                       (equal? (element-id node) "card-2"))
                                     card-sel))
                      2
                      "selection not count")
         (check-equal (element-id ($first ($not (lambda (node)
                                                  (equal? (element-id node) "card-2"))
                                                card-sel)))
                      "card-1"
                      "selection not first")
         (check-equal ($length (.not card-sel
                                     (lambda (node)
                                       (equal? (element-id node) "card-2"))))
                      2
                      "chainable not count")
         (check-equal ($chain ($ ".card")
                              .filter
                              (lambda (node)
                                (equal? (element-id node) "card-2"))
                              .map
                              element-id)
                      #("card-2")
                      "$chain filter map")
         (check-equal ($chain ($ ".card")
                              .where
                              (lambda (node)
                                (equal? (element-id node) "card-2"))
                              .map
                              element-id)
                      #("card-2")
                      "$chain where map")
         (check-equal ($chain ($ ".card")
                              .not
                              (lambda (node)
                                (equal? (element-id node) "card-2"))
                              .map
                              element-id)
                      #("card-1" "card-3")
                      "$chain not map")
         (check-equal (element-id card-1) "card-1" "first card id")
         (check-equal ($length ($children tree)) 2 "tree children length")
         (check-equal (element-id ($first ($children tree))) "branch-a" "tree first child")
         (check-equal (element-id ($last ($children tree))) "branch-b" "tree last child")
         (check-equal ($length ($parent leaf-sel)) 3 "leaf parent length")
         (check-equal (element-id ($first ($parent leaf-sel))) "branch-a" "leaf parent first")
         (check-equal (element-id ($last ($parent leaf-sel))) "branch-b" "leaf parent last")
         (check-equal ($length (.children tree)) 2 "chainable children length")
         (check-equal ($length (.parent leaf-sel)) 3 "chainable parent length")
         (check-equal ($length ($closest leaf-sel "section")) 3 "leaf closest length")
         (check-equal (element-id ($first ($closest leaf-sel "section"))) "branch-a" "leaf closest first")
         (check-equal (element-id ($last ($closest leaf-sel "section"))) "branch-b" "leaf closest last")
         (check-equal ($length (.closest leaf-sel "section")) 3 "chainable closest length")
         (check-equal ($length ($next leaf-sel)) 1 "leaf next length")
         (check-equal (element-text-content ($first ($next leaf-sel))) "Leaf B" "leaf next first")
         (check-equal ($length ($prev leaf-sel)) 1 "leaf prev length")
         (check-equal (element-text-content ($first ($prev leaf-sel))) "Leaf A" "leaf prev first")
         (check-equal ($length (.next leaf-sel)) 1 "chainable next length")
         (check-equal ($length (.prev leaf-sel)) 1 "chainable prev length")
         (check-equal ($length ($siblings (list->$selection (list ($first leaf-sel))))) 1 "leaf siblings length")
         (check-equal (element-text-content ($first ($siblings (list->$selection (list ($first leaf-sel)))))) "Leaf B" "leaf siblings first")
         (check-equal ($length (.siblings (list->$selection (list ($first leaf-sel))))) 1 "chainable siblings length")
         (check-equal ($length ($ "#hw")) 1 "selector length")
         (check-exn (lambda () ($ 'hw)) "selector rejects symbols")
         (check-equal ($first (list->$selection '(a b c))) 'a "first selection item")
         (check-equal (.first (list->$selection '(a b c))) 'a "chainable first selection item")
         (check-equal ($last (list->$selection '(a b c))) 'c "last selection item")
         (check-equal (.last (list->$selection '(a b c))) 'c "chainable last selection item")
         (check-equal ($vector (list->$selection '(a b c))) #(a b c) "selection vector conversion")
         (check-equal (.vector (list->$selection '(a b c))) #(a b c) "chainable vector conversion")
         (check-equal ($list (list->$selection '(a b c))) '(a b c) "selection list conversion")
         (check-equal (.list (list->$selection '(a b c))) '(a b c) "chainable list conversion")
         (check-equal ($empty? (list->$selection '())) #t "empty selection")
         (check-equal (.empty? (list->$selection '())) #t "chainable empty selection")
         (check-equal ($chain ($ "#hw") .text) "Hello World!" "$chain text")
         (check-equal ($chain ($ "#hw")
                              .for-each
                              (lambda (node) (element-set-attribute! node 'data-seen 'yes))
                              .text)
                      "Hello World!"
                      "$chain with args")
         #t)))
