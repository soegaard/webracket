;;;
;;; Documentation Pages (extracted)
;;;

;;;
;;; DOCUMENTATION PAGE
;;;

;; documentation-page : -> List
;;   Documentation page layout.
(define (documentation-page)
  `(div (@ (class "page page--docs"))
        ,(navbar)
        (section (@ (class "docs-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Compiler passes")
                           (span (@ (class "pill")) "Runtime")
                           (span (@ (class "pill")) "FFI"))
                      (h1 (@ (class "hero-title")) "Documentation")
                      (p (@ (class "hero-lead"))
                         "Reference notes for WebRacket’s compiler and runtime.")))
        ,(section-block
          "Documentation Topics"
          "Pick a topic."
          (list
           (card-grid
            (list
             (list `(h3 "WebRacket at a Glance")
                   `(p "A conceptual overview of what WebRacket is, what it targets, "
                       "and how the main pieces fit together.")
                   `(p "This is the first thing new readers should read.")
                   `(a (@ (class "doc-cta doc-cta--primary")
                          (href "documentation-webracket-glance.html"))
                       "Read overview"))
             (list `(h3 "Guide to the JavaScript FFI")
                   `(p "How WebRacket interacts with JavaScript and the browser.")
                   `(a (@ (class "doc-cta doc-cta--primary")
                          (href "documentation-js-ffi.html"))
                       "Open guide"))
             (list `(h3 "FFI Reference Docs")
                   `(p "Reference pages for the builtin bindings.")
                   `(a (@ (class "doc-cta doc-cta--primary")
                          (href "documentation-ffi-standard.html"))
                       "Browse references"))
             (list `(h3 "Compiler Overview")
                   `(p "A high-level look at the compiler pipeline, passes, and runtime layout.")
                   `(a (@ (class "doc-cta doc-cta--primary")
                          (href "documentation-compiler-overview.html"))
                       "Read overview"))))
           )
          #f
          "section--docs-topics")
        ,(section-block
          "JSXGraph Examples"
          #f
          (list
           (card-grid
            (list
             (list `(h3 "Board")
                   `(p "Set up a JSXGraph board with the WebRacket bindings.")
                   `(a (@ (class "doc-cta doc-cta--primary")
                          (href "documentation-extended-example-jsxgraph-board-points.html"))
                       "Open example"))
             (list `(h3 "Board with points and lines")
                   `(p "Board with point and lines.")
                   `(a (@ (class "doc-cta doc-cta--primary")
                          (href "documentation-extended-example-jsxgraph-geometry-constructors.html"))
                       "Open example")))))
          #f
          "section--docs-topics")
        ,(section-block
          "FFI Reference Pages"
          "API references generated from the markdown docs."
          (list
           (card-grid
            (map
             (lambda (entry)
               (define title  (cdr (assoc 'title  entry)))
               (define output (cdr (assoc 'output entry)))
               (list `(h3 ,title)
                     `(a (@ (class "doc-cta doc-cta--primary")
                            (href ,output))
                         "Open reference")))
             ffi-doc-pages)))
          #f
          "section--docs-topics")
        ,(footer-section)))

;; ffi-doc-entry : String -> (U #f (Listof Pair))
;;   Looks up generated ffi doc metadata by slug.
(define (ffi-doc-entry slug)
  (let loop ([xs ffi-doc-pages])
    (cond
      [(null? xs) #f]
      [else
       (define e (car xs))
       (if (equal? (cdr (assoc 'slug e)) slug)
           e
           (loop (cdr xs)))])))

;; ffi-doc-render-mode : Symbol
;;   'legacy -> current pre-rendered xexpr body path
;;   'structured -> structured markdown AST rendered at runtime
(define ffi-doc-render-mode 'structured)

;; sxml->text : Any -> String
;;   Flattens text content from generated SXML nodes.
(define (sxml->text node)
  (cond
    [(string? node) node]
    [(symbol? node) ""]
    [(list? node)
     (define pieces
       (for/list ([part (in-list node)])
         (sxml->text part)))
     (string-trim (string-join pieces " "))]
    [else ""]))

;; heading-id : List -> (U #f String)
;;   Extracts @id from a heading node: (h2 (@ (id "...")) ...).
(define (heading-id node)
  (and (list? node)
       (pair? node)
       (list? (cdr node))
       (pair? (cdr node))
       (let ([maybe-attrs (cadr node)])
         (and (list? maybe-attrs)
              (pair? maybe-attrs)
              (eq? (car maybe-attrs) '@)
              (let loop ([attrs (cdr maybe-attrs)])
                (cond
                  [(null? attrs) #f]
                  [else
                   (define a (car attrs))
                   (if (and (list? a)
                            (= (length a) 2)
                            (eq? (car a) 'id)
                            (string? (cadr a)))
                       (cadr a)
                       (loop (cdr attrs)))]))))))

;; ffi-doc-toc-items : (Listof Any) -> (Listof (List Integer String String))
;;   Collects h2/h3 headings from generated docs for sidebar TOC.
(define (ffi-doc-toc-items body)
  (for/list ([node (in-list body)]
             #:when (and (list? node)
                         (pair? node)
                         (memq (car node) '(h2 h3))
                         (heading-id node)))
    (define level (if (eq? (car node) 'h3) 3 2))
    (define id (heading-id node))
    (define text
      (string-trim
       (sxml->text
        (let ([tail (cddr node)])
          (if (list? tail) tail '())))))
    (list level id (if (string=? text "") id text))))

;; ffi-doc-structured-entry : String -> (U #f (Listof Pair))
;;   Looks up structured generated ffi doc metadata by slug.
(define (ffi-doc-structured-entry slug)
  (let loop ([xs ffi-doc-structured-pages])
    (cond
      [(null? xs) #f]
      [else
       (define e (car xs))
       (if (equal? (cdr (assoc 'slug e)) slug)
           e
           (loop (cdr xs)))])))

;; ffi-inline->text : Any -> String
;;   Converts structured inline nodes to plain text for heading slugs/toc text.
(define (ffi-strip-surrounding-backticks s)
  (if (and (string? s)
           (>= (string-length s) 2)
           (char=? (string-ref s 0) #\`)
           (char=? (string-ref s (sub1 (string-length s))) #\`))
      (substring s 1 (sub1 (string-length s)))
      s))

(define (ffi-inline->text node)
  (cond
    [(string? node) node]
    [(and (list? node) (pair? node))
     (case (car node)
       [(code)
        (if (and (list? (cdr node))
                 (pair? (cdr node))
                 (string? (cadr node)))
            (cadr node)
            "")]
       [(link)
        (if (and (>= (length node) 2)
                 (string? (cadr node)))
            (ffi-strip-surrounding-backticks (cadr node))
            "")]
       [(strong em)
        (string-join (map ffi-inline->text (cdr node)) " ")]
       [(br) " "]
       [else ""])]
    [else ""]))

;; ffi-render-inline : Any -> Any
;;   Renders structured inline nodes as xexpr inline nodes.
(define (ffi-render-inline node)
  (cond
    [(string? node) node]
    [(and (list? node) (pair? node))
     (case (car node)
       [(code)
        (if (and (= (length node) 2) (string? (cadr node)))
            `(code ,(cadr node))
            "")]
       [(link)
        (if (and (= (length node) 3)
                 (string? (cadr node))
                 (string? (caddr node)))
            `(a (@ (href ,(caddr node))) ,(ffi-strip-surrounding-backticks (cadr node)))
            "")]
       [(strong)
        `(strong ,@(map ffi-render-inline (cdr node)))]
       [(em)
        `(em ,@(map ffi-render-inline (cdr node)))]
       [(br) '(br)]
       [else ""])]
    [else ""]))

;; ffi-trim-hyphens : String -> String
;;   Removes leading/trailing '-' from a slug candidate.
(define (ffi-trim-hyphens s)
  (define len (string-length s))
  (define start
    (let loop ([i 0])
      (cond
        [(>= i len) len]
        [(char=? (string-ref s i) #\-) (loop (add1 i))]
        [else i])))
  (define end
    (let loop ([i (sub1 len)])
      (cond
        [(< i start) (sub1 start)]
        [(char=? (string-ref s i) #\-) (loop (sub1 i))]
        [else i])))
  (if (> start end)
      ""
      (substring s start (add1 end))))

;; ffi-slug-base : String -> String
;;   Computes base heading slug text (without duplicate suffix).
(define (ffi-slug-base text)
  (define raw (string-downcase text))
  (define no-backticks (string-replace raw "`" ""))
  (define cleaned
    (list->string
     (for/list ([ch (in-string no-backticks)]
                #:when (or (char-alphabetic? ch)
                           (char-numeric? ch)
                           (char=? ch #\space)
                           (char=? ch #\-)))
       ch)))
  (define normalized (string-replace cleaned " " "-"))
  (define trimmed (ffi-trim-hyphens normalized))
  (if (string=? trimmed "") "section" trimmed))

;; ffi-next-heading-id : String (HashTable String Integer) -> String
;;   Generates deduplicated heading IDs.
(define (ffi-next-heading-id heading-text slug-counts)
  (define base (ffi-slug-base heading-text))
  (define n (hash-ref slug-counts base 0))
  (hash-set! slug-counts base (add1 n))
  (if (= n 0) base (format "~a-~a" base (add1 n))))

;; ffi-render-structured-cell : Any -> Any
;;   Renders a structured table cell node.
(define (ffi-render-structured-cell cell)
  (if (and (list? cell) (pair? cell) (eq? (car cell) 'cell))
      `(td ,@(map ffi-render-inline (cdr cell)))
      '(td "")))

;; ffi-render-structured-header-cell : Any -> Any
;;   Renders a structured header cell node.
(define (ffi-render-structured-header-cell cell)
  (if (and (list? cell) (pair? cell) (eq? (car cell) 'cell))
      `(th ,@(map ffi-render-inline (cdr cell)))
      '(th "")))

;; ffi-structured-doc->body+toc : Any -> (Values (Listof Any) (Listof (List Integer String String)))
;;   Renders structured markdown AST blocks to xexpr body and toc items.
(define (ffi-structured-doc->body+toc ast)
  (match ast
    [`(doc ,_meta ,blocks ...)
     (define slug-counts (make-hash))
     (define toc-items-rev '())

     (define (render-block block)
       (match block
         [`(,tag ,inlines ...)
          #:when (memq tag '(h1 h2 h3 h4 h5 h6))
          (define heading-text (string-trim (string-join (map ffi-inline->text inlines) " ")))
          (define id (ffi-next-heading-id heading-text slug-counts))
          (define level
            (case tag
              [(h1) 1] [(h2) 2] [(h3) 3] [(h4) 4] [(h5) 5] [else 6]))
          (when (or (= level 2) (= level 3))
            (set! toc-items-rev
                  (cons (list level id (if (string=? heading-text "") id heading-text))
                        toc-items-rev)))
          `(,tag (@ (id ,id)) ,@(map ffi-render-inline inlines))]
         [`(p ,inlines ...)
          `(p ,@(map ffi-render-inline inlines))]
         [`(ul ,items ...)
          `(ul
            ,@(for/list ([item (in-list items)])
                (match item
                  [`(li ,inlines ...)
                   `(li ,@(map ffi-render-inline inlines))]
                  [_ '(li "")])))]
         [`(ol ,items ...)
          `(ol
            ,@(for/list ([item (in-list items)])
                (match item
                  [`(li ,inlines ...)
                   `(li ,@(map ffi-render-inline inlines))]
                  [_ '(li "")])))]
         [`(code-block (lang ,_) (text ,txt))
          `(pre (code ,(if (string? txt) txt "")))]
         [`(table (header ,header-cells ...) ,rows ...)
          `(div (@ (class "doc-table-wrap ffi-table"))
                (table (@ (class "ffi-type-table"))
                 (thead
                  (tr
                   ,@(for/list ([cell (in-list header-cells)])
                       (ffi-render-structured-header-cell cell))))
                 (tbody
                  ,@(for/list ([row (in-list rows)])
                      (match row
                        [`(row ,cells ...)
                         `(tr
                           ,@(for/list ([cell (in-list cells)])
                               (ffi-render-structured-cell cell)))]
                        [_ '(tr)])))))]
         [`(blockquote ,nested ...)
          `(blockquote
            ,@(for/list ([b (in-list nested)])
                (render-block b)))]
         [`(hr) '(hr)]
         [_ `(p "Unsupported structured node: " (code ,(format "~a" block)))]))

     (values (for/list ([b (in-list blocks)]) (render-block b))
             (reverse toc-items-rev))]
    [_ (values (list `(p "Invalid structured markdown document.")) '())]))

;; ffi-doc-content+toc : String -> (Values String (Listof Any) (Listof (List Integer String String)))
;;   Returns title, rendered body, and toc items for the selected render mode.
(define (ffi-doc-content+toc slug)
  (define legacy-entry (ffi-doc-entry slug))
  (define legacy-title (and legacy-entry (cdr (assoc 'title legacy-entry))))
  (cond
    [(eq? ffi-doc-render-mode 'structured)
     (define structured-entry (ffi-doc-structured-entry slug))
     (define structured-title
       (or (and structured-entry (cdr (assoc 'title structured-entry)))
           (ffi-doc-structured-title slug)))
     (define ast (ffi-doc-structured-ast slug))
     (if ast
         (let-values ([(body toc-items) (ffi-structured-doc->body+toc ast)])
           (values (or structured-title legacy-title "FFI Reference")
                   body
                   toc-items))
         (let ([body (or (ffi-doc-body slug)
                         (list `(p "Missing generated FFI documentation for slug: " ,slug ".")))])
           (values (or legacy-title (ffi-doc-title slug) "FFI Reference")
                   body
                   (ffi-doc-toc-items body))))]
    [else
     (define body (or (ffi-doc-body slug)
                      (list `(p "Missing generated FFI documentation for slug: " ,slug "."))))
     (values (or legacy-title (ffi-doc-title slug) "FFI Reference")
             body
             (ffi-doc-toc-items body))]))

;; doc-ffi-page : String -> List
;;   Renders a generated markdown ffi reference page.
(define (doc-ffi-page slug)
  (define-values (title body toc-items) (ffi-doc-content+toc slug))
  `(div (@ (class "page page--docs page--ffi-reference"))
        ,(navbar)
        (section (@ (class "docs-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "FFI")
                           (span (@ (class "pill")) "Reference")
                           (span (@ (class "pill")) "Generated"))
                      (p (@ (class "hero-sublead"))
                         (a (@ (href "documentation.html")) "Documentation")
                         " / " ,title)
                      (h1 (@ (class "hero-title")) ,title)
                      (p (@ (class "hero-lead"))
                         "Reference generated from repository markdown sources.")))
        ,(section-block
          title
          #f
          (list
           `(div (@ (class "docs-layout"))
                 (div (@ (class "docs-article"))
                      ,@(if (null? toc-items)
                            '()
                            (list
                             `(div (@ (class "toc-mobile") (id "doc-toc-mobile"))
                                   (details (@ (class "toc-accordion"))
                                            (summary "On this page")
                                            (div ,(doc-toc-list toc-items))))))
                      (div (@ (class "doc-content doc-prose")
                              (data-render-mode ,(symbol->string ffi-doc-render-mode)))
                           ,@body))
                 ,@(if (null? toc-items)
                       '()
                       (list
                        `(aside (@ (class "docs-toc"))
                                (div (@ (class "toc-card") (id "doc-toc-card"))
                                     (div (@ (class "toc-header"))
                                          (div (@ (class "toc-title")) "On this page")
                                          (button (@ (class "toc-toggle")
                                                     (id "doc-toc-toggle")
                                                     (type "button")
                                                     (aria-expanded "true")
                                                     (aria-controls "doc-toc"))
                                                  "Collapse"))
                                     (nav (@ (class "toc-nav") (id "doc-toc")
                                             (aria-label "Table of contents"))
                                          ,(doc-toc-list toc-items))))))))
          #f
          #f)
        ,(footer-section)))

(define (doc-ffi-standard-page) (doc-ffi-page "ffi-standard"))
(define (doc-ffi-dom-page)      (doc-ffi-page "ffi-dom"))
(define (doc-ffi-js-page)       (doc-ffi-page "ffi-js"))
(define (doc-ffi-math-page)     (doc-ffi-page "ffi-math"))
(define (doc-ffi-jsxgraph-page) (doc-ffi-page "ffi-jsxgraph"))
(define (doc-ffi-xtermjs-page)  (doc-ffi-page "ffi-xtermjs"))

;;;
;;; Extended Example: JSXGraph Board + Points
;;;

;; doc-extended-example-jsxgraph-board-points-page : -> List
;;   Documentation subpage: extended example using jsx-board + jsx-point.
(define (doc-extended-example-jsxgraph-board-points-page)
  `(div (@ (class "page page--docs page--glance"))
        ,(navbar)
        (section (@ (class "docs-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Extended Example")
                           (span (@ (class "pill")) "JSXGraph")
                           (span (@ (class "pill")) "FFI"))
                      (p (@ (class "hero-sublead"))
                         (a (@ (href "documentation.html")) "Documentation")
                         " / JSXGraph Example: Board")
                      (h1 (@ (class "hero-title")) "JSXGraph Example: Board")
                      (p (@ (class "hero-lead"))
                         "End-to-end setup of a JSXGraph board.")))
        ,(section-block
          "JSXGraph Example: Board"
          #f
          (list
           `(div (@ (class "doc-content"))
                 (p "This example assumes your program is compiled with "
                    (code "--ffi jsxgraph") ", " (code "--ffi dom") ", and "
                    (code "--ffi standard") ".")

                 (h2 "Live Board Demo")
                 (p "This page initializes a live JSXGraph board with three points.")
                 (div (@ (class "doc-demo-panel"))
                      (div (@ (id "extended-jsx-board")
                              (class "jxgbox")
                              (style "width: 640px; height: 420px; border: 1px solid rgba(255,255,255,0.22); border-radius: 8px;")))
                      (p (@ (id "extended-jsx-board-status") (class "hero-sublead"))
                         "Loading JSXGraph..."))

                 (h2 "Step 1: Create a container element")
                 (p "JSXGraph needs a DOM element with a known id. "
                    "Using " (code "sxml->dom") " from the browser stdlib keeps this concise.")
                 (pre (code ";; Requires --stdlib in browser mode for sxml->dom.\n"
                            "(define container\n"
                            "  (sxml->dom\n"
                            "   '(div (@ (id \"box\")\n"
                            "             (class \"jxgbox\")\n"
                            "             (style \"width: 640px; height: 420px;\")))))\n"
                            "\n"
                            "(js-append-child! (js-document-body) container)"))

                 (h2 "Step 2: Initialize the board")
                 (p "Build a JavaScript attributes object and initialize the board.")
                 (pre (code "(define board-attrs\n"
                            "  (js-object\n"
                            "   (vector (vector \"boundingbox\" #[-6 6 6 -6])\n"
                            "           (vector \"axis\" #t)\n"
                            "           (vector \"keepaspectratio\" #t))))\n"
                            "\n"
                            "(define board\n"
                            "  (jsx-init-board \"box\" board-attrs))"))

                 (h2 "Step 3: Create core geometry elements")
                 (p "Phase 2 adds constructor wrappers for common board elements.")
                 (pre (code "(define p-attrs (js-object (vector (vector \"name\" \"P\") (vector \"size\" 4))))\n"
                            "(define q-attrs (js-object (vector (vector \"name\" \"Q\") (vector \"size\" 4))))\n"
                            "(define text-attrs (js-object (vector)))\n"
                            "\n"
                            "(define p (jsx-board-create-point board #[-3 1] p-attrs))\n"
                            "(define q (jsx-board-create-point board #[2 2] q-attrs))\n"
                            "(define l (jsx-board-create-line board #[p q] (js-object (vector))))\n"
                            "(define s (jsx-board-create-segment board #[p q] (js-object (vector))))\n"
                            "(define c (jsx-board-create-circle board #[p q] (js-object (vector))))\n"
                            "(define t (jsx-board-create-text board #[-5 5 \"PQ\"] text-attrs))"))

                 (h2 "Step 4: Read and adjust point properties")
                 (p "Point bindings can validate and update appearance/behavior.")
                 (pre (code "(when (jsx-point? p)\n"
                            "  (jsx-set-point-face! p \"circle\")\n"
                            "  (jsx-set-point-size! p 5.0)\n"
                            "  (jsx-set-point-snap-to-grid! p 1))\n"
                            "\n"
                            "(list (jsx-point-x p)\n"
                            "      (jsx-point-y p)\n"
                            "      (jsx-point-size p))"))

                 (h2 "Step 5: Batch updates for performance")
                 (p "Suspend redraws during multiple changes, then resume and refresh.")
                 (pre (code "(jsx-board-suspend-update! board)\n"
                            "(jsx-set-point-size! q 6.0)\n"
                            "(jsx-board-unsuspend-update! board)\n"
                            "(jsx-board-update! board)"))

                 (h2 "Step 6: Remove an object")
                 (pre (code "(jsx-board-remove-object! board s)\n"
                            "(jsx-board-full-update! board)"))))
          #f
          #f)
        ,(footer-section)))

;; doc-extended-example-jsxgraph-geometry-constructors-page : -> List
;;   Documentation subpage: extended example for Phase 2 geometry constructors.
(define (doc-extended-example-jsxgraph-geometry-constructors-page)
  `(div (@ (class "page page--docs page--glance"))
        ,(navbar)
        (section (@ (class "docs-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Extended Example")
                           (span (@ (class "pill")) "JSXGraph")
                           (span (@ (class "pill")) "Geometry"))
                      (p (@ (class "hero-sublead"))
                         (a (@ (href "documentation.html")) "Documentation")
                         " / JSXGraph Example: Board with points and lines")
                      (h1 (@ (class "hero-title"))
                          "JSXGraph Example: Board with points and lines")
                      (p (@ (class "hero-lead"))
                         "Build geometry elements using the Phase 2 constructor wrappers.")))
        ,(section-block
          "JSXGraph Example: Board with points and lines"
          #f
          (list
           `(div (@ (class "doc-content"))
                 (p "This example assumes your program is compiled with "
                    (code "--ffi jsxgraph") ", " (code "--ffi dom") ", and "
                    (code "--ffi standard") ".")

                 (h2 "Live Board Demo")
                 (p "This page initializes a board and demonstrates point, line, segment, circle, intersection, and text.")
                 (div (@ (class "doc-demo-panel"))
                      (div (@ (id "extended-jsx-geometry-board")
                              (class "jxgbox")
                              (style "width: 640px; height: 420px; border: 1px solid rgba(255,255,255,0.22); border-radius: 8px;")))
                      (p (@ (id "extended-jsx-geometry-board-status") (class "hero-sublead"))
                         "Loading JSXGraph..."))

                 (h2 "Step 1: Create geometry constructors")
                 (pre (code "(define p (jsx-board-create-point board '(-4 1)\n"
                            "             (js-object (vector (vector \"name\" \"P\") (vector \"size\" 4)))))\n"
                            "(define q (jsx-board-create-point board '(2 3)\n"
                            "             (js-object (vector (vector \"name\" \"Q\") (vector \"size\" 4)))))\n"
                            "(define l (jsx-board-create-line board '((-4 1) (2 3)) (js-object (vector))))\n"
                            "(define s (jsx-board-create-segment board '((-4 1) (2 3)) (js-object (vector))))\n"
                            "(define c (jsx-board-create-circle board '((-4 1) (2 3)) (js-object (vector))))\n"
                            "(define i (jsx-board-create-point board '(0 0)\n"
                            "             (js-object (vector (vector \"name\" \"I\") (vector \"size\" 3)))))\n"
                            "(jsx-point-make-intersection! i l c 0 0)\n"
                            "(define t (jsx-board-create-text board '(-6 6 \"P-Q constructors\")\n"
                            "             (js-object (vector))))"))

                 (h2 "Step 2: Lifecycle")
                 (pre (code "(jsx-board-suspend-update! board)\n"
                            "(jsx-set-point-size! p 5.0)\n"
                            "(jsx-set-point-size! q 5.0)\n"
                            "(jsx-board-unsuspend-update! board)\n"
                            "(jsx-board-full-update! board)"))))
          #f
          #f)
        ,(footer-section)))

;;;
;;; WebRacket at a Glance
;;;

;; doc-webracket-at-a-glance-page : -> List
;;   Documentation subpage: WebRacket at a Glance
(define (doc-webracket-at-a-glance-page)
  `(div (@ (class "page page--docs page--glance"))
        ,(navbar)
        (section (@ (class "docs-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Overview")
                           (span (@ (class "pill")) "Runtime")
                           (span (@ (class "pill")) "FFI"))
                      (p (@ (class "hero-sublead"))
                         (a (@ (href "documentation.html")) "Documentation")
                         " / WebRacket at a Glance")
                      (h1 (@ (class "hero-title")) "WebRacket at a Glance")
                      (p (@ (class "hero-lead"))
                         "A high-level overview of what WebRacket is, what it targets, "
                         "and how the main pieces fit together.")))
        ,(section-block
          "WebRacket at a Glance"
          #f
          (list
           `(div (@ (class "doc-content"))
                 (p "WebRacket is a " (strong "Racket to WebAssembly compiler")
                    " designed to run practical Racket programs directly in "
                    "modern web browsers.")
                 (p "This page is a short, high-level map. It sets expectations, "
                    "explains the execution model, and points to deeper documentation.")
                 ,(callout
                   'note
                   "Who is this page for?"
                   '(p "This page is the recommended entry point for understanding WebRacket. "
                       "It is intended for readers who are new to the system and want a high-level "
                       "orientation before exploring the rest of the documentation. "))

                 (h2 "What WebRacket is")
                 (p "WebRacket compiles a substantial, practical subset of Racket into WebAssembly. "
                    "WebAssembly is a portable, low-level code format designed for safe and efficient "
                    "execution in modern web browsers. You can think of WebAssembly as a kind of "
                    "portable machine code for the browser.")
                 (p "WebAssembly code runs in a virtual machine that is separate from the JavaScript "
                    "virtual machine. This separation means that a foreign-function interface (FFI) is "
                    "required whenever WebAssembly code needs to call JavaScript in order to access "
                    "browser-provided functionality such as the DOM, timers, or graphics APIs.")
                 (p "In practice, this means that WebRacket:"
                    (ul
                     (li "Targets modern browsers (Chrome, Firefox, Safari).")
                     (li "Uses a custom runtime designed for WebAssembly.")
                     (li "Keeps the boundary to JavaScript explicit via the FFI.")))

                 (h2 "What WebRacket is not")
                 (p "WebRacket is not a drop-in replacement for Racket-on-VM. "
                    "Some features are intentionally missing or behave differently "
                    "due to browser and WebAssembly constraints.")
                 ,(callout
                   'note
                   "Compatibility"
                   `(p "WebRacket aims for a useful and predictable subset of Racket, rather than complete "
                       "compatibility. Differences from Racket-on-VM are currently intentional and reflect "
                       "explicit design choices, but the goal is to reduce these differences over time."))

                 (h2 "Execution model")
                 (p "WebRacket programs execute as single-threaded WebAssembly modules embedded in a "
                    "browser environment. This model determines how computation, memory management, "
                    "and interaction with the host are structured.")
                 (ul
                  (li (strong "Single-threaded execution") ": no parallel threads or shared-memory "
                      "concurrency.")
                  (li (strong "Custom runtime") ": implements core Racket value representations and "
                      "primitive operations.")
                  (li (strong "WebAssembly GC types") ": uses Wasm GC for heap-allocated values where "
                      "available."))
                 

                 (h2 "Language coverage")
                 (p "WebRacket focuses on practical web programs. Support includes many core forms "
                    "and data structures, with some areas still in progress. WebRacket currently "
                    "implements over 800 primitive procedures; see the "
                    (a (@ (href "implementation-status.html")) "status page")
                    " for a detailed and up-to-date overview.")

                 (h3 "Supported areas")
                 (ul
                  (li "Fixnums and flonums.")
                  (li "Strings and byte strings.")
                  (li "Symbols.")
                  (li "Keywords (as values and syntax).")
                  (li "Pairs, vectors, boxes, and mutable hash tables.")
                  (li "Standard expander, including " (code "for") " and " (code "match") ".")
                  (li "Tail calls, multiple values, and upward exceptions.")
                  (li "Structures with super-structures and properties.")
                  (li "Applicable structures.")
                  (li "Large parts of " (code "racket/base") "."))

                 (h3 "Current limitations")
                 (ul
                  (li "Single-threaded execution only.")
                  (li "Modules are a work in progress (use " (code "include") " for now).")
                  (li "Procedures do not yet support keyword arguments.")
                  (li "Some numeric and control features are not yet implemented."))
                 ,(callout
                   'info
                   "Where to look next"
                   `(p "For a detailed list, see the Language Coverage section on "
                       "the front page and the dedicated reference pages in "
                       (a (@ (href "documentation.html")) "Documentation") "."))

                 (h2 "JavaScript FFI")
                 (p "WebRacket integrates with the browser through an explicit "
                    "JavaScript FFI. This is how WebRacket programs use the DOM, "
                    "Canvas, and libraries like MathJax, Xterm.js, and JSXGraph.")
                 (p "The FFI is designed to make conversions and effects explicit, "
                    "so boundary behavior stays predictable and debuggable.")
                 (div (@ (class "doc-cta-card"))
                      ,(callout
                        'info
                        "FFI docs"
                        `(p "Start with the FFI guide, then use the generated "
                            "references when you need exact APIs.")
                        `(div (@ (class "doc-cta-group"))
                              (a (@ (class "doc-cta doc-cta--primary")
                                    (href "documentation-js-ffi.html"))
                                 "Open FFI guide")
                              (a (@ (class "doc-cta")
                                    (href "documentation.html"))
                                 "Browse FFI references"))))

                 (h2 "Very Brief Compiler Overview")
                 (p "WebRacket uses a direct-style compiler that translates expanded Racket programs "
                    "into WebAssembly through a sequence of small, focused transformations. This "
                    "structure keeps the compiler pipeline easy to follow and makes it "
                    "straightforward to relate generated code back to the original source program.")
                 (p "For a pass-by-pass description of the compiler pipeline and pointers into the "
                    "implementation, see the "
                    (a (@ (href "documentation-compiler-overview.html")) "Compiler Overview")
                    ".")


                 (h2 "Toolchain essentials")
                 (p "A small set of tools powers the workflow from source to the "
                    "browser:")
                 (ul
                  (li (code "wasm-tools") " to validate and inspect Wasm output.")
                  (li "Node.js to run compiled programs in the terminal.")
                  (li (code "raco static-web") " to serve artifacts locally."))

                 (h2 "What to read next")
                 (div (@ (class "doc-grid doc-grid--3"))
                      (div (@ (class "doc-card"))
                           (h3 "Using WebRacket")
                           (ul
                            (li (a (@ (href "quick-start.html")) "Quick Start"))
                            (li (a (@ (href "examples.html")) "Examples"))))
                      (div (@ (class "doc-card"))
                           (h3 "Understanding WebRacket")
                           (ul
                            (li (a (@ (href "documentation-compiler-overview.html"))
                                   "Compiler Overview"))
                            (li (a (@ (href "documentation-js-ffi.html"))
                                   "Guide to the JavaScript FFI")))))

                 #;,(callout
                     'note
                     "Design goal"
                     `(p "A recurring goal is to avoid relying on host-provided "
                         "features when possible, while still integrating cleanly "
                         "with the browser through the JavaScript boundary."))))
          #f
          #f)
        ,(footer-section)))
                      


;;;
;;; Compiler Overview
;;;

;; doc-compiler-overview-page : -> List
;;   Documentation subpage: Compiler Overview.
(define (doc-compiler-overview-page)
  `(div (@ (class "page page--docs"))
        ,(navbar)
        (section (@ (class "docs-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Compiler")
                           (span (@ (class "pill")) "Passes")
                           (span (@ (class "pill")) "Wasm"))
                      (p (@ (class "hero-sublead"))
                         (a (@ (href "documentation.html")) "Documentation")
                         " / Compiler Overview")
                      (h1 (@ (class "hero-title")) "Compiler Overview")
                      (p (@ (class "hero-lead"))
                         "A guide to the WebRacket compiler pipeline.")))
        ,(section-block
          "Introduction"
          #f
          (list
           `(div (@ (class "doc-content"))
                 (p "Compilers are complex programs. It is helpful to look at a compiler as a pipeline. "
                    "We will (mis)use the classic terminology of front end, middle end and backend.")
                 ,(callout
                   'note
                   "Etymology"
                   `(p "The division of a compiler in the "
                       ,(ext "https://en.wikipedia.org/wiki/Compiler#Three-stage_compiler_structure"
                             "three stages")
                       " front end, middle end and back end dates back to the late 70s and early 80s."))
                 (p "We will interpret \"front end\" and \"back end\" as the parts that others wrote "
                    "and \"middle end\" as the part implemented for WebRacket specifically.")
                 (p "In the following sections we will look at the components of this pipeline.")))
          #f
          #f)
        ,(section-block
          "Compiler Pipeline"
          `(span
            "WebRacket is a direct-style compiler (no "
            (a (@ (href "https://en.wikipedia.org/wiki/Continuation-passing_style")
                  (target "_blank")
                  (rel "noreferrer noopener"))
               "CPS")
            " pass) that outputs "
            (a (@ (href "https://webassembly.org/")
                  (target "_blank")
                  (rel "noreferrer noopener"))
               "WebAssembly")
            ". The "
            (a (@ (href "https://nanopass.org/")
                  (target "_blank")
                  (rel "noreferrer noopener"))
               "NanoPass")
            " compiler framework is used for the internal passes.")
          (list
           (card-grid
            (list
             (list `(div (@ (class "pipeline-header"))
                         ,(step-badge)
                         (h3 (@ (class "pipeline-title")) "Frontend"))
                   `(p "Racket syntax is expanded, then normalized into a "
                       (a (@ (href "https://docs.racket-lang.org/reference/syntax-model.html#%28part._fully-expanded%29")
                             (target "_blank")
                             (rel "noreferrer noopener"))
                          "compiler-friendly core")
                       "."))
             (list `(div (@ (class "pipeline-header"))
                         ,(step-badge)
                         (h3 (@ (class "pipeline-title")) "Middle End"))
                   `(p "Nanopass passes like "
                       (a (@ (href "https://matt.might.net/articles/closure-conversion/")
                             (target "_blank")
                             (rel "noreferrer noopener"))
                          "closure conversion")
                       " and "
                       (a (@ (href "https://matt.might.net/articles/a-normalization/")
                             (target "_blank")
                             (rel "noreferrer noopener"))
                          "ANF")
                       " make environments and intermediate values explicit."))
             (list `(div (@ (class "pipeline-header"))
                         ,(step-badge)
                         (h3 (@ (class "pipeline-title")) "Backend"))
                   `(p (a (@ (href "https://scholarworks.iu.edu/dspace/handle/2022/34138")
                            (target "_blank")
                            (rel "noreferrer noopener"))
                          "Destination-driven code generation")
                       " emits WebAssembly."))
             (list `(div (@ (class "pipeline-header"))
                         ,(step-badge)
                         (h3 (@ (class "pipeline-title")) "Runtime"))
                   `(p "The runtime is written to be as independent from JavaScript as possible.")))
            "pipeline-grid"))
          #f
          "section--pipeline")
        ,(section-block
          "Direct style vs continuation passing style"
          #f
          (list
           `(div (@ (class "doc-content"))
                 (p "Compilation can be seen as a series of program rewrites. Each rewrite removes complexity until eventually the program has become simple enough that the backend can handle it.")
                 (p "There has been a long discussion in the compiler community about whether a compiler should rewrite the program into the so-called continuation-passing style. This style makes continuations explicit.")
                 (p "In a direct-style program continuations are implicit. In simple programs there is often an obvious connection between the original program and the structure of the resulting WebAssembly program.")
                 (p "The WebRacket compiler is so far a direct-style compiler. Relating obscure WebAssembly errors to the problematic location in the original, erroneous program has worked reasonably well so far.")
                 (p "However, I am not sure WebRacket will stay a direct-style compiler. At some point continuations and continuation marks need to be added. It might be possible to use WebAssembly support to implement continuations and keep the compiler direct-style, but I think continuation marks require a CPS transformation. Time will tell.")
                 (p (@ (class "doc-spacer-top"))
                    "Finally, the external tool "
                    (a (@ (class "code-link")
                          (href "https://github.com/bytecodealliance/wasm-tools")
                          (target "_blank")
                          (rel "noreferrer noopener"))
                       (code "wasm-tools"))
                    " "
                    (a (@ (class "code-link")
                          (href "https://bytecodealliance.github.io/wasm-tools/parse")
                          (target "_blank")
                          (rel "noreferrer noopener"))
                       (code "parse"))
                    " is called to get the wasm bytecode.")
                 (h3 "References")
                 (ul
                  (li "Flanagan, Sabry, Duba, and Felleisen (1993, 2004), "
                      "\"The Essence of Compiling with Continuations\". "
                      ,(ext "https://doi.org/10.1145/173262.155113" "DOI")
                      ", "
                      ,(ext "https://dl.acm.org/doi/pdf/10.1145/173262.155113" "PDF")
                      ", "
                      ,(ext "https://dl.acm.org/doi/pdf/10.1145/989393.989443" "PDF"))
                  (li "Kennedy (2007), "
                      "\"Compiling with Continuations, Continued\". "
                      ,(ext "https://doi.org/10.1145/1291151.1291179" "DOI")
                      ", "
                      ,(ext "https://www.microsoft.com/en-us/research/wp-content/uploads/2007/10/compilingwithcontinuationscontinued.pdf" "PDF"))
                  (li "Danvy and Filinski (1992), "
                      "\"Representing Control: a Study of the CPS Transformation\". "
                      ,(ext "https://doi.org/10.1017/S0960129500001535" "DOI")
                      ", "
                      ,(ext "http://www.cs.tufts.edu/~nr/cs257/archive/olivier-danvy/danvy92representing.ps.gz" "PS"))
                  (li "Appel (1991), "
                      "\"Compiling with Continuations\". "
                      ,(ext "https://doi.org/10.1017/CBO9780511609619" "DOI"))
                  (li "Steele (1978), "
                      "\"RABBIT: A Compiler for Scheme\". "
                      ,(ext "https://dl.acm.org/doi/abs/10.5555/889478" "DOI")
                      ", "
                      ,(ext "https://dl.acm.org/doi/suppl/10.5555/889478/suppl_file/mit___artificial_intelligence_laboratory_aitr-474.ps" "PS")))))
          #f
          #f)
        ,(section-block
          "Frontend"
          #f
          (list
           `(div (@ (class "doc-content"))
                 (p "The frontend of WebRacket reads in a source file and passes it on to the normal Racket expander. "
                    "During expansion the compile-time portions of the program are evaluated.")
                 (p "The result is a large top-level form that follows the grammar for "
                    ,(ext "https://docs.racket-lang.org/reference/syntax-model.html#%28part._fully-expanded%29"
                          "fully expanded programs")
                    ". Think of fully expanded programs as programs written in a small, compiler-friendly subset of Racket.")
                 (p "The WebRacket compiler converts the fully expanded program (which is a large syntax object) "
                    "into AST records that NanoPass passes expect. To be precise, by the normal Racket expander, I mean "
                    ,(code-link "https://docs.racket-lang.org/syntax/toplevel.html#%28def._%28%28lib._syntax%2Ftoplevel..rkt%29._expand-syntax-top-level-with-compile-time-evals%29%29"
                                "expand-top-level-with-compile-time-evals")
                    " and not "
                    ,(code-link "https://docs.racket-lang.org/reference/Expanding_Top-Level_Forms.html#%28def._%28%28quote._~23~25kernel%29._expand-syntax%29%29:~:text=(-,expand%2Dsyntax,-stx%C2%A0%5B"
                                "expand-syntax")
                    ". Note that WebRacket only compiles the runtime part of the expanded program.")
                 (h3 "References")
                 (ul
                  (li "Flatt (2016), "
                      "\"Binding as Sets of Scopes\". "
                      ,(ext "https://doi.org/10.1145/2914770.2837620" "DOI")
                      ", "
                      ,(ext "https://dl.acm.org/doi/pdf/10.1145/2914770.2837620" "PDF")
                      ", "
                      ,(ext "https://users.cs.utah.edu/plt/scope-sets/" "Web")))))
          #f
          #f)
        ,(section-block
          "Middle End"
          #f
          (list
           `(div (@ (class "doc-content"))
                 (p "The middle end of the compiler consists of several passes implemented using the "
                    ,(code-link "https://github.com/nanopass/nanopass-framework-racket" "NanoPass")
                    " framework. For completeness the list below includes frontend and backend passes.")
                 (div (@ (class "pass-list-block"))
                      (div (@ (class "pass-list-label pass-list-label--quiet")) "Compiler passes")
                      (pre (@ (class "pass-list"))
                           (code "FRONTEND\n  unexpand\n  parse\nMIDDLE END\n  flatten-topbegin\n  infer-names\n  convert-quotations\n  explicit-begin\n  explicit-case-lambda\n  α-rename\n  assignment-conversion\n  categorize-applications\n  anormalize\n  closure-conversion\n  flatten-begin\nBACKEND\n  (classify-variables)\n  generate-code")))
                 (p "The first pass of the middle end is " (code "flatten-topbegin")
                    ". This pass rewrites nested top-level " (code "begin")
                    " expressions into a single top-level " (code "begin") ".")
                 (p "The pass " (code "infer-names")
                    " annotates lambda expressions with names. For example, "
                    (code "(define fact (lambda (n) ...))")
                    " will annotate the resulting procedure with the name "
                    (code "fact") ". The names are used in error messages.")
                 (p "The pass " (code "convert-quotations")
                    " removes " (code "quote")
                    " from the program. After this pass only literals are allowed as subexpressions of "
                    (code "quote") ".")
                 (p "Bodies of " (code "lambda") ", " (code "let-values")
                    ", and " (code "letrec-values")
                    " can be sequences of expressions. There is an implicit "
                    (code "begin")
                    " around the body expressions. After the "
                    (code "explicit-begin")
                    " pass, these forms have only a single expression in their bodies.")
                 (p "The pass " (code "explicit-case-lambda")
                    " introduces a " (code "lambda")
                    " expression for each clause. Later, " (code "case-lambda")
                    " is compiled as a case statement that determines which closure to call.")
                 (p "The pass " (code "α-rename")
                    " renames identifiers. After this pass, two identifiers with the same name have the same binding. "
                    "The pass is written not to rename primitives, so after the pass everything that looks like a primitive is a primitive.")
                 (p "The pass " (code "assignment-conversion")
                    " identifies assignable variables. If a variable is potentially assigned to, the variable is bound to a box. "
                    "Assignments are then turned into box mutations. The pass simplifies the representation of closures.")
                 (p "The pass " (code "categorize-applications")
                    " annotates each application as an application of a primitive, an abstraction, or a general procedure. "
                    "Closed applications (direct calls of a lambda expression) are rewritten into "
                    (code "let")
                    " expressions.")
                 (p "The pass " (code "anormalize")
                    " (short for A-normalization) introduces an evaluation order for complex expressions. "
                    "At the same time, intermediate results are named.")
                 (p "The pass " (code "closure-conversion")
                    " performs closure conversion. For each abstraction (lambda expression), closure conversion chooses a concrete runtime representation. "
                    "WebRacket uses flat closures, which means closures are represented as a code label and an array of the values of the free variables at closure-creation time. "
                    "Due to boxing in the " (code "assignment-conversion")
                    " pass, this representation also works with assignable variables.")
                 (p "The final pass of the middle end is " (code "flatten-begin")
                    ", which removes nested " (code "begin")
                    " expressions. This simplifies the code generator a little.")
                 (p "See the comments in " ,(code-link (gh-file "compiler.rkt") "compiler.rkt")
                    " to see the gory details.")
                 (h3 "References")
                 (ul
                  (li "Flanagan, Sabry, Duba, and Felleisen (1993), "
                      "\"The Essence of Compiling with Continuations\" (introduces A-normal form / ANF). "
                      ,(ext "https://doi.org/10.1145/155090.155113" "DOI")
                      ", "
                      ,(ext "https://dl.acm.org/doi/pdf/10.1145/155090.155113" "PDF")))
                 ,(callout
                   'note
                   "Why so many passes?"
                   `(p "Small, focused passes keep each transformation understandable and testable."))))
          #f
          #f)
        ,(section-block
          "Backend"
          #f
          (list
           `(div (@ (class "doc-content"))
                 (p (@ (class "doc-spacer-top"))
                    "The backend, also known as the code generator, is responsible for choosing concrete representations "
                    "for both data and variables in the WebAssembly runtime.")
                 (p "WebAssembly supports a simple form of structs (arrays with named fields). These are used to represent "
                    "most Racket data types. The concrete choices of WebAssembly data structures were inspired by traditional "
                    "compilers such as Larceny and Chez Scheme, and also by the WebAssembly projects Guile Hoot and Wasocaml.")
                 (p "Modern compilers often use register allocators to map variables to machine registers. WebRacket instead "
                    "uses the approach in \"Destination-Driven Code Generation\": generate code that matches the context in "
                    "which the result value is needed. This reduces many unnecessary register shuffles.")
                 (p "The output of the code generator is WebAssembly. WebAssembly tools accept WebAssembly in two formats. "
                    "We output the folded textual format. In the folded format, we can use nested expressions - so if you squint "
                    "the WebAssembly looks like low-level Scheme.")
                 (p (@ (class "doc-spacer-top"))
                    "Finally, the external tool "
                    (a (@ (class "code-link")
                          (href "https://github.com/bytecodealliance/wasm-tools")
                          (target "_blank")
                          (rel "noreferrer noopener"))
                       (code "wasm-tools"))
                    " "
                    (a (@ (class "code-link")
                          (href "https://bytecodealliance.github.io/wasm-tools/parse")
                          (target "_blank")
                          (rel "noreferrer noopener"))
                       (code "parse"))
                    " is called to get the wasm bytecode.")
                 (h3 "References")
                 (ul
                  (li "Dybvig, Hieb, and Butler (1990), \"Destination-Driven Code Generation\". "
                      ,(ext "https://hdl.handle.net/2022/34138" "DOI")
                      ", "
                      ,(ext "https://scholarworks.iu.edu/dspace/bitstreams/bd620c2a-3f98-41ef-9617-21f2998ce228/download" "PDF"))
                  (li "Léo Andrès (2024), \"Exécution symbolique pour tous ou Compilation d'OCaml vers WebAssembly\". DOI unavailable. "
                      ,(ext "https://theses.fr/2024UBFCK015" "Record")
                      ", "
                      ,(ext "https://theses.hal.science/tel-04995610/document" "PDF"))
                  (li "\"Hoot: Scheme on WebAssembly\". "
                      ,(ext "https://spritely.institute/hoot/" "Web")))
                 (h3 "Source")
                 (p "The main part of the compiler is in "
                    ,(code-link (gh-file "compiler.rkt") "compiler.rkt")
                    ". The WebAssembly runtime is in "
                    ,(code-link (gh-file "runtime-wasm.rkt") "runtime-wasm.rkt")
                    ". The standard library (implemented in WebRacket) is found in "
                    ,(code-link (gh-dir "stdlib/") "stdlib/")
                    ". FFI bindings for popular libraries are in "
                    ,(code-link (gh-dir "ffi/") "ffi/") ".")
                 (p "It has been a design goal to avoid relying on functionality provided by the "
                    "WebAssembly host if possible. Who knows - maybe someone needs a non-JavaScript host "
                    "at some point? For browser functionality there is no way around interfacing with "
                    "the JavaScript host. The JavaScript part of the runtime support is in "
                    ,(code-link (gh-file "assembler.rkt") "assembler.rkt") ".")
                 ,(callout
                   'note
                   "Runtime goal"
                   `(p "The runtime deliberately minimizes host dependencies so WebRacket can target "
                       "non-JavaScript environments when they become viable."))))
          #f
          #f)
        ,(footer-section)))

;;;
;;; Guide to the JavaScript FFI
;;;


;; doc-js-ffi-page : -> List
;;   Documentation subpage: Guide to the JavaScript FFI.

;; Aside:
;;   A slug is the unique, user-friendly, and URL-friendly, identifier
;;   at the end of a web address that describes a specific page.
(define (doc-js-ffi-page)
  (define toc-items '())
  (define slug-counts (make-hash))
  ;; doc-slugify : String -> String
  ;;   Generates a stable, URL-safe slug for headings.
  (define (doc-slugify text)
    (define raw (string-downcase text))
    (define parts
      (let loop ([i 0] [current '()] [acc '()])
        (cond
          [(= i (string-length raw))
           (define part (list->string (reverse current)))
           (define next-acc (if (string=? part "") acc (cons part acc)))
           (reverse next-acc)]
          [else
           (define ch (string-ref raw i))
           (define ok?
             (or (and (char>=? ch #\a) (char<=? ch #\z))
                 (and (char>=? ch #\0) (char<=? ch #\9))))
           (if ok?
               (loop (add1 i) (cons ch current) acc)
               (let ([part (list->string (reverse current))])
                 (loop (add1 i) '() (if (string=? part "") acc (cons part acc)))))])))
    (define hyphenated
      (if (null? parts) "" (string-join parts "-")))
    (define trimmed (trim-hyphens hyphenated))
    (define base (if (string=? trimmed "") "section" trimmed))
    (define n (hash-ref slug-counts base 0))
    (hash-set! slug-counts base (add1 n))
    (if (= n 0)
        base
        (format "~a-~a" base (add1 n))))
  ;; doc-heading : Integer String -> List
  ;;   Creates an h2/h3 heading with a slug id and records it for the TOC.
  (define (doc-heading level text)
    (define tag (if (= level 2) 'h2 'h3))
    (define id (doc-slugify text))
    (set! toc-items (cons (list level id text) toc-items))
    `(,tag (@ (id ,id)) ,text))
  (define doc-body
    `(section (@ (class "section section--doc-body"))
              (div (@ (class "doc-content doc-prose"))
                   ,(doc-heading 2 "0. A quick example: calling Math.sin")
                     (p "Start with a tiny, complete example. We fetch the JavaScript " (code "Math")
                        " object as an extern, then call " (code "Math.sin") " with 0.0.")
                     (pre (code "(define math (js-ref/extern (js-global-this) \"Math\"))\n(js-send/flonum math \"sin\" (vector 0.0))"))
                     (p "Step 1: " (code "js-global-this") " returns the global JavaScript object."
                        " Then " (code "js-ref/extern") " reads the " (code "Math") " property and keeps it"
                        " as an extern (a JavaScript object wrapper).")
                     (p "Step 2: " (code "js-send/flonum") " invokes the " (code "sin") " method on that extern"
                        " and returns a WebRacket flonum. The argument list is passed as a vector.")
                     ,(doc-heading 2 "1. Purpose and scope")
                     (p "The JavaScript FFI is the bridge between WebRacket and the host JavaScript runtime. "
                        "It lets you call JavaScript APIs from WebRacket, access browser objects (DOM, Canvas, "
                        "Audio, etc.), and pass data back and forth. The FFI is intentionally thin: it mirrors "
                        "the host APIs with minimal translation so that MDN and library docs stay relevant.")
                     (p "The FFI focuses on browser and JavaScript interop, not on replacing Racket’s full "
                        "foreign-interface system. Expect simple value conversions, explicit extern objects, "
                        "and predictable mappings rather than automatic lifting of complex data structures.")
                     ,(doc-heading 2 "2. How bindings are declared")
                     (p "Bindings live in the files under " (code "ffi/") " and are defined with "
                        (code "define-foreign") ". Each binding names a Racket-level procedure, the JavaScript "
                        "module it lives in, the host name, and a compact type signature.")
                     (p "Example (simplified):")
                     (pre (code "(define-foreign js-ref
  #:module "standard"
  #:name   "ref/value"
  (-> (extern string) (value)))"))
                     (p "At compile time, WebRacket reads these " (code ".ffi") " files, validates them, and "
                        "generates the WebAssembly imports that call into JavaScript.")
                     ,(doc-heading 2 "3. FFI type language (argument + result types)")
                     (p "The type language is intentionally small. Here are the core types and what they mean:")
                     (div (@ (class "ffi-table"))
                          (table (@ (class "ffi-type-table") (aria-label "FFI argument types"))
                                 (caption "FFI argument types")
                                 (thead
                                  (tr
                                   (th (@ (scope "col")) "Type")
                                   (th (@ (scope "col")) "Meaning / mapping")
                                   (th (@ (scope "col")) "Notes")))
                                 (tbody
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "string"))
                                   (td "A WebRacket string marshaled to JS.")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "string/symbol"))
                                   (td "Accepts either a string or symbol; marshaled as a string.")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "value"))
                                   (td "Any WebRacket value; marshaled through the FASL encoder.")
                                   (td (@ (class "ffi-note-cell")) "Marshaled via FASL."))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "extern"))
                                   (td "An external JavaScript object (externref wrapper).")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "extern/raw"))
                                   (td "Alias for extern in argument position.")
                                   (td (@ (class "ffi-note-cell")) "Optional explicit spelling."))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "i32"))
                                   (td "Fixnum to signed 32-bit integer.")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "u32"))
                                   (td "Fixnum to unsigned 32-bit integer.")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "f64"))
                                   (td "Flonum to JavaScript number.")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "boolean"))
                                   (td "#f/#t to 0/1.")
                                   (td (@ (class "ffi-note-cell is-empty")) "—")))))
                     (div (@ (class "ffi-table"))
                          (table (@ (class "ffi-type-table") (aria-label "FFI return types"))
                                 (caption "FFI return types")
                                 (thead
                                  (tr
                                   (th (@ (scope "col")) "Type")
                                   (th (@ (scope "col")) "Meaning / mapping")
                                   (th (@ (scope "col")) "Notes")))
                                 (tbody
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "string"))
                                   (td "A JavaScript string decoded to a WebRacket string.")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "value"))
                                   (td "Any JavaScript value decoded through the FASL bridge.")
                                   (td (@ (class "ffi-note-cell")) "Marshaled via FASL."))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "extern"))
                                   (td "JS null maps to #f; other results are wrapped as external values.")
                                   (td (@ (class "ffi-note-cell")) "Default extern return behavior."))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "extern/raw"))
                                   (td "An externref wrapped as a WebRacket external value.")
                                   (td (@ (class "ffi-note-cell")) "Legacy extern behavior."))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "extern/null"))
                                   (td "Alias for extern: JS null maps to #f; other results are wrapped as external values.")
                                   (td (@ (class "ffi-note-cell")) "Return type only."))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "extern/undefined"))
                                   (td "JS undefined maps to #f; other results are wrapped as external values.")
                                   (td (@ (class "ffi-note-cell")) "Return type only."))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "extern/nullish"))
                                   (td "JS null or undefined map to #f; other results are wrapped as external values.")
                                   (td (@ (class "ffi-note-cell")) "Return type only."))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "i32"))
                                   (td "Signed 32-bit integer re-tagged as a fixnum.")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "u32"))
                                   (td "Unsigned 32-bit integer re-tagged as a fixnum.")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "f64"))
                                   (td "JavaScript number boxed as a flonum.")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "boolean"))
                                   (td "0/1 converted to #f/#t.")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))
                                  (tr
                                   (td (@ (class "ffi-type-cell")) (code "void"))
                                   (td "No result (equivalent to an empty result list).")
                                   (td (@ (class "ffi-note-cell is-empty")) "—"))))))
                     (p "For optional arguments, many bindings accept " (code "value") " and treat "
                        (code "(void)") " as JavaScript " (code "undefined") ".")
                     ,(doc-heading 2 "4. Crossing the boundary: argument conversion")
                     (p "WebRacket and JavaScript do not share a data representation, so arguments are "
                        "converted before a host call. The conversion is driven by the type signature in "
                        (code "define-foreign") ".")
                     (p "For simple, direct types, the values are unboxed and passed to the JavaScript import:")
                     (ul
                      (li (code "i32") " / " (code "u32") ": a fixnum becomes a signed/unsigned 32-bit integer.")
                      (li (code "f64") ": a flonum becomes a JavaScript number.")
                      (li (code "boolean") ": any non-false value becomes " (code "1") "; " (code "#f") " becomes " (code "0") ".")
                      (li (code "extern") ": no conversion is performed; the WebRacket value is an external wrapper "
                          "that refers to the original JavaScript object."))
                     (p "For " (code "string") ", " (code "string/symbol") ", and " (code "value") ", the compiler "
                        "uses FASL to move data through linear memory. The runtime encodes the argument "
                        "to FASL bytes, copies those bytes into linear memory, and passes the start index to "
                        "JavaScript. The JavaScript side then decodes the bytes via "
                        (code "fasl_to_js_value") " in " (code "assembler.rkt") ".")
                     (p "The rules in " (code "define-foreign.rkt") " enforce which types are legal: "
                        (code "string/symbol") " is only allowed for arguments, and " (code "void") " is "
                        "a synonym for an empty result list.")
                     ,(doc-heading 2 "5. Return values and conversion back to WebRacket")
                     (p "After the host call returns, the result is converted back to a WebRacket value using "
                        "the declared result type.")
                     (ul
                      (li (code "i32") " / " (code "u32") ": JavaScript numbers are re-tagged as fixnums.")
                      (li (code "f64") ": JavaScript numbers are boxed as flonums.")
                      (li (code "boolean") ": " (code "0") " becomes " (code "#f") "; any nonzero becomes " (code "#t") ".")
                      (li (code "string") " / " (code "value") ": JavaScript returns a FASL-encoded payload, "
                          "which is decoded by " (code "$linear-memory->string") " or "
                          (code "$linear-memory->value") ".")
                      (li (code "extern") ": the externref is wrapped back into a WebRacket external object." ))
                     (p "On the JavaScript side, " (code "js_value_to_fasl") " mirrors the decoder: numbers, "
                        "booleans, strings, symbols, vectors/arrays, and pairs are encoded explicitly; "
                        "anything else falls back to an external reference. That fallback is exactly how "
                        (code "extern") " values are kept on the JavaScript side while still being usable from "
                        "WebRacket.")
                     (div (@ (class "roadmap") (id "roadmap"))
                          (details (@ (class "roadmap-panel") (open "open"))
                                   (summary (@ (class "roadmap-summary"))
                                            (span "Roadmap")
                                            (span (@ (class "roadmap-kicker"))
                                                  "Planned sections"))
                                   (div (@ (class "roadmap-list"))
                                        (div (@ (class "roadmap-item") (data-target "value-marshaling-model-fasl"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "Value marshaling model (FASL)"))
                                        (div (@ (class "roadmap-item") (data-target "mapping-table-racket-js-values"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "Mapping table: Racket <-> JS values"))
                                        (div (@ (class "roadmap-item") (data-target "external-values-and-host-objects"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "External values and host objects"))
                                        (div (@ (class "roadmap-item") (data-target "optional-arguments-and-undefined"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "Optional arguments and undefined"))
                                        (div (@ (class "roadmap-item") (data-target "core-js-interop-primitives"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "Core JS interop primitives"))
                                        (div (@ (class "roadmap-item") (data-target "arrays-and-collections"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "Arrays and collections"))
                                        (div (@ (class "roadmap-item") (data-target "callbacks-from-js-into-webracket"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "Callbacks from JS into WebRacket"))
                                        (div (@ (class "roadmap-item") (data-target "ffi-modules-overview"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "FFI modules overview"))
                                        (div (@ (class "roadmap-item") (data-target "dom-manipulation-patterns"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "DOM manipulation patterns"))
                                        (div (@ (class "roadmap-item") (data-target "canvas-2d-patterns"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "Canvas 2D patterns"))
                                        (div (@ (class "roadmap-item") (data-target "timers-and-events"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "Timers and events"))
                                        (div (@ (class "roadmap-item") (data-target "async-and-promises"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "Async and promises"))
                                        (div (@ (class "roadmap-item") (data-target "audio-and-browser-apis"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "Audio and browser APIs"))
                                        (div (@ (class "roadmap-item") (data-target "troubleshooting-and-limitations"))
                                             (span (@ (class "roadmap-badge roadmap-badge--planned")) "Planned")
                                             (a (@ (class "roadmap-link")) "Troubleshooting and limitations")))))))
  (set! toc-items (reverse toc-items))
  `(div (@ (class "page page--docs"))
        ,(navbar)
        (section (@ (class "docs-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "FFI")
                           (span (@ (class "pill")) "JavaScript")
                           (span (@ (class "pill")) "Browser"))
                      (p (@ (class "hero-sublead"))
                         (a (@ (href "documentation.html")) "Documentation")
                         " / Guide to the JavaScript FFI")
                      (h1 (@ (class "hero-title")) "Guide to the JavaScript FFI")
                      (p (@ (class "hero-lead"))
                         "How WebRacket crosses the boundary to JavaScript.")))
        (div (@ (class "docs-layout"))
             (div (@ (class "docs-article"))
                  (div (@ (class "toc-mobile") (id "doc-toc-mobile"))
                       (details (@ (class "toc-accordion"))
                                (summary "On this page")
                                (div ,(doc-toc-list toc-items))))
                  ,doc-body)
             (aside (@ (class "docs-toc"))
                    (div (@ (class "toc-card"))
                         (div (@ (class "toc-title")) "On this page")
                         (nav (@ (class "toc-nav") (id "doc-toc") (aria-label "On this page"))
                              ,(doc-toc-list toc-items)))))
        (div (@ (class "copy-toast") (id "copy-toast") (role "status") (aria-live "polite")) "")
        ,(footer-section)))





;; init-extended-example-jsxgraph-board! : -> Void
;;   Loads JSXGraph assets (if needed) and renders the live board demo.
(define (init-extended-example-jsxgraph-board!)
  (define board-el (js-get-element-by-id "extended-jsx-board"))
  (define status-el (js-get-element-by-id "extended-jsx-board-status"))
  (define head (js-document-head))
  (define body (js-document-body))
  (define win (js-window-window))
  (define local-jsxgraph-css "assets/vendor/jsxgraph/jsxgraph.css")
  (define local-jsxgraph-js "assets/vendor/jsxgraph/jsxgraphcore.js")
  (define cdn-jsxgraph-css "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraph.css")
  (define cdn-jsxgraph-js "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js")
  (define (extern-present? v)
    (and (external? v) (not (js-nullish? v))))
  (define (board-ready?)
    (define v (and board-el (js-get-attribute board-el "data-jxg-ready")))
    (and (string? v) (string=? v "1")))
  (define (set-status! msg)
    (when status-el
      (js-set! status-el "textContent" msg)))
  (define (ensure-assets!)
    (define append-target
      (if (extern-present? head) head body))
    (unless (extern-present? (js-query-selector "link[data-jsxgraph-css='1']"))
      (define link (js-create-element "link"))
      (js-set-attribute! link "rel" "stylesheet")
      (js-set-attribute! link "href" cdn-jsxgraph-css)
      (js-set-attribute! link "data-jsxgraph-css" "1")
      (define on-css-error
        (procedure->external
         (lambda (_evt)
           (js-set-attribute! link "href" local-jsxgraph-css))))
      (js-add-event-listener! link "error" on-css-error)
      (js-append-child! append-target link))
    (unless (extern-present? (js-query-selector "script[data-jsxgraph-core='1']"))
      (define script (js-create-element "script"))
      (js-set-attribute! script "src" cdn-jsxgraph-js)
      (js-set-attribute! script "data-jsxgraph-core" "1")
      (define on-error
        (procedure->external
         (lambda (_evt)
           (js-set-attribute! script "src" local-jsxgraph-js))))
      (js-add-event-listener! script "error" on-error)
      (js-append-child! append-target script)))
  (define (initialize-board!)
    (define jxg (js-ref win "JXG"))
    (cond
      [(not (extern-present? jxg))
       #f]
      [else
       (define jsxgraph (js-ref jxg "JSXGraph"))
       (cond
         [(not (extern-present? jsxgraph))
          #f]
         [else
          (define attrs
            (js-object
             (vector (vector "boundingbox" #[-6 6 6 -6])
                     (vector "axis" #t)
                     (vector "keepaspectratio" #t))))
          (define board
            (js-send/extern jsxgraph "initBoard" (vector "extended-jsx-board" attrs)))
          (define p
            (js-send/extern board "create"
                            (vector "point" #[-3 1]
                                    (js-object
                                     (vector (vector "name" "P")
                                             (vector "size" 4)
                                             (vector "strokeColor" "#2b6cb0")
                                             (vector "fillColor" "#2b6cb0"))))))
          (define q
            (js-send/extern board "create"
                            (vector "point" #[2 2]
                                    (js-object
                                     (vector (vector "name" "Q")
                                             (vector "size" 4)
                                             (vector "strokeColor" "#2f855a")
                                             (vector "fillColor" "#2f855a"))))))
          (define r
            (js-send/extern board "create"
                            (vector "point" #[1 -3]
                                    (js-object
                                     (vector (vector "name" "R")
                                             (vector "size" 4)
                                             (vector "strokeColor" "#c53030")
                                             (vector "fillColor" "#c53030"))))))
          (js-send/extern board "update" (vector))
          (js-set-attribute! board-el "data-jxg-ready" "1")
          (set-status! "Board ready. Drag points P, Q, and R.")
          #t])]))
  (when board-el
    (if (board-ready?)
        (begin
          (set-status! "Board ready. Drag points P, Q, and R."))
        (let ([tries 0]
              [attempt-ext #f])
          (set-status! "Loading JSXGraph...")
          (ensure-assets!)
          (define (attempt . _args)
            (with-handlers ([exn? (lambda (_e)
                                    (set-status! "Failed to initialize JSXGraph board."))])
              (if (initialize-board!)
                  (void)
                  (begin
                    (set! tries (+ tries 1))
                    (if (< tries 80)
                        (js-window-set-timeout/delay attempt-ext 100.)
                        (begin
                          (set-status! "Failed to load JSXGraph assets.")))))))
          (set! attempt-ext (procedure->external attempt))
          (attempt #f)))))

;; init-extended-example-jsxgraph-geometry-board! : -> Void
;;   Loads JSXGraph assets (if needed) and renders the geometry constructor board demo.
(define (init-extended-example-jsxgraph-geometry-board!)
  (define board-el (js-get-element-by-id "extended-jsx-geometry-board"))
  (define status-el (js-get-element-by-id "extended-jsx-geometry-board-status"))
  (define head (js-document-head))
  (define body (js-document-body))
  (define win (js-window-window))
  (define local-jsxgraph-css "assets/vendor/jsxgraph/jsxgraph.css")
  (define local-jsxgraph-js "assets/vendor/jsxgraph/jsxgraphcore.js")
  (define cdn-jsxgraph-css "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraph.css")
  (define cdn-jsxgraph-js "https://cdn.jsdelivr.net/npm/jsxgraph/distrib/jsxgraphcore.js")
  (define (extern-present? v)
    (and (external? v) (not (js-nullish? v))))
  (define (board-ready?)
    (define v (and board-el (js-get-attribute board-el "data-jxg-ready")))
    (and (string? v) (string=? v "1")))
  (define (set-status! msg)
    (when status-el
      (js-set! status-el "textContent" msg)))
  (define (ensure-assets!)
    (define append-target
      (if (extern-present? head) head body))
    (unless (extern-present? (js-query-selector "link[data-jsxgraph-css='1']"))
      (define link (js-create-element "link"))
      (js-set-attribute! link "rel" "stylesheet")
      (js-set-attribute! link "href" cdn-jsxgraph-css)
      (js-set-attribute! link "data-jsxgraph-css" "1")
      (define on-css-error
        (procedure->external
         (lambda (_evt)
           (js-set-attribute! link "href" local-jsxgraph-css))))
      (js-add-event-listener! link "error" on-css-error)
      (js-append-child! append-target link))
    (unless (extern-present? (js-query-selector "script[data-jsxgraph-core='1']"))
      (define script (js-create-element "script"))
      (js-set-attribute! script "src" cdn-jsxgraph-js)
      (js-set-attribute! script "data-jsxgraph-core" "1")
      (define on-error
        (procedure->external
         (lambda (_evt)
           (js-set-attribute! script "src" local-jsxgraph-js))))
      (js-add-event-listener! script "error" on-error)
      (js-append-child! append-target script)))
  (define (initialize-board!)
    (define jxg (js-ref win "JXG"))
    (cond
      [(not (extern-present? jxg)) #f]
      [else
       (define jsxgraph (js-ref jxg "JSXGraph"))
       (cond
         [(not (extern-present? jsxgraph)) #f]
         [else
          (define attrs
            (js-object
             (vector (vector "boundingbox" #[-7 7 7 -7])
                     (vector "axis" #t)
                     (vector "keepaspectratio" #t))))
          (define board
            (js-send/extern jsxgraph "initBoard" (vector "extended-jsx-geometry-board" attrs)))
          (define p-parent '(-4 1))
          (define q-parent '(2 3))
          (define line-parents (list p-parent q-parent))
          (define text-parent '(-6 6 "P-Q constructors"))
          (define (list->js-array v)
            (cond
              [(list? v) (list->vector (map list->js-array v))]
              [else v]))
          (define p
            (js-send/extern board "create"
                            (vector "point" (list->js-array p-parent)
                                    (js-object (vector (vector "name" "P")
                                                       (vector "size" 4))))))
          (define q
            (js-send/extern board "create"
                            (vector "point" (list->js-array q-parent)
                                    (js-object (vector (vector "name" "Q")
                                                       (vector "size" 4))))))
          (define l
            (js-send/extern board "create"
                            (vector "line" (list->js-array line-parents) (js-object (vector)))))
          (define s
            (js-send/extern board "create"
                            (vector "segment" (list->js-array line-parents) (js-object (vector)))))
          (define c
            (js-send/extern board "create"
                            (vector "circle" (list->js-array line-parents) (js-object (vector)))))
          (define i
            (js-send/extern board "create"
                            (vector "point" (list->js-array '(0 0))
                                    (js-object (vector (vector "name" "I")
                                                       (vector "size" 3))))))
          (js-send/extern i "makeIntersection" (vector l c 0 0))
          (define _t
            (js-send/extern board "create"
                            (vector "text" (list->js-array text-parent) (js-object (vector)))))
          (js-send/extern board "suspendUpdate" (vector))
          (js-set! p "size" 5)
          (js-set! q "size" 5)
          (js-send/extern board "unsuspendUpdate" (vector))
          (js-send/extern board "fullUpdate" (vector))
          (js-set-attribute! board-el "data-jxg-ready" "1")
          (set-status! "Board ready. Phase 2 constructors demo loaded.")
          #t])]))
  (when board-el
    (if (board-ready?)
        (set-status! "Board ready. Phase 2 constructors demo loaded.")
        (let ([tries 0]
              [attempt-ext #f])
          (set-status! "Loading JSXGraph...")
          (ensure-assets!)
          (define (attempt . _args)
            (with-handlers ([exn? (lambda (_e)
                                    (set-status! "Failed to initialize JSXGraph board."))])
              (if (initialize-board!)
                  (void)
                  (begin
                    (set! tries (+ tries 1))
                    (if (< tries 80)
                        (js-window-set-timeout/delay attempt-ext 100.)
                        (set-status! "Failed to load JSXGraph assets."))))))
          (set! attempt-ext (procedure->external attempt))
          (attempt #f)))))

;; init-doc-js-ffi-page! : -> Void
;;   Enhances the JS FFI doc page using WebRacket DOM helpers.
(define (init-doc-js-ffi-page!)
  (define doc-root (js-query-selector ".docs-article"))
  (define content (and doc-root (js-element-query-selector doc-root ".doc-prose, .doc-content")))
  (define docs-layout (js-query-selector ".docs-layout"))
  (when (and doc-root content)
    (let* ([body (js-document-body)]
           [extended-jsx-board-page?
            (memq (current-page)
                  '(doc-extended-example-jsxgraph-board-points
                    doc-extended-example-jsxgraph-geometry-constructors))]
           [page-root (js-query-selector ".page")]
           [page-class-names (element-class-string page-root)]
           [ffi-reference-page?
            (and (string? page-class-names)
                 (for/or ([tok (in-list (string-split page-class-names))])
                   (string=? tok "page--ffi-reference")))]
           [toc-nav (js-get-element-by-id "doc-toc")]
           [toc-card (js-get-element-by-id "doc-toc-card")]
           [toc-toggle (js-get-element-by-id "doc-toc-toggle")]
           [toc-collapsed? #f]
           [toast (js-get-element-by-id "copy-toast")]
           [toast-timer #f]
           [debug-toc? #f]
           [headings (append (node-list->list (js-query-selector-all "h2"))
                             (node-list->list (js-query-selector-all "h3")))]
           [toc-links (node-list->list (js-query-selector-all "[data-toc-link]"))]
           [main-toc-list #f]
           [link-by-id (make-hasheq)]
           [current-active #f]
           [scroll-tick #f]
           [code-blocks (node-list->list (js-element-query-selector-all content "pre > code"))]
           [enable-line-numbers
            (let* ([dataset (and body (js-ref body "dataset"))]
                   [flag (and dataset (js-ref dataset "codeLines"))])
              (and (string? flag) (string=? flag "true")))]
           [roadmap-items (node-list->list (js-query-selector-all ".roadmap-item[data-target]"))]
           [toc-log
            (lambda (msg)
              (when debug-toc?
                (js-log (string-append "[toc] " msg))))]
           [show-toast
            (lambda (msg)
              (when toast
                (js-set! toast "textContent" msg)
                (classlist-add! toast "is-visible")
                (when toast-timer
                  (js-window-clear-timeout toast-timer))
                (let ([hide-handler
                       (procedure->external
                        (lambda _
                          (classlist-remove! toast "is-visible")))])
                  (remember-doc-js-ffi-handler! hide-handler)
                  (set! toast-timer (js-window-set-timeout/delay hide-handler 1600.)))))]
           [copy-text
            (lambda (text)
              (let* ([helper (js-create-element "textarea")]
                     [style (js-ref helper "style")])
                (js-set! helper "value" text)
                (js-set-attribute! helper "readonly" "")
                (when style
                  (js-set! style "position" "absolute")
                  (js-set! style "left" "-9999px"))
                (js-append-child! body helper)
                (js-send helper "select" (vector))
                (let ([ok (js-send/truthy (js-document) "execCommand" (vector "copy"))])
                  (js-remove! helper)
                  ok)))]
           [normalize-id
            (lambda (v)
              (cond
                [(string? v) v]
                [(external? v) (external-string->string v)]
                [(symbol? v) (symbol->string v)]
                [else ""]))]
           [string-prefix-ci?
            (lambda (s prefix)
              (and (string? s)
                   (string? prefix)
                   (>= (string-length s) (string-length prefix))
                   (string=? (string-downcase (substring s 0 (string-length prefix)))
                             (string-downcase prefix))))]
           [substring-index
            (lambda (s needle)
              (define s-len (string-length s))
              (define n-len (string-length needle))
              (let loop ([i 0])
                (cond
                  [(> (+ i n-len) s-len) #f]
                  [(string=? (substring s i (+ i n-len)) needle) i]
                  [else (loop (add1 i))])))]
           [split-toc-label
            (lambda (raw)
              (define s (string-trim (if (string? raw) raw "")))
              (cond
                [(string-prefix-ci? s "chapter ")
                 (let* ([emdash-pos (substring-index s "—")]
                        [dash-pos (substring-index s " - ")]
                        [sep-pos (cond
                                   [(number? emdash-pos) emdash-pos]
                                   [(number? dash-pos) dash-pos]
                                   [else #f])])
                   (if (number? sep-pos)
                       (list 'chapter
                             (string-trim (substring s 0 sep-pos))
                             (string-trim (substring s (if (number? emdash-pos) (add1 sep-pos) (+ sep-pos 3)))))
                       (list 'chapter s "")))]
                [else
                 (let loop ([i 0] [saw-digit? #f] [saw-dot? #f])
                   (cond
                     [(>= i (string-length s))
                      (list 'section "" s)]
                     [else
                      (define ch (string-ref s i))
                      (cond
                        [(char-numeric? ch) (loop (add1 i) #t saw-dot?)]
                        [(char=? ch #\.) (loop (add1 i) saw-digit? #t)]
                        [(and saw-digit? saw-dot? (char-whitespace? ch))
                         (list 'section
                               (string-trim (substring s 0 i))
                               (string-trim (substring s i)))]
                        [else
                         (list 'section "" s)])]))]))]
           [heading-tag?
            (lambda (node)
              (if (external? node)
                  (let ([tag (normalize-id (js-ref node "tagName"))])
                    (or (string=? tag "H1")
                        (string=? tag "H2")
                        (string=? tag "H3")
                        (string=? tag "H4")
                        (string=? tag "H5")
                        (string=? tag "H6")))
                  #f))]
           [id->key
            (lambda (id)
              (let ([s (normalize-id id)])
                (and (string? s) (not (string=? s "")) (string->symbol s))))]
           [mark-active-chapter!
            (lambda (active-id)
              (when ffi-reference-page?
                (for ((link (in-list toc-links)))
                  (classlist-remove! link "is-active-parent"))
                (let loop ([links toc-links]
                           [last-chapter #f])
                  (when (pair? links)
                    (define link (car links))
                    (define kind (normalize-id (js-get-attribute link "data-toc-kind")))
                    (define id (normalize-id (js-get-attribute link "data-toc-link")))
                    (define chapter-link
                      (if (string=? kind "chapter")
                          link
                          last-chapter))
                    (when (and (string? id) (string=? id active-id))
                      (when chapter-link
                        (classlist-add! chapter-link "is-active-parent")))
                    (loop (cdr links) chapter-link)))))]
           [keep-active-link-visible!
            (lambda ()
              (when toc-nav
                (define active-link (js-element-query-selector toc-nav ".toc-link.is-active"))
                (when active-link
                  (js-send active-link "scrollIntoView"
                           (vector (js-object (vector (vector "block" "nearest")
                                                      (vector "inline" "nearest")
                                                      (vector "behavior" "auto"))))))))]
           [set-active!
            (lambda (id)
              (define current-key #f)
              (define new-key #f)
              (when (and (string? id) (not (equal? id current-active)))
                (set! current-key (and current-active (id->key current-active)))
                (when current-key
                  (when (hash-has-key? link-by-id current-key)
                    (for ((link (in-list (hash-ref link-by-id current-key))))
                      (classlist-remove! link "is-active")
                      (js-remove-attribute! link "aria-current"))))
                (set! new-key (id->key id))
                (when (hash-has-key? link-by-id new-key)
                  (for ((link (in-list (hash-ref link-by-id new-key))))
                    (classlist-add! link "is-active")
                    (js-set-attribute! link "aria-current" "true"))
                  (set! current-active id)
                  (mark-active-chapter! id)
                  (keep-active-link-visible!))))
            ]
           [smooth-scroll-to!
            (lambda (target)
              (when target
                (js-send target "scrollIntoView"
                         (vector (js-object (vector (vector "behavior" "smooth")
                                                    (vector "block" "start")))))))]
           [active-from-scroll!
            (lambda ()
              (let* ([threshold (* 0.35 (js-window-inner-height))]
                     [candidate #f])
                (for ((heading (in-list headings)))
                  (let* ([id (normalize-id (js-ref heading "id"))]
                         [key (id->key id)]
                         [rect (js-get-bounding-client-rect heading)]
                         [top (and rect (js-ref rect "top"))])
                    (when (and key (hash-has-key? link-by-id key)
                               (number? top) (<= top threshold))
                      (set! candidate id))))
                (when (not candidate)
                  (let ([first-id
                         (for/first ((heading (in-list headings))
                                     #:when (let* ([id (normalize-id (js-ref heading "id"))]
                                                   [key (id->key id)])
                                              (and key (hash-has-key? link-by-id key))))
                           (normalize-id (js-ref heading "id")))])
                    (set! candidate first-id)))
                (when (string? candidate)
                  (set-active! candidate))))]
           [active-at-bottom!
            (lambda ()
              (let* ([inner (js-window-inner-height)]
                     [scroll-y (js-window-scroll-y)]
                     [height (and body (js-ref body "offsetHeight"))])
                (and (number? inner) (number? scroll-y) (number? height)
                     (>= (+ inner scroll-y) (- height 2)))))]
           [select-last-heading!
            (lambda ()
              (let ([last-id
                     (for/first ((heading (in-list (reverse headings)))
                                 #:when (let* ([id (normalize-id (js-ref heading "id"))]
                                               [key (id->key id)])
                                          (and key (hash-has-key? link-by-id key))))
                       (normalize-id (js-ref heading "id")))])
                (when (string? last-id)
                  (set-active! last-id))))]
           [raf-handler
            (procedure->external
             (lambda (_)
               (set! scroll-tick #f)
               (if (active-at-bottom!)
                   (select-last-heading!)
                   (active-from-scroll!))))]
           [scroll-handler
            (procedure->external
             (lambda (_)
               (when (not scroll-tick)
                 (set! scroll-tick (js-window-request-animation-frame raf-handler)))))]
           [resize-handler
            (procedure->external
             (lambda (_)
               (toc-log "resize event")
               (active-from-scroll!)))])
          (define (toc-link-id link)
            (define raw-id (js-get-attribute link "data-toc-link"))
            (define dataset (js-ref link "dataset"))
            (define dataset-id (and dataset (js-ref dataset "tocLink")))
            (define href (js-get-attribute link "href"))
            (cond
              [(not (string=? (normalize-id raw-id) "")) (normalize-id raw-id)]
              [(not (string=? (normalize-id dataset-id) "")) (normalize-id dataset-id)]
              [else
               (if (and (string? href)
                        (> (string-length href) 1)
                        (char=? (string-ref href 0) #\#))
                   (substring href 1)
                   "")]))
          (define (enhance-chapter-rhythm!)
            (when (and ffi-reference-page? content)
              (define h2s (node-list->list (js-element-query-selector-all content "h2")))
              (for ([h (in-list h2s)])
                (define title (string-trim (element-text h)))
                (when (string-prefix-ci? title "chapter ")
                  (classlist-add! h "ffi-chapter-title")
                  (let find-lead ([node (js-ref h "nextElementSibling")])
                    (when (external? node)
                      (define tag (normalize-id (js-ref node "tagName")))
                      (cond
                        [(string=? tag "P")
                         (when (not (string=? (string-trim (element-text node)) ""))
                           (classlist-add! node "ffi-chapter-lead"))]
                        [(or (string=? tag "H2") (string=? tag "H3")) (void)]
                        [else (find-lead (js-ref node "nextElementSibling"))])))))))
          ;; ---- Execute behavior ----
          (enhance-chapter-rhythm!)
          (when (and ffi-reference-page? content)
            (define (toc-list-like? node)
              (and (external? node)
                   (let ([tag (normalize-id (js-ref node "tagName"))])
                     (or (string=? tag "UL") (string=? tag "OL")))
                   (let* ([links (node-list->list (js-element-query-selector-all node "li a"))]
                          [count (length links)]
                          [chapter-ish?
                           (for/or ([a (in-list links)])
                             (string-prefix-ci? (string-trim (element-text a)) "chapter "))])
                     (and (>= count 4) chapter-ish?))))
            (define headings-h2-h3 (node-list->list (js-element-query-selector-all content "h2, h3")))
            (define toc-heading
              (or (js-element-query-selector content "#table-of-contents")
                  (for/first ([h (in-list headings-h2-h3)]
                              #:when (or (string=? (string-downcase (string-trim (element-text h))) "table of contents")
                                         (string-prefix-ci? (string-trim (element-text h)) "table of contents")))
                    h)))
            (define (style-main-toc-list! lst)
              (set! main-toc-list lst)
              (classlist-add! main-toc-list "ffi-main-toc-list")
              (let ([entries (node-list->list (js-element-query-selector-all main-toc-list "li"))])
                (for ([entry (in-list entries)])
                  (define link (js-element-query-selector entry "a"))
                  (when link
                    (define raw-label (string-trim (element-text link)))
                    (define parsed (split-toc-label raw-label))
                    (define kind (if (and (pair? parsed) (eq? (car parsed) 'chapter))
                                     "chapter"
                                     "section"))
                    (define num (if (>= (length parsed) 2) (cadr parsed) ""))
                    (define title (if (>= (length parsed) 3) (caddr parsed) ""))
                    (classlist-add! entry "ffi-main-toc-entry")
                    (if (string=? kind "chapter")
                        (classlist-add! entry "ffi-main-toc-entry--chapter")
                        (classlist-add! entry "ffi-main-toc-entry--section"))
                    (classlist-add! link "toc-link")
                    (if (string=? kind "chapter")
                        (classlist-add! link "toc-link--chapter")
                        (classlist-add! link "toc-link--section"))
                    (js-set-attribute! link "data-toc-kind" kind)
                    (define href (js-get-attribute link "href"))
                    (when (and (string? href)
                               (> (string-length href) 1)
                               (char=? (string-ref href 0) #\#))
                      (js-set-attribute! link "data-toc-link" (substring href 1)))
                    (js-set! link "textContent" "")
                    (when (and (string? num) (not (string=? num "")))
                      (define num-node (js-create-element "span"))
                      (js-set-attribute! num-node "class" "toc-num")
                      (js-set! num-node "textContent" num)
                      (js-append-child! link num-node))
                    (define title-node (js-create-element "span"))
                    (js-set-attribute! title-node "class" "toc-title")
                    (js-set! title-node "textContent"
                             (if (and (string? title) (not (string=? title ""))) title raw-label))
                    (js-append-child! link title-node)))))
            (cond
              [(and toc-heading
                    (let loop ([node (js-ref toc-heading "nextElementSibling")])
                      (cond
                        [(or (not (external? node)) (heading-tag? node)) #f]
                        [(toc-list-like? node) (style-main-toc-list! node) #t]
                        [else (loop (js-ref node "nextElementSibling"))])))]
              [else
               ;; Fallback for docs where the TOC heading shape differs.
               (define lists (node-list->list (js-element-query-selector-all content "ul, ol")))
               (define fallback
                 (for/first ([lst (in-list lists)] #:when (toc-list-like? lst))
                   lst))
               (when fallback
                 (style-main-toc-list! fallback))]))
          (when (and ffi-reference-page? content)
            (define tables (node-list->list (js-element-query-selector-all content ".ffi-type-table")))
            (for ([tbl (in-list tables)])
              (define headers (node-list->list (js-element-query-selector-all tbl "thead th")))
              (define function-idx
                (for/first ([th (in-list headers)]
                            [i (in-naturals)]
                            #:when (string=? (string-downcase (string-trim (element-text th))) "function"))
                  i))
              (define use-when-idx
                (for/first ([th (in-list headers)]
                            [i (in-naturals)]
                            #:when (string=? (string-downcase (string-trim (element-text th))) "use when"))
                  i))
              (when (number? function-idx)
                (define function-th (list-ref headers function-idx))
                (classlist-add! function-th "ffi-function-col"))
              (define rows (node-list->list (js-element-query-selector-all tbl "tbody tr")))
              (when (number? use-when-idx)
                (define header-th (list-ref headers use-when-idx))
                (classlist-add! header-th "ffi-use-when-col"))
              (for ([row (in-list rows)])
                (define cells (node-list->list (js-element-query-selector-all row "td")))
                (when (and (number? function-idx) (< function-idx (length cells)))
                  (classlist-add! (list-ref cells function-idx) "ffi-function-col"))
                (when (and (number? use-when-idx) (< use-when-idx (length cells)))
                  (classlist-add! (list-ref cells use-when-idx) "ffi-use-when-col")))))
          (set! toc-links (node-list->list (js-query-selector-all "[data-toc-link]")))
          (for ((heading (in-list headings)))
            (define text (element-text heading))
            (define id (and heading (js-ref heading "id")))
            (when (and (string? text) (string? id) (not (string=? id "")))
              (define anchor (js-create-element "button"))
              (js-set-attribute! anchor "type" "button")
              (js-set-attribute! anchor "class" "heading-anchor")
              (js-set-attribute! anchor "aria-label" (string-append "Copy link to " text))
              (js-set-attribute! anchor "title" "Copy link")
              (js-set! anchor "textContent" "#")
              (define anchor-handler
                (procedure->external
                 (lambda (evt)
                   (js-event-prevent-default evt)
                   (js-event-stop-propagation evt)
                   (define loc (js-window-location))
                   (define origin (and loc (js-ref loc "origin")))
                   (define pathname (and loc (js-ref loc "pathname")))
                   (define url
                     (cond
                       ((and (string? origin) (string? pathname))
                        (string-append origin pathname "#" id))
                       (else (string-append "#" id))))
                   (define ok (copy-text url))
                   (js-send (js-window-history) "replaceState"
                            (vector (js-null) "" (string-append "#" id)))
                   (show-toast (if ok "Link copied" "Copy failed")))))
              (remember-doc-js-ffi-handler! anchor-handler)
              (js-add-event-listener! anchor "click" anchor-handler)
              (js-append-child! heading anchor)))

          (for ((link (in-list toc-links)))
            (define id (toc-link-id link))
            (define key (and (string? id) (not (string=? id "")) (id->key id)))
            (define class-val (js-get-attribute link "class"))
            (when key
              (define current (hash-ref link-by-id key '()))
              (hash-set! link-by-id key (cons link current))))

          ;; Default to the first TOC link if available.
          (when (pair? toc-links)
            (define first-id
              (for/first ((link (in-list toc-links))
                          #:when (let ((id (toc-link-id link)))
                                   (and (string? id) (not (string=? id "")))))
                (toc-link-id link)))
            (when (string? first-id)
              (set-active! first-id)))

          (for ((link (in-list toc-links)))
            (define id (toc-link-id link))
            (when (and (string? id) (not (string=? id "")))
              (define link-handler
                (procedure->external
                 (lambda (evt)
                   (toc-log (format "toc click: ~a" id))
                   (js-event-prevent-default evt)
                   (define target (js-get-element-by-id id))
                   (when target
                     (smooth-scroll-to! target)
                     (js-send (js-window-history) "replaceState"
                              (vector (js-null) "" (string-append "#" id)))))))
              (remember-doc-js-ffi-handler! link-handler)
              (js-add-event-listener! link "click" link-handler)))

          (js-add-event-listener! (js-window-window) "scroll" scroll-handler)
          (js-add-event-listener! (js-window-window) "resize" resize-handler)

          (when (and toc-card toc-toggle toc-nav)
            (define (sync-toc-toggle!)
              (if toc-collapsed?
                  (begin
                    (when docs-layout (classlist-add! docs-layout "is-toc-collapsed"))
                    (classlist-add! toc-card "is-collapsed")
                    (js-set! toc-toggle "textContent" "Restore")
                    (js-set-attribute! toc-toggle "aria-expanded" "false"))
                  (begin
                    (when docs-layout (classlist-remove! docs-layout "is-toc-collapsed"))
                    (classlist-remove! toc-card "is-collapsed")
                    (js-set! toc-toggle "textContent" "Collapse")
                    (js-set-attribute! toc-toggle "aria-expanded" "true"))))
            (sync-toc-toggle!)
            (define toc-toggle-handler
              (procedure->external
               (lambda (_evt)
                 (set! toc-collapsed? (not toc-collapsed?))
                 (sync-toc-toggle!))))
            (remember-doc-js-ffi-handler! toc-toggle-handler)
            (js-add-event-listener! toc-toggle "click" toc-toggle-handler))

          (active-from-scroll!)
          (define hash (js-ref (js-window-location) "hash"))
          (when (and (string? hash) (> (string-length hash) 1))
            (set-active! (substring hash 1)))

          (for ((code (in-list code-blocks)))
            (define pre (js-ref code "parentElement"))
            (define parent (and pre (js-ref pre "parentNode")))
            (when (and pre parent)
              (define wrapper (js-create-element "div"))
              (js-set-attribute! wrapper "class" "code-block")
              (js-send parent "insertBefore" (vector wrapper pre))
              (js-append-child! wrapper pre)

              (define toolbar (js-create-element "div"))
              (js-set-attribute! toolbar "class" "code-toolbar")
              (define label (js-create-element "div"))
              (js-set-attribute! label "class" "code-toolbar-label")
              (js-set! label "textContent" "Code")
              (define button (js-create-element "button"))
              (js-set-attribute! button "type" "button")
              (js-set-attribute! button "class" "code-copy")
              (js-set-attribute! button "aria-label" "Copy code to clipboard")
              (js-set! button "textContent" "Copy")
              (js-append-child! toolbar label)
              (js-append-child! toolbar button)
              (js-send wrapper "insertBefore" (vector toolbar pre))

              (define raw (let ((text (js-ref code "textContent")))
                            (if (string? text) (trim-trailing-newline text) "")))
              (js-set-attribute! code "data-raw" raw)
              (define copy-handler
                (procedure->external
                 (lambda (_)
                   (define ok (copy-text raw))
                   (show-toast (if ok "Copied to clipboard" "Copy failed")))))
              (remember-doc-js-ffi-handler! copy-handler)
              (js-add-event-listener! button "click" copy-handler)

              (when enable-line-numbers
                (define lines (string-split raw "\n"))
                (define html
                  (string-join
                   (for/list ((line (in-list lines)))
                     (string-append "<span class=\"code-line\">"
                                    (escape-html line)
                                    "</span>"))
                   "\n"))
                (js-set! code "innerHTML" html)
                (js-set-attribute! wrapper "data-line-numbers" "true"))))

          (for ((item (in-list roadmap-items)))
            (define target (js-get-attribute item "data-target"))
            (define link (js-element-query-selector item ".roadmap-link"))
            (when link
              (cond
                ((and (string? target) (js-get-element-by-id target))
                 (js-set-attribute! link "href" (string-append "#" target))
                 (classlist-remove! link "is-disabled")
                 (js-set-attribute! link "aria-disabled" "false")
                 (js-remove-attribute! link "tabindex"))
                (else
                 (classlist-add! link "is-disabled")
                 (js-set-attribute! link "aria-disabled" "true")
                 (js-set-attribute! link "href" "#roadmap")
                 (js-remove-attribute! link "tabindex")
                 (define disable-handler
                   (procedure->external
                    (lambda (evt)
                      (js-event-prevent-default evt))))
                 (remember-doc-js-ffi-handler! disable-handler)
                 (js-add-event-listener! link "click" disable-handler)))))
        (when (eq? (current-page) 'doc-extended-example-jsxgraph-board-points)
          (init-extended-example-jsxgraph-board!))
        (when (eq? (current-page) 'doc-extended-example-jsxgraph-geometry-constructors)
          (init-extended-example-jsxgraph-geometry-board!))
        (void))))

;; quick-start-page : -> List
;;   Quick Start page layout.
(define (quick-start-page)
  `(div (@ (class "page page--docs page--quick-start"))
        ,(navbar)
        (section (@ (class "docs-hero"))
                 (div (@ (class "hero-panel"))
                      (h1 (@ (class "hero-title")) "Quick Start")
                      (p (@ (class "hero-lead"))
                         "Get a WebRacket program running in your browser in about 10 minutes.")
                      (p "This guide shows the shortest path from a fresh checkout to a working demo. "
                         "If you want full setup details, see the "
                         (a (@ (href "installation.html")) "Installation")
                         " page.")))
        ,(section-block
          "What You’ll Do"
          #f
          (list
           `(ul
             (li "Run a WebRacket demo locally")
             (li "Open a Racket program running in the browser")
             (li "See where to go next (" (a (@ (href "examples.html")) "examples") ", "
                 (a (@ (href "documentation.html")) "docs") ", and "
                 (a (@ (href "implementation-status.html")) "status") ")")))
          #f
          "section--quick-start-what")
        (div (@ (class "quick-start-steps"))
             ,(quick-start-step
               1
               "Minimal Requirements"
               #f
               (list
                `(p "You’ll need:")
                `(ul
                  (li "Racket (9.0 or newer)")
                  (li "Node.js")
                  (li "wasm-tools"))
                `(p "If you already installed everything, skip to Step 2.")
                `(p "Otherwise, follow the full " (a (@ (href "installation.html")) "Installation") " guide.")))
             ,(quick-start-step
               2
               "Get WebRacket"
               #f
               (list
                `(p "Clone the repository and enter it:")
                `(pre (code "git clone https://github.com/soegaard/webracket.git\ncd webracket"))))
             ,(quick-start-step
               3
               "Run a Demo Site"
               #f
               (list
                `(p "WebRacket includes ready-made demos. To serve them locally:")
                `(pre (code "cd examples\nraco static-web"))
                `(p "This starts a local web server.")
                `(p "Now open your browser at:")
                `(p (a (@ (href "http://localhost:8000")) "http://localhost:8000"))))
             ,(quick-start-step
               4
               "Try a Demo"
               #f
               (list
                `(p "Pick one of the demos and open it in your browser.")
                `(p "Some good starting points:")
                `(ul
                  (li "MathJax Editor — live LaTeX preview in the browser")
                  (li "Mini REPL — interactive Racket in a web page")
                  (li "Graphics / Canvas demos — Racket driving browser graphics"
                      (p "Each demo shows Racket code compiled to WebAssembly and executed in the browser.")
                      (p "If something breaks, try a different demo — support varies by feature.")))))
             ,(quick-start-step
               5
               "Peek at the Source"
               #f
               (list
                `(p "Each demo has a Racket source file in the examples/ directory.")
                `(p "Open one and notice:")
                `(ul
                  (li "It looks like normal Racket")
                  (li "It uses browser features (DOM, canvas, terminal, etc.)")
                  (li "It is compiled to WebAssembly before running"
                      (p "You don’t need to understand the compiler yet — just get a feel for how Racket "
                         "maps to the browser."))))))
        ,(section-block
          "How to Think About WebRacket (Mental Model)"
          #f
          (list
           `(p "At a high level:")
           `(ul
             (li "You write Racket")
             (li "WebRacket compiles it to WebAssembly")
             (li "The browser runs it like a native module")
             (li "JavaScript is used only for interoperability with the web"
                 (p "You’re still programming in Racket — just targeting the browser."))))
          #f
          #f)
        ,(section-block
          "If Something Doesn’t Work"
          #f
          (list
           (callout
            'note
            #f
            `(p "WebRacket is still evolving, and not all Racket features are implemented.")
            `(p "If a demo fails:")
            `(ul
              (li "Try another example")
              (li "Check the " (a (@ (href "implementation-status.html")) "Status Dashboard")
                  " to see what’s implemented")
              (li "Ask for help in the " (a (@ (href "community.html")) "Community") " page"))))
          #f
          "section--quick-start-callout")
        ,(section-block
          "Where to Go Next"
          #f
          (list
           `(p "Choose your next path:")
           (card-grid
            (list
             (list `(h3 "▶ Explore more demos")
                   `(div (@ (class "doc-cta-group"))
                         (a (@ (class "doc-cta doc-cta--primary")
                               (href "examples.html"))
                            "Go to Examples")))
             (list `(h3 "▶ Learn how WebRacket works")
                   `(div (@ (class "doc-cta-group"))
                         (a (@ (class "doc-cta doc-cta--primary")
                               (href "documentation.html"))
                            "Go to Documentation")))
             (list `(h3 "▶ See what’s implemented")
                   `(div (@ (class "doc-cta-group"))
                         (a (@ (class "doc-cta doc-cta--primary")
                               (href "implementation-status.html"))
                            "Go to the Status Dashboard")))
             (list `(h3 "▶ Set up a full development environment")
                   `(div (@ (class "doc-cta-group"))
                         (a (@ (class "doc-cta doc-cta--primary")
                               (href "installation.html"))
                            "Go to Installation")))
             (list `(h3 "▶ Ask questions or share feedback")
                   `(div (@ (class "doc-cta-group"))
                         (a (@ (class "doc-cta doc-cta--primary")
                               (href "community.html"))
                            "Go to Community"))))
            "card-grid--quick-start"))
          #f
          #f)
        #;,(section-block
          "Optional: Your First Tiny Program"
          #f
          (list
           (callout
            'info
            #f
            `(p "If you want to try a minimal Racket program, create: " (code "hello.rkt"))
            `(pre (code "#lang racket\n\n(displayln \"Hello from WebRacket!\")"))
            `(p "Then compile it using the WebRacket toolchain (see "
                (a (@ (href "documentation.html")) "Documentation")
                " for details).")))
          #f
          "section--quick-start-callout")
        ,(footer-section)))

;; installation-page : -> List
;;   Installation page layout.
(define (installation-page)
  `(div (@ (class "page"))
        ,(navbar)
        (section (@ (class "install-hero"))
                 (div (@ (class "install-hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "wasm-tools")
                           (span (@ (class "pill")) "Node.js")
                           (span (@ (class "pill")) "Racket 9+")
                           (span (@ (class "pill")) "raco-static-web"))
                      (h1 (@ (class "hero-title")) "Installation")
                      (p (@ (class "hero-lead"))
                         "Set up WebRacket for terminal and browser workflows.")
                      (p (@ (class "install-hero-note"))
                         "Requires wasm-tools and Node.js for compilation and testing.")))
        ,(section-block
          "Prerequisites"
          "Short version checklist for a working WebRacket setup."
          (list
           `(div (@ (class "install-grid"))
                 (div (@ (class "install-grid-item"))
                      (span (@ (class "install-grid-icon")) "✓")
                      (span (@ (class "install-grid-text")) "wasm-tools (Bytecode Alliance) v1.243.0+"))
                 (div (@ (class "install-grid-item"))
                      (span (@ (class "install-grid-icon")) "✓")
                      (span (@ (class "install-grid-text"))
                            "Node.js (must support "
                            (code "--experimental-wasm-exnref")
                            ")."))
                 (div (@ (class "install-grid-item"))
                      (span (@ (class "install-grid-icon")) "✓")
                      (span (@ (class "install-grid-text")) "Racket 9.0"))
                 (div (@ (class "install-grid-item"))
                      (span (@ (class "install-grid-icon")) "✓")
                      (span (@ (class "install-grid-text")) "raco-static-web"))
                 (div (@ (class "install-grid-item"))
                      (span (@ (class "install-grid-icon")) "✓")
                      (span (@ (class "install-grid-text")) "WebRacket repo clone"))))
          #f
          "install-section")
        ,(section-block
          "Installation Steps"
          "Follow the steps below to install dependencies and run WebRacket locally."
          (list
           `(div (@ (class "install-steps"))
                 (div (@ (class "install-step-card"))
                      (h3 "1. wasm-tools")
                      (p "Download the latest wasm-tools release.")
                      (p (a (@ (href "https://github.com/bytecodealliance/wasm-tools/releases"))
                            "https://github.com/bytecodealliance/wasm-tools/releases"))
                      (p "Extract it and add it to your PATH.")
                      (pre (code "tar -xvf wasm-tools-1.243.0-aarch64-macos.tar.gz"))
                      (pre (code "sudo mv wasm-tools /usr/local/bin/"))
                      (p "Verify:")
                      (pre (code "wasm-tools"))
                      (div (@ (class "callout"))
                           (strong "Troubleshooting: ")
                           "On macOS, you may get a Privacy & Security prompt. Allow "
                           (code "wasm-tools")
                           " to run from System Settings."))
                 (div (@ (class "install-step-card"))
                      (h3 "2. Node.js")
                      (p "Install Node.js.")
                      (p (a (@ (href "https://nodejs.org/en/download")) "https://nodejs.org/en/download"))
                      (p "Confirm Node runs:")
                      (pre (code "node"))
                      (p "Confirm required flags:")
                      (pre (code "node --experimental-wasm-exnref --expose-gc"))
                      (div (@ (class "callout"))
                           (strong "Troubleshooting: ")
                           "WebRacket needs support for "
                           (code "--experimental-wasm-exnref")
                           ", so the command above must start without errors."))
                 (div (@ (class "install-step-card"))
                      (h3 "3. Racket")
                      (p "Install Racket 9 or newer.")
                      (p (a (@ (href "https://download.racket-lang.org/"))
                            "https://download.racket-lang.org/")))
                 (div (@ (class "install-step-card"))
                      (h3 "4. raco-static-web")
                      (p "Install the local web server package, then run it in a folder with an HTML file.")
                      (pre (code "raco pkg install raco-static-web"))
                      (pre (code "raco static-web")))
                 (div (@ (class "install-step-card"))
                      (h3 "5. Clone the WebRacket repo")
                      (p "Clone the WebRacket repository (contains compiler and examples).")
                      (pre (code "git clone https://github.com/soegaard/webracket.git")))
                 (div (@ (class "install-step-card") (id "install-quick-test"))
                      (h3 "6. Quick test: Run the examples")
                      (p "Serve the examples and open one in your browser.")
                      (pre (code "cd examples\nraco static-web"))
                      (p "Open http://localhost:8000/ and select an example HTML file.")))
           `(div (@ (class "install-next-steps"))
                 (h3 "Next steps")
                 (p "Keep exploring with a quick demo or a deeper read.")
                 (ul (@ (class "install-next-steps-list"))
                     (li (a (@ (href "#install-quick-test")) "Try the examples"))
                     (li (a (@ (href "examples.html")) "Browse live demos")))))
          #f
          "install-section")
        ,(footer-section)))

;; community-page : -> List
;;   Community page layout.
(define (community-page)
  `(div (@ (class "page"))
        ,(navbar)
        (section (@ (class "docs-hero"))
                 (div (@ (class "hero-panel"))
                      (h1 (@ (class "hero-title")) "Community")
                      (p (@ (class "hero-lead"))
                         "Where to ask questions, share progress, and get help.")))
        ,(section-block
          "Community spaces"
          "Choose the best place to connect with the Racket community."
          (list
           (card-grid
            (list
             (list `(h3 "Racket Discourse")
                   `(p "Forum for questions, announcements, and longer-form discussions.")
                   `(div (@ (class "doc-cta-group"))
                         (a (@ (class "doc-cta doc-cta--primary")
                               (href "https://racket.discourse.group/")
                               (target "_blank")
                               (rel "noreferrer noopener"))
                            "Open Discourse")))
             (list `(h3 "Racket Discord")
                   `(p "Real-time chat for quick questions and coordination.")
                   `(div (@ (class "doc-cta-group"))
                         (a (@ (class "doc-cta doc-cta--primary")
                               (href "https://discord.gg/6Zq8sH5")
                               (target "_blank")
                               (rel "noreferrer noopener"))
                            "Join Discord")))))
           (callout
            'note
            "WebRacket note"
            `(p "For WebRacket topics, start a thread on Discourse or ask in Discord. "
                "Include a link to the demo, code snippet, and browser details when reporting issues.")))
          #f
          #f)
        ,(footer-section)))

;; init-dom : -> Void
