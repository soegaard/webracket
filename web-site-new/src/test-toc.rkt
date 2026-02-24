;; Minimal TOC test harness to debug class application without building the full site.

;; element-class-string: any/c -> string?
;;   Extract className if present; otherwise fall back to the class attribute.
(define (element-class-string element)
  (define class-name (and element (js-ref element "className")))
  (cond
    [(string? class-name) class-name]
    [else
     (define attr (and element (js-get-attribute element "class")))
     (if (string? attr) attr "")]))

;; classlist-add!: any/c string? -> void?
;;   Adds a CSS class via classList when available, otherwise updates className.
(define (classlist-add! element class-name)
  (define class-list (and element (js-ref element "classList")))
  (js-log (format "classlist-add class-list? ~a" (if class-list "yes" "no")))
  (cond
    [class-list
     (when (string? class-name)
       (js-send class-list "add" (vector class-name)))]
    [else
     (define current (element-class-string element))
     (define tokens (if (string? current) (string-split current) '()))
     (when (and (string? class-name) (not (member class-name tokens)))
       (define next (append tokens (list class-name)))
       (js-set! element "className" (string-join next " ")))]))

;; classlist-remove!: any/c string? -> void?
;;   Removes a CSS class via classList when available, otherwise updates className.
(define (classlist-remove! element class-name)
  (define class-list (and element (js-ref element "classList")))
  (js-log (format "classlist-remove class-list? ~a" (if class-list "yes" "no")))
  (cond
    [class-list
     (when (string? class-name)
       (js-send class-list "remove" (vector class-name)))]
    [else
     (define current (element-class-string element))
     (define tokens (if (string? current) (string-split current) '()))
     (define next (filter (lambda (t) (not (string=? t class-name))) tokens))
     (js-set! element "className" (string-join next " "))]))

;; build-test-dom!: -> (values element element (listof element) (listof element))
;;   Returns doc-root, content, headings, toc-links.
(define (build-test-dom!)
  (define body (js-document-body))
  (define doc-root (js-create-element "div"))
  (js-set-attribute! doc-root "class" "docs-article")
  (define content (js-create-element "div"))
  (js-set-attribute! content "class" "doc-prose")
  (define toc (js-create-element "aside"))
  (js-set-attribute! toc "class" "docs-toc")
  (define toc-list (js-create-element "div"))
  (js-set-attribute! toc-list "class" "toc-list")

  (define h1 (js-create-element "h2"))
  (js-set-attribute! h1 "id" "sec-one")
  (js-set! h1 "textContent" "Section One")
  (define h2 (js-create-element "h2"))
  (js-set-attribute! h2 "id" "sec-two")
  (js-set! h2 "textContent" "Section Two")
  (define headings (list h1 h2))

  (define link1 (js-create-element "a"))
  (js-set-attribute! link1 "class" "toc-link")
  (js-set-attribute! link1 "data-toc-link" "sec-one")
  (js-set! link1 "textContent" "Section One")
  (define link2 (js-create-element "a"))
  (js-set-attribute! link2 "class" "toc-link")
  (js-set-attribute! link2 "data-toc-link" "sec-two")
  (js-set! link2 "textContent" "Section Two")
  (define toc-links (list link1 link2))

  (js-append-child! toc-list link1)
  (js-append-child! toc-list link2)
  (js-append-child! toc toc-list)

  (js-append-child! content h1)
  (js-append-child! content h2)

  (js-append-child! doc-root content)
  (js-append-child! doc-root toc)
  (js-append-child! body doc-root)
  (values doc-root content headings toc-links))

(define (test-set-active!)
  (define-values (doc-root content headings toc-links) (build-test-dom!))
  (define (normalize-id v)
    (cond
      [(string? v) v]
      [(external? v) (external-string->string v)]
      [(symbol? v) (symbol->string v)]
      [else ""]))
  (define link-by-id (make-hasheq))
  (define (key-from-id v)
    (define s (normalize-id v))
    (if (string=? s "") #f (string->symbol s)))
  (for ([link (in-list toc-links)])
    (define raw-id (js-get-attribute link "data-toc-link"))
    (define dataset (js-ref link "dataset"))
    (define dataset-id (and dataset (js-ref dataset "tocLink")))
    (define id
      (let ([norm-raw (normalize-id raw-id)]
            [norm-ds (normalize-id dataset-id)])
        (if (string=? norm-raw "") norm-ds norm-raw)))
    (define key (key-from-id id))
    (js-log (format "insert raw=~a normalized=~a"
                    (if (string? raw-id) raw-id (if (symbol? raw-id) (symbol->string raw-id) ""))
                    id))
    (js-log (format "insert dataset=~a"
                    (if (string? dataset-id) dataset-id (if (symbol? dataset-id) (symbol->string dataset-id) ""))))
    (when key
      (hash-set! link-by-id key (list link)))
    (js-log (format "post-insert key=~a has-key? ~a string? ~a"
                    key
                    (hash-has-key? link-by-id key)
                    (string? id))))
  (define current-active #f)
  (define (set-active! id)
    (define key (key-from-id id))
    (js-log (format "set-active called id=~a" id))
    (js-log (format "lookup key=~a" key))
    (js-log (format "has-key? ~a" (and key (hash-has-key? link-by-id key))))
    (js-log (format "key symbol? ~a eq? ~a"
                    (symbol? key)
                    (and (symbol? key) (eq? key 'sec-one))))
    (when (and key (not (equal? key current-active)))
      (when (and current-active (hash-has-key? link-by-id current-active))
        (for ([link (in-list (hash-ref link-by-id current-active))])
          (classlist-remove! link "is-active")))
      (when (hash-has-key? link-by-id key)
        (for ([link (in-list (hash-ref link-by-id key))])
          (classlist-add! link "is-active")))
      (set! current-active key)))
  (js-log "before set-active")
  (set-active! "sec-one")
  (for ([link (in-list toc-links)])
    (js-log (format "link ~a className=~a"
                    (js-get-attribute link "data-toc-link")
                    (js-ref link "className"))))
  ;; Direct classList.add to compare behavior.
  (define first-link (car toc-links))
  (js-log (format "before classList.add: ~a" (js-ref first-link "className")))
  (define class-list (js-ref first-link "classList"))
  (when class-list
    (js-send class-list "add" (vector "is-active")))
  (js-log (format "after classList.add: ~a" (js-ref first-link "className"))))

(test-set-active!)
