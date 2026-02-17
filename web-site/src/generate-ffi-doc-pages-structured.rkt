#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/pretty
         racket/runtime-path
         racket/string)

(define-runtime-path here ".")
(define repo-root (simplify-path (build-path here ".." "..")))
(define generated-dir (build-path here "generated"))
(define generated-file (build-path generated-dir "ffi-doc-pages-structured.rkt"))
(define generated-include-file (build-path generated-dir "ffi-doc-pages-structured.inc.rkt"))

(define docs-manifest
  (list
   (list "ffi-standard" "DOCS-standard.md" "documentation-ffi-standard.html")
   (list "ffi-dom" "DOCS-dom.md" "documentation-ffi-dom.html")
   (list "ffi-js" "DOCS-js.md" "documentation-ffi-js.html")
   (list "ffi-math" "DOCS-math.md" "documentation-ffi-math.html")
   (list "ffi-jsxgraph" "DOCS-jsxgraph.md" "documentation-ffi-jsxgraph.html")
   (list "ffi-xtermjs" "DOCS-xtermjs.md" "documentation-ffi-xtermjs.html")))

(define warnings '())
(define (warn who msg)
  (set! warnings (cons (format "[~a] ~a" who msg) warnings)))

(define (find-char s ch start)
  (let loop ([i start])
    (cond
      [(>= i (string-length s)) #f]
      [(char=? (string-ref s i) ch) i]
      [else (loop (add1 i))])))

(define (append-inline-text out text)
  (cond
    [(string=? text "") out]
    [(and (pair? out) (string? (car out)))
     (cons (string-append (car out) text) (cdr out))]
    [else
     (cons text out)]))

(define (parse-inline-ast text)
  (define len (string-length text))
  (let loop ([i 0] [plain ""] [out '()])
    (cond
      [(>= i len)
       (reverse (append-inline-text out plain))]
      [else
       (define ch (string-ref text i))
       (cond
         [(char=? ch #\`)
          (define j (find-char text #\` (add1 i)))
          (if j
              (let ([out* (append-inline-text out plain)])
                (loop (add1 j) "" (cons `(code ,(substring text (add1 i) j)) out*)))
              (loop (add1 i) (string-append plain "`") out))]
         [(char=? ch #\[)
          (define close-bracket (find-char text #\] (add1 i)))
          (define open-paren (and close-bracket
                                  (< (add1 close-bracket) len)
                                  (char=? (string-ref text (add1 close-bracket)) #\()
                                  (add1 close-bracket)))
          (define close-paren (and open-paren (find-char text #\) (add1 open-paren))))
          (if (and close-bracket open-paren close-paren)
              (let* ([label (substring text (add1 i) close-bracket)]
                     [url (substring text (add1 open-paren) close-paren)]
                     [out* (append-inline-text out plain)])
                (loop (add1 close-paren) "" (cons `(link ,label ,url) out*)))
              (loop (add1 i) (string-append plain "[") out))]
         [else
          (loop (add1 i) (string-append plain (string ch)) out)])])))

(define (line-blank? s)
  (string=? "" (string-trim s)))

(define (heading-line? s)
  (regexp-match? #px"^#{1,6}\\s+" s))

(define (parse-table-lines who lines)
  (define (split-table-row row)
    (define s (string-trim row))
    (define body
      (cond
        [(and (positive? (string-length s))
              (char=? (string-ref s 0) #\|))
         (substring s 1)]
        [else s]))
    (define body*
      (cond
        [(and (positive? (string-length body))
              (char=? (string-ref body (sub1 (string-length body))) #\|))
         (substring body 0 (sub1 (string-length body)))]
        [else body]))
    (map string-trim (string-split body* "|" #:trim? #f)))

  (if (< (length lines) 2)
      (begin
        (warn who "table block had fewer than 2 rows; rendering as paragraph")
        `(p ,@(parse-inline-ast (string-join (map string-trim lines) " "))))
      (let* ([header-cells (split-table-row (first lines))]
             [divider-cells (split-table-row (second lines))]
             [rows (drop lines 2)])
        (unless (= (length header-cells) (length divider-cells))
          (warn who (format "table divider width mismatch (~a headers, ~a divider cells)"
                             (length header-cells)
                             (length divider-cells))))
        (define width (length header-cells))
        (define (normalize-cells cells)
          (cond
            [(= (length cells) width) cells]
            [(> (length cells) width) (take cells width)]
            [else (append cells (make-list (- width (length cells)) ""))]))
        `(table
          (header
           ,@(for/list ([cell (in-list header-cells)])
               `(cell ,@(parse-inline-ast cell))))
          ,@(for/list ([row (in-list rows)])
              (define cells (normalize-cells (split-table-row row)))
              `(row
                ,@(for/list ([cell (in-list cells)])
                    `(cell ,@(parse-inline-ast cell)))))))))

(define (parse-fenced-code-header line)
  (define trimmed (string-trim line))
  (define raw-lang (string-trim (substring trimmed 3)))
  (if (string=? raw-lang "") "" raw-lang))

(define (parse-markdown->ast who text slug source output)
  (define lines (string-split text "\n" #:trim? #f))
  (define title #f)

  (define (collect pred i)
    (let loop ([j i] [acc '()])
      (if (and (< j (length lines)) (pred (list-ref lines j)))
          (loop (add1 j) (cons (list-ref lines j) acc))
          (values j (reverse acc)))))

  (define blocks
    (let loop ([i 0] [acc '()])
      (cond
        [(>= i (length lines))
         (reverse acc)]
        [else
         (define line (list-ref lines i))
         (define trimmed (string-trim line))
         (cond
           [(line-blank? line)
            (loop (add1 i) acc)]
           [(regexp-match? #px"^```" line)
            (define lang (parse-fenced-code-header line))
            (let scan ([j (add1 i)] [code-lines '()])
              (cond
                [(>= j (length lines))
                 (warn who "unterminated fenced code block")
                 (loop j (cons `(code-block (lang ,lang) (text ,(string-join (reverse code-lines) "\n"))) acc))]
                [(regexp-match? #px"^```" (list-ref lines j))
                 (loop (add1 j)
                       (cons `(code-block (lang ,lang) (text ,(string-join (reverse code-lines) "\n"))) acc))]
                [else
                 (scan (add1 j) (cons (list-ref lines j) code-lines))]))]
           [(heading-line? line)
            (define m (regexp-match #px"^(#{1,6})\\s+(.+)$" line))
            (define level (string-length (second m)))
            (define heading-text (string-trim (third m)))
            (when (and (= level 1) (not title))
              (set! title (regexp-replace* #px"`" heading-text "")))
            (define tag
              (cond
                [(= level 1) 'h1]
                [(= level 2) 'h2]
                [(= level 3) 'h3]
                [(= level 4) 'h4]
                [(= level 5) 'h5]
                [else 'h6]))
            (loop (add1 i) (cons `(,tag ,@(parse-inline-ast heading-text)) acc))]
           [(string-prefix? line "|")
            (define-values (j table-lines)
              (collect (lambda (s) (string-prefix? s "|")) i))
            (loop j (cons (parse-table-lines who table-lines) acc))]
           [(regexp-match? #px"^[-*_]{3,}$" trimmed)
            (loop (add1 i) (cons '(hr) acc))]
           [(string-prefix? trimmed ">")
            (define-values (j quote-lines)
              (collect (lambda (s) (string-prefix? (string-trim s) ">")) i))
            (define quote-text
              (string-join
               (for/list ([ql (in-list quote-lines)])
                 (define qt (string-trim ql))
                 (string-trim (substring qt 1)))
               " "))
            (loop j (cons `(blockquote (p ,@(parse-inline-ast quote-text))) acc))]
           [(or (string-prefix? trimmed "- ")
                (string-prefix? trimmed "* "))
            (define-values (j list-lines)
              (collect
               (lambda (s)
                 (define t (string-trim s))
                 (or (string-prefix? t "- ")
                     (string-prefix? t "* ")))
               i))
            (define items
              (for/list ([l (in-list list-lines)])
                (define t (string-trim l))
                (define text (string-trim (substring t 2)))
                `(li ,@(parse-inline-ast text))))
            (loop j (cons `(ul ,@items) acc))]
           [(regexp-match? #px"^[0-9]+\\.\\s+" trimmed)
            (define-values (j list-lines)
              (collect
               (lambda (s)
                 (regexp-match? #px"^[0-9]+\\.\\s+" (string-trim s)))
               i))
            (define items
              (for/list ([l (in-list list-lines)])
                (define t (string-trim l))
                (define maybe-m (regexp-match #px"^[0-9]+\\.\\s+(.+)$" t))
                (define text (if maybe-m (cadr maybe-m) t))
                `(li ,@(parse-inline-ast text))))
            (loop j (cons `(ol ,@items) acc))]
           [else
            (define-values (j para-lines)
              (collect
               (lambda (s)
                 (define t (string-trim s))
                 (and (not (line-blank? s))
                      (not (regexp-match? #px"^```" s))
                      (not (heading-line? s))
                      (not (string-prefix? s "|"))
                      (not (regexp-match? #px"^[-*_]{3,}$" t))
                      (not (string-prefix? t ">"))
                      (not (string-prefix? t "- "))
                      (not (string-prefix? t "* "))
                      (not (regexp-match? #px"^[0-9]+\\.\\s+" t))))
               i))
            (define para-text (string-join (map string-trim para-lines) " "))
            (loop j (cons `(p ,@(parse-inline-ast para-text)) acc))])])))

  `(doc
    (meta
     (format "ffi-structured-markdown")
     (version 1)
     (slug ,slug)
     (title ,(or title "FFI Documentation"))
     (source ,source)
     (output ,output))
    ,@blocks))

(define (structured-pages-data pages)
  `(quote
    ,(for/list ([p (in-list pages)])
       (list (cons 'slug (hash-ref p 'slug))
             (cons 'title (hash-ref p 'title))
             (cons 'source (hash-ref p 'source))
             (cons 'output (hash-ref p 'output))))))

(define (structured-ast-string-data pages)
  `(quote
    ,(for/list ([p (in-list pages)])
       (cons (hash-ref p 'slug)
             (with-output-to-string
               (lambda () (write (hash-ref p 'ast))))))))

(define (structured-title-data pages)
  `(quote
    ,(for/list ([p (in-list pages)])
       (cons (hash-ref p 'slug) (hash-ref p 'title)))))

(define (write-generated-definitions out pages)
  (fprintf out "(define ffi-doc-structured-pages\n")
  (pretty-write (structured-pages-data pages) out)
  (fprintf out ")\n\n")

  (fprintf out "(define ffi-doc-structured-ast-string-alist\n")
  (pretty-write (structured-ast-string-data pages) out)
  (fprintf out ")\n\n")

  (fprintf out "(define ffi-doc-structured-title-alist\n")
  (pretty-write (structured-title-data pages) out)
  (fprintf out ")\n\n")

  (fprintf out "(define ffi-doc-structured-ast-cache (make-hash))\n\n")

  (fprintf out "(define (ffi-doc-structured-ast slug)\n")
  (fprintf out "  (hash-ref! ffi-doc-structured-ast-cache slug\n")
  (fprintf out "             (lambda ()\n")
  (fprintf out "               (define r (assoc slug ffi-doc-structured-ast-string-alist))\n")
  (fprintf out "               (and r (read (open-input-string (cdr r)))))))\n\n")

  (fprintf out "(define (ffi-doc-structured-title slug)\n")
  (fprintf out "  (define r (assoc slug ffi-doc-structured-title-alist))\n")
  (fprintf out "  (and r (cdr r)))\n"))

(define (render-generated-module pages)
  (call-with-output-file generated-file
    (lambda (out)
      (fprintf out "#lang racket/base\n\n")
      (fprintf out "(provide ffi-doc-structured-pages ffi-doc-structured-ast ffi-doc-structured-title)\n\n")
      (write-generated-definitions out pages))
    #:exists 'replace)
  (call-with-output-file generated-include-file
    (lambda (out)
      (fprintf out ";; Auto-generated by generate-ffi-doc-pages-structured.rkt. Do not edit by hand.\n\n")
      (write-generated-definitions out pages))
    #:exists 'replace))

(define (main)
  (make-directory* generated-dir)
  (define pages
    (for/list ([entry (in-list docs-manifest)])
      (match-define (list slug file output) entry)
      (define input-path (build-path repo-root file))
      (unless (file-exists? input-path)
        (error 'generate-ffi-doc-pages-structured (format "missing input file: ~a" input-path)))
      (define text (file->string input-path))
      (define ast (parse-markdown->ast file text slug file output))
      (define title
        (match ast
          [`(doc (meta (format ,_) (version ,_) (slug ,_) (title ,t) (source ,_) (output ,_)) . ,_) t]
          [_ "FFI Documentation"]))
      (hash 'slug slug
            'title title
            'source file
            'output output
            'ast ast)))

  (render-generated-module pages)

  (printf "Generated ~a and ~a (~a docs).\n"
          (path->string generated-file)
          (path->string generated-include-file)
          (length pages))
  (if (null? warnings)
      (printf "No warnings.\n")
      (begin
        (printf "Warnings (~a):\n" (length warnings))
        (for ([w (in-list (reverse warnings))])
          (printf "- ~a\n" w))))
  (void))

(module+ main
  (main))
