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
(define generated-file (build-path generated-dir "ffi-doc-pages.rkt"))
(define generated-include-file (build-path generated-dir "ffi-doc-pages.inc.rkt"))

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

(define (find-substr s pat start)
  (define n (string-length s))
  (define m (string-length pat))
  (let loop ([i start])
    (cond
      [(> (+ i m) n) #f]
      [(string=? (substring s i (+ i m)) pat) i]
      [else (loop (add1 i))])))

(define (append-text out text)
  (if (string=? text "")
      out
      (cons text out)))

(define (parse-inline text)
  (define len (string-length text))
  (let loop ([i 0] [plain ""] [out '()])
    (cond
      [(>= i len)
       (reverse (append-text out plain))]
      [else
       (define ch (string-ref text i))
       (cond
         [(char=? ch #\`)
          (define j (find-char text #\` (add1 i)))
          (define-values (next-i next-plain next-out)
            (if j
                (values (add1 j)
                        ""
                        (cons `(code ,(substring text (add1 i) j))
                              (append-text out plain)))
                (values (add1 i)
                        (string-append plain "`")
                        out)))
          (loop next-i next-plain next-out)]
         [(char=? ch #\[)
          (define close-bracket (find-char text #\] (add1 i)))
          (define open-paren (and close-bracket
                                  (< (add1 close-bracket) len)
                                  (char=? (string-ref text (add1 close-bracket)) #\()
                                  (add1 close-bracket)))
          (define close-paren (and open-paren
                                   (find-char text #\) (add1 open-paren))))
          (if (and close-bracket open-paren close-paren)
              (let* ([label (substring text (add1 i) close-bracket)]
                     [url (substring text (add1 open-paren) close-paren)]
                     [out* (append-text out plain)])
                (loop (add1 close-paren)
                      ""
                      (cons `(a (@ (href ,url)) ,label) out*)))
              (loop (add1 i) (string-append plain "[") out))]
         [else
          (loop (add1 i) (string-append plain (string ch)) out)])])))

(define (line-blank? s)
  (string=? "" (string-trim s)))

(define (heading-line? s)
  (regexp-match? #px"^#{1,6}\\s+" s))

(define (trim-hyphens s)
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

(define (slugify text slug-counts)
  (define raw (string-downcase text))
  (define cleaned
    (list->string
     (for/list ([ch (in-string raw)]
                #:when (or (char-alphabetic? ch)
                           (char-numeric? ch)
                           (char=? ch #\space)
                           (char=? ch #\-)))
       ch)))
  (define normalized (string-replace cleaned " " "-"))
  (define trimmed (trim-hyphens normalized))
  (define base (if (string=? trimmed "") "section" trimmed))
  (define n (hash-ref slug-counts base 0))
  (hash-set! slug-counts base (add1 n))
  (if (= n 0) base (format "~a-~a" base (add1 n))))

(define (split-table-row row)
  (define s (string-trim row))
  (define body
    (cond
      [(and (positive? (string-length s)) (char=? (string-ref s 0) #\|))
       (substring s 1)]
      [else s]))
  (define body*
    (cond
      [(and (positive? (string-length body))
            (char=? (string-ref body (sub1 (string-length body))) #\|))
       (substring body 0 (sub1 (string-length body)))]
      [else body]))
  (map string-trim (string-split body* "|" #:trim? #f)))

(define (make-table-xexpr who lines)
  (if (< (length lines) 2)
      (begin
        (warn who "table block had fewer than 2 rows; rendering as paragraph")
        `(p ,(string-join (map string-trim lines) " ")))
      (let* ([header-cells (split-table-row (first lines))]
             [divider-cells (split-table-row (second lines))]
             [rows (drop lines 2)])
        (unless (= (length header-cells) (length divider-cells))
          (warn who (format "table divider width mismatch (~a headers, ~a divider cells)"
                             (length header-cells)
                             (length divider-cells))))
        `(div (@ (class "doc-table-wrap"))
              (table
               (thead
                (tr
                 ,@(for/list ([cell (in-list header-cells)])
                     `(th ,@(parse-inline cell)))))
               (tbody
                ,@(for/list ([row (in-list rows)])
                    (define cells (split-table-row row))
                    `(tr
                      ,@(for/list ([cell (in-list cells)])
                          `(td ,@(parse-inline cell)))))))))))

(define (parse-markdown who text)
  (define lines (string-split text "\n" #:trim? #f))
  (define slug-counts (make-hash))
  (define title #f)

  (define (collect pred i)
    (let loop ([j i] [acc '()])
      (if (and (< j (length lines)) (pred (list-ref lines j)))
          (loop (add1 j) (cons (list-ref lines j) acc))
          (values j (reverse acc)))))

  (let loop ([i 0] [blocks '()])
    (cond
      [(>= i (length lines))
       (values (or title "FFI Documentation") (reverse blocks))]
      [else
       (define line (list-ref lines i))
       (cond
         [(line-blank? line)
          (loop (add1 i) blocks)]

         [(regexp-match? #px"^```" line)
          (let scan ([j (add1 i)] [acc '()])
            (cond
              [(>= j (length lines))
               (warn who "unterminated fenced code block")
               (loop j (cons `(pre (code ,(string-join (reverse acc) "\n"))) blocks))]
              [(regexp-match? #px"^```" (list-ref lines j))
               (loop (add1 j)
                     (cons `(pre (code ,(string-join (reverse acc) "\n"))) blocks))]
              [else
               (scan (add1 j) (cons (list-ref lines j) acc))]))]

         [(heading-line? line)
          (define m (regexp-match #px"^(#{1,6})\\s+(.+)$" line))
          (define level (string-length (second m)))
          (define text (string-trim (third m)))
          (when (and (= level 1) (not title))
            (set! title (regexp-replace* #px"`" text "")))
          (define id (slugify text slug-counts))
          (define tag
            (cond
              [(= level 1) 'h1]
              [(= level 2) 'h2]
              [(= level 3) 'h3]
              [(= level 4) 'h4]
              [else 'h5]))
          (loop (add1 i)
                (cons `(,tag (@ (id ,id)) ,@(parse-inline text)) blocks))]

         [(string-prefix? line "|")
          (define-values (j table-lines)
            (collect (lambda (s) (string-prefix? s "|")) i))
          (loop j (cons (make-table-xexpr who table-lines) blocks))]

         [(string-prefix? (string-trim line) "- ")
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
              `(li ,@(parse-inline text))))
          (loop j (cons `(ul ,@items) blocks))]

         [else
          (define-values (j para-lines)
            (collect
             (lambda (s)
               (and (not (line-blank? s))
                    (not (regexp-match? #px"^```" s))
                    (not (heading-line? s))
                    (not (string-prefix? s "|"))
                    (not (string-prefix? (string-trim s) "- "))
                    (not (string-prefix? (string-trim s) "* "))))
             i))
          (define para-text (string-join (map string-trim para-lines) " "))
          (loop j (cons `(p ,@(parse-inline para-text)) blocks))])])))

(define (ffi-pages-data pages)
  `(quote
    ,(for/list ([p (in-list pages)])
       (list (cons 'slug (hash-ref p 'slug))
             (cons 'title (hash-ref p 'title))
             (cons 'source (hash-ref p 'source))
             (cons 'output (hash-ref p 'output))))))

(define (ffi-body-data pages)
  `(quote
    ,(for/list ([p (in-list pages)])
       (cons (hash-ref p 'slug) (hash-ref p 'body)))))

(define (ffi-body-string-data pages)
  `(quote
    ,(for/list ([p (in-list pages)])
       (cons (hash-ref p 'slug)
             (with-output-to-string
               (lambda () (write (hash-ref p 'body))))))))

(define (ffi-title-data pages)
  `(quote
    ,(for/list ([p (in-list pages)])
       (cons (hash-ref p 'slug) (hash-ref p 'title)))))

(define (write-generated-definitions out pages)
  (fprintf out "(define ffi-doc-pages\n")
  (pretty-write (ffi-pages-data pages) out)
  (fprintf out ")\n\n")

  (fprintf out "(define ffi-doc-body-string-alist\n")
  (pretty-write (ffi-body-string-data pages) out)
  (fprintf out ")\n\n")

  (fprintf out "(define ffi-doc-title-alist\n")
  (pretty-write (ffi-title-data pages) out)
  (fprintf out ")\n\n")

  (fprintf out "(define ffi-doc-body-cache (make-hash))\n\n")

  (fprintf out "(define (ffi-doc-body slug)\n")
  (fprintf out "  (hash-ref! ffi-doc-body-cache slug\n")
  (fprintf out "             (lambda ()\n")
  (fprintf out "               (define r (assoc slug ffi-doc-body-string-alist))\n")
  (fprintf out "               (and r (read (open-input-string (cdr r)))))))\n\n")

  (fprintf out "(define (ffi-doc-title slug)\n")
  (fprintf out "  (define r (assoc slug ffi-doc-title-alist))\n")
  (fprintf out "  (and r (cdr r)))\n"))

(define (render-generated-module pages)
  (call-with-output-file generated-file
    (lambda (out)
      (fprintf out "#lang racket/base\n\n")
      (fprintf out "(provide ffi-doc-pages ffi-doc-body ffi-doc-title)\n\n")
      (write-generated-definitions out pages))
    #:exists 'replace)
  (call-with-output-file generated-include-file
    (lambda (out)
      (fprintf out ";; Auto-generated by generate-ffi-doc-pages.rkt. Do not edit by hand.\n\n")
      (write-generated-definitions out pages))
    #:exists 'replace))

(define (main)
  (make-directory* generated-dir)
  (define pages
    (for/list ([entry (in-list docs-manifest)])
      (match-define (list slug file output) entry)
      (define input-path (build-path repo-root file))
      (unless (file-exists? input-path)
        (error 'generate-ffi-doc-pages (format "missing input file: ~a" input-path)))
      (define text (file->string input-path))
      (define-values (title body) (parse-markdown file text))
      (hash 'slug slug
            'title title
            'source file
            'output output
            'body body)))

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
