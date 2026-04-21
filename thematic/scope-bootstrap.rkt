#lang racket/base

;; thematic-scope : command-line -> void
;;   Scope a Bootstrap stylesheet under one wrapper selector for side-by-side previews.

(provide main)

(require lexers/css
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/string)

(define recursive-at-rules
  '("@media" "@supports" "@layer" "@container" "@document"))

(define raw-block-at-rules
  '("@font-face" "@keyframes" "@-webkit-keyframes" "@property"))

(define (fail fmt . args)
  (raise-user-error 'thematic-scope (apply format fmt args)))

;; token-text : css-derived-token? -> string?
;;   Extract the original token text.
(define (token-text token)
  (css-derived-token-text token))

;; token-count : vector? -> integer?
;;   Return number of token entries.
(define (token-count tokens)
  (vector-length tokens))

;; token-ref-text : vector? integer? -> string?
;;   Read token text at index i.
(define (token-ref-text tokens i)
  (token-text (vector-ref tokens i)))

;; update-delimiter-depths : string? integer? integer? integer? -> (values integer? integer? integer?)
;;   Update grouping depths based on delimiter text.
(define (update-delimiter-depths text paren-depth bracket-depth brace-depth)
  (cond
    [(string=? text "(")
     (values (add1 paren-depth) bracket-depth brace-depth)]
    [(string=? text ")")
     (values (max 0 (sub1 paren-depth)) bracket-depth brace-depth)]
    [(string=? text "[")
     (values paren-depth (add1 bracket-depth) brace-depth)]
    [(string=? text "]")
     (values paren-depth (max 0 (sub1 bracket-depth)) brace-depth)]
    [(string=? text "{")
     (values paren-depth bracket-depth (add1 brace-depth))]
    [(string=? text "}")
     (values paren-depth bracket-depth (max 0 (sub1 brace-depth)))]
    [else
     (values paren-depth bracket-depth brace-depth)]))

;; trim-selector : string? -> string?
;;   Normalize selector whitespace around the edges.
(define (trim-selector s)
  (string-trim s))

;; split-selector-list : string? -> (listof string?)
;;   Split a selector list on top-level commas.
(define (split-selector-list selector-text)
  (define len (string-length selector-text))
  (define parts '())
  (define start 0)
  (define paren-depth 0)
  (define bracket-depth 0)
  (define brace-depth 0)
  (for ([i (in-range len)])
    (define ch (string-ref selector-text i))
    (cond
      [(char=? ch #\()
       (set! paren-depth (add1 paren-depth))]
      [(char=? ch #\))
       (set! paren-depth (max 0 (sub1 paren-depth)))]
      [(char=? ch #\[)
       (set! bracket-depth (add1 bracket-depth))]
      [(char=? ch #\])
       (set! bracket-depth (max 0 (sub1 bracket-depth)))]
      [(char=? ch #\{)
       (set! brace-depth (add1 brace-depth))]
      [(char=? ch #\})
       (set! brace-depth (max 0 (sub1 brace-depth)))]
      [(and (char=? ch #\,)
            (zero? paren-depth)
            (zero? bracket-depth)
            (zero? brace-depth))
       (set! parts (cons (substring selector-text start i) parts))
       (set! start (add1 i))]))
  (reverse (cons (substring selector-text start len) parts)))

;; selector-already-scoped? : string? string? -> boolean?
;;   Detect whether selector already begins with scope.
(define (selector-already-scoped? selector-text scope)
  (string-prefix? (trim-selector selector-text) scope))

;; replace-leading-root-ish : string? string? -> string?
;;   Replace a leading :root/html/body token with scope when present.
(define (replace-leading-root-ish selector-text scope)
  (define s (trim-selector selector-text))
  (cond
    [(regexp-match? #px"^:root($|[#.[:space:]>+~\\[])" s)
     (regexp-replace #px"^:root" s scope)]
    [(regexp-match? #px"^html($|[#.[:space:]>+~\\[])" s)
     (regexp-replace #px"^html" s scope)]
    [(regexp-match? #px"^body($|[#.[:space:]>+~\\[])" s)
     (regexp-replace #px"^body" s scope)]
    [else
     #f]))

;; scope-one-selector : string? string? -> string?
;;   Scope one selector clause under scope.
(define (scope-one-selector selector-text scope)
  (define s (trim-selector selector-text))
  (cond
    [(string=? s "")
     s]
    [(selector-already-scoped? s scope)
     s]
    [else
     (or (replace-leading-root-ish s scope)
         (string-append scope " " s))]))

;; scope-selector-prelude : string? string? -> string?
;;   Scope a full selector list under scope.
(define (scope-selector-prelude selector-text scope)
  (string-join
   (for/list ([part (in-list (split-selector-list selector-text))])
     (scope-one-selector part scope))
   ", "))

;; at-rule-name : string? -> (or/c string? #f)
;;   Extract a lowercase at-rule name from prelude text.
(define (at-rule-name prelude-text)
  (define m (regexp-match #px"^\\s*(@[-_a-zA-Z0-9]+)" prelude-text))
  (and m (string-downcase (list-ref m 1))))

;; collect-balanced-block : vector? integer? -> (values string? integer?)
;;   Collect raw token text until the matching closing brace, excluding the closing brace.
(define (collect-balanced-block tokens start-index)
  (let loop ([i start-index] [parts '()] [paren-depth 0] [bracket-depth 0] [brace-depth 1])
    (when (>= i (token-count tokens))
      (fail "unterminated CSS block"))
    (define text (token-ref-text tokens i))
    (if (and (string=? text "}")
             (zero? paren-depth)
             (zero? bracket-depth)
             (= brace-depth 1))
        (values (apply string-append (reverse parts)) (add1 i))
        (let-values ([(new-paren new-bracket new-brace)
                      (update-delimiter-depths text paren-depth bracket-depth brace-depth)])
          (loop (add1 i)
                (cons text parts)
                new-paren
                new-bracket
                new-brace)))))

;; parse-statement-list : vector? integer? boolean? string? -> (values string? integer?)
;;   Parse and scope a statement list until EOF or a matching close brace.
(define (parse-statement-list tokens start-index stop-on-close? scope)
  (let loop ([i start-index] [parts '()])
    (cond
      [(>= i (token-count tokens))
       (values (apply string-append (reverse parts)) i)]
      [stop-on-close?
       (let scan-close ([j i] [trivia '()])
         (cond
           [(>= j (token-count tokens))
            (fail "unterminated nested at-rule block")]
           [(string=? (token-ref-text tokens j) "}")
            (values (apply string-append
                           (reverse (append trivia parts)))
                    (add1 j))]
           [(string=? (string-trim (token-ref-text tokens j)) "")
            (scan-close (add1 j) (cons (token-ref-text tokens j) trivia))]
           [else
            (define-values (statement-text next-index)
              (parse-one-statement tokens i stop-on-close? scope))
            (loop next-index (cons statement-text parts))]))]
      [else
       (define-values (statement-text next-index)
         (parse-one-statement tokens i stop-on-close? scope))
       (loop next-index (cons statement-text parts))])))

;; parse-one-statement : vector? integer? boolean? string? -> (values string? integer?)
;;   Parse one top-level or nested statement and scope it when appropriate.
(define (parse-one-statement tokens start-index stop-on-close? scope)
  (let loop ([i start-index] [parts '()] [paren-depth 0] [bracket-depth 0])
    (if (>= i (token-count tokens))
        (values (apply string-append (reverse parts)) i)
        (let ([text (token-ref-text tokens i)])
          (cond
            [(and stop-on-close? (null? parts) (string=? text "}"))
             (values "" i)]
            [(and (string=? text ";") (zero? paren-depth) (zero? bracket-depth))
             (values (apply string-append (reverse (cons text parts))) (add1 i))]
            [(and (string=? text "{") (zero? paren-depth) (zero? bracket-depth))
             (let* ([prelude (apply string-append (reverse parts))]
                    [name (at-rule-name prelude)])
               (if name
                   (if (member name recursive-at-rules)
                       (let-values ([(inner-text next-index)
                                     (parse-statement-list tokens (add1 i) #t scope)])
                         (values (string-append prelude "{" inner-text "}") next-index))
                       (let-values ([(raw-body next-index)
                                     (collect-balanced-block tokens (add1 i))])
                         (values (string-append prelude "{" raw-body "}") next-index)))
                   (let-values ([(raw-body next-index)
                                 (collect-balanced-block tokens (add1 i))])
                     (values (string-append (scope-selector-prelude prelude scope)
                                            "{"
                                            raw-body
                                            "}")
                             next-index))))]
            [else
             (let-values ([(new-paren new-bracket _)
                           (update-delimiter-depths text paren-depth bracket-depth 0)])
               (loop (add1 i) (cons text parts) new-paren new-bracket))])))))

;; scope-bootstrap-css : string? string? -> string?
;;   Scope a Bootstrap stylesheet under scope.
(define (scope-bootstrap-css source scope)
  (define tokens (list->vector (css-string->derived-tokens source)))
  (define-values (scoped-text _) (parse-statement-list tokens 0 #f scope))
  scoped-text)

;; write-scoped-bootstrap! : path-string? path-string? string? -> void
;;   Read input CSS, scope it, and write the result.
(define (write-scoped-bootstrap! input-path output-path scope)
  (define source (file->string input-path))
  (call-with-output-file output-path
    (lambda (out)
      (display (scope-bootstrap-css source scope) out))
    #:exists 'truncate/replace))

;; main : -> void
;;   Parse command-line arguments and run the scoper.
(define (main)
  (define input-path #f)
  (define output-path #f)
  (define scope ".bs-preview")
  (command-line
   #:program "thematic-scope"
   #:once-each
   [("--input") path "Path to the Bootstrap CSS file."
                (set! input-path path)]
   [("--output") path "Path to the scoped CSS file."
                 (set! output-path path)]
   [("--scope") selector-text "Wrapper selector to scope Bootstrap under."
                (set! scope selector-text)])
  (unless input-path
    (fail "missing required --input path"))
  (unless output-path
    (fail "missing required --output path"))
  (write-scoped-bootstrap! input-path output-path scope))

(module+ main
  (main))

(module+ test
  (require rackunit)

  (define sample-css
    (string-append
     "@import url(\"theme.css\");\n"
     ":root { --bs-body-bg: #fff; }\n"
     "body > .btn, .nav .item:hover { color: red; }\n"
     "@media (min-width: 700px) {\n"
     "  .btn { color: blue; }\n"
     "  :root { --bs-body-bg: #eee; }\n"
     "}\n"
     "@keyframes spin { from { opacity: 0; } to { opacity: 1; } }\n"))

  (test-case "scope-bootstrap-css scopes root, body, and nested media rules"
    (define css (scope-bootstrap-css sample-css ".bs-preview"))
    (check-true (regexp-match? #px"@import url\\(\"theme\\.css\"\\);" css))
    (check-true (regexp-match? #px"\\.bs-preview\\s*\\{\\s*--bs-body-bg: #fff;\\s*\\}" css))
    (check-true (regexp-match? #px"\\.bs-preview > \\.btn, \\.bs-preview \\.nav \\.item:hover\\s*\\{\\s*color: red;\\s*\\}" css))
    (check-true (regexp-match? #px"@media \\(min-width: 700px\\) \\{" css))
    (check-true (regexp-match? #px"\\.bs-preview \\.btn\\s*\\{\\s*color: blue;\\s*\\}" css))
    (check-true (regexp-match? #px"\\.bs-preview\\s*\\{\\s*--bs-body-bg: #eee;\\s*\\}" css))
    (check-true (regexp-match? #px"@keyframes spin\\s*\\{\\s*from \\{ opacity: 0; \\} to \\{ opacity: 1; \\}\\s*\\}" css))))
