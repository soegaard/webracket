;;;
;;; MiniScheme page (WebRacket site page)
;;;

(define minischeme-sample-program
  (string-append
   "(define (fact n)\n"
   "  (if (= n 0)\n"
   "      1\n"
   "      (* n (fact (- n 1)))))\n"
   "(fact 6)\n"))

(define minischeme-codemirror-css-url
  "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.16/codemirror.min.css")
(define minischeme-codemirror-js-url
  "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.16/codemirror.min.js")
(define minischeme-codemirror-scheme-mode-js-url
  "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.16/mode/scheme/scheme.min.js")
(define minischeme-codemirror-matchbrackets-js-url
  "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.16/addon/edit/matchbrackets.min.js")
(define minischeme-codemirror-closebrackets-js-url
  "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.16/addon/edit/closebrackets.min.js")
(define minischeme-storage-key
  "webracket:minischeme:source")

(define (minischeme-page)
  `(div (@ (class "page page--minischeme"))
        ,(navbar)
        (section (@ (class "mathjax-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "Interpreter")
                           (span (@ (class "pill")) "REPL")
                           (span (@ (class "pill")) "DOM + JS FFI"))
                      (h1 (@ (class "hero-title")) "MiniScheme")
                      (p (@ (class "hero-lead"))
                         "A tiny Scheme interpreter running entirely in the browser.")))
        (section (@ (class "section section--mathjax"))
                 (div (@ (class "section-content"))
                      (div (@ (class "minischeme-shell")
                              (style ,(string-append
                                       "display: grid; gap: 12px; max-width: 920px; margin: 0 auto; "
                                       "padding: 22px; border-radius: 14px; "
                                       "background: rgba(13, 15, 29, 0.74); "
                                       "border: 1px solid rgba(119, 141, 214, 0.25); "
                                       "box-shadow: 0 22px 48px rgba(10, 12, 25, 0.5);")))
                           (p (@ (style "margin: 0; color: rgba(213, 223, 255, 0.85);"))
                              "Type one or more expressions, then click "
                              (code "Run")
                              ". Definitions persist until reset.")
                           (textarea
                            (@ (id "minischeme-input")
                               (rows "12")
                               (spellcheck "false")
                               (style ,(string-append
                                        "display: block; width: 100%; height: 320px; min-height: 320px; resize: vertical; "
                                        "font-family: \"Iosevka\", \"Fira Code\", ui-monospace, monospace; "
                                        "font-size: 0.96rem; line-height: 1.45; "
                                        "padding: 12px; border-radius: 10px; "
                                        "background: #0A0C18; color: #EEF2FF; "
                                        "border: 1px solid rgba(120, 140, 220, 0.35);")))
                            ,minischeme-sample-program)
                           (div (@ (style "display: flex; gap: 10px; flex-wrap: wrap;"))
                                (button (@ (id "minischeme-run")
                                           (type "button")
                                           (style ,(string-append
                                                    "padding: 8px 14px; border-radius: 999px; border: none; "
                                                    "font-weight: 600; cursor: pointer; "
                                                    "background: #5E7BFF; color: white;")))
                                        "Run")
                                (button (@ (id "minischeme-load-sample")
                                           (type "button")
                                           (style ,(string-append
                                                    "padding: 8px 14px; border-radius: 999px; border: none; "
                                                    "font-weight: 600; cursor: pointer; "
                                                    "background: #20A085; color: white;")))
                                        "Load Sample")
                                (button (@ (id "minischeme-reset")
                                           (type "button")
                                           (style ,(string-append
                                                    "padding: 8px 14px; border-radius: 999px; border: none; "
                                                    "font-weight: 600; cursor: pointer; "
                                                    "background: #E06A5F; color: white;")))
                                        "Reset State"))
                           (pre (@ (id "minischeme-output")
                                   (style ,(string-append
                                            "margin: 0; min-height: 96px; white-space: pre-wrap; word-break: break-word; "
                                            "padding: 12px; border-radius: 10px; "
                                            "font-family: \"Iosevka\", \"Fira Code\", ui-monospace, monospace; "
                                            "background: #070914; color: #DCE5FF; "
                                            "border: 1px solid rgba(120, 140, 220, 0.26);")))
                                "MiniScheme ready."))))
        (section (@ (class "section section--mathjax-details"))
                 (div (@ (class "section-content"))
                      (div (@ (class "mathjax-details"))
                           (p "This page ports the existing MiniScheme example into the web-site shell.")
                           (div (@ (class "mathjax-actions"))
                                ,(code-pill (gh-file "examples/minischeme/interpreter.rkt")
                                            "Original interpreter")
                                ,(code-pill (gh-file "web-site/src/examples/minischeme/minischeme.rkt")
                                            "Web-site interpreter")
                                ,(code-pill (gh-file "web-site/src/examples/minischeme-page.rkt")
                                            "Page shell")))))
        ,(footer-section)))

(define (js-true? v)
  (cond
    [(boolean? v) v]
    [(not v) #f]
    [(external? v)
     (with-handlers ([exn:fail? (λ (_) #f)])
       (string=? (js-value->string v) "true"))]
    [else #t]))

(define (js-number-value v [default 0.0])
  (cond
    [(number? v) v]
    [(external? v)
     (with-handlers ([exn:fail? (λ (_) default)])
       (external-number->flonum v))]
    [else default]))

(include "minischeme/minischeme.rkt")

(define minischeme-page-started? #f)
(define minischeme-run-handler   #f)
(define minischeme-reset-handler #f)
(define minischeme-load-handler  #f)
(define minischeme-input-handler #f)
(define minischeme-editor-key-handler #f)
(define minischeme-editor-change-handler #f)
(define minischeme-editor #f)
(define minischeme-keyword-table (make-hasheq))
(define minischeme-primitive-table (make-hasheq))

(define (minischeme-codemirror-ready?)
  (not (js-nullish? (js-var "CodeMirror"))))

(define (minischeme-codemirror-scheme-mode-ready?)
  (and (minischeme-codemirror-ready?)
       (let* ([codemirror (js-var "CodeMirror")]
              [modes (js-ref codemirror "modes")]
              [scheme-mode (and (not (js-nullish? modes))
                                (js-ref modes "scheme"))])
         (not (js-nullish? scheme-mode)))))

(define (minischeme-codemirror-option-handler-ready? option-name)
  (and (minischeme-codemirror-ready?)
       (let* ([codemirror (js-var "CodeMirror")]
              [handlers (js-ref codemirror "optionHandlers")]
              [handler (and (not (js-nullish? handlers))
                            (js-ref handlers option-name))])
         (not (js-nullish? handler)))))

(define (minischeme-codemirror-matchbrackets-ready?)
  (minischeme-codemirror-option-handler-ready? "matchBrackets"))

(define (minischeme-codemirror-closebrackets-ready?)
  (minischeme-codemirror-option-handler-ready? "autoCloseBrackets"))

(define (minischeme-load-saved-source)
  (define storage (js-ref (js-var "window") "localStorage"))
  (if (js-nullish? storage)
      #f
      (let ([saved (js-send/extern storage "getItem" (vector minischeme-storage-key))])
        (if (js-nullish? saved)
            #f
            (js-value->string saved)))))

(define (minischeme-save-source source)
  (define storage (js-ref (js-var "window") "localStorage"))
  (unless (js-nullish? storage)
    (js-send storage "setItem" (vector minischeme-storage-key source))))

(define (minischeme-restore-source! input-node)
  (define saved (minischeme-load-saved-source))
  (when (and saved (not (string=? saved "")))
    (js-set! input-node "value" saved)))

(define (minischeme-editor-get-source input-node)
  (if minischeme-editor
      (js-value->string (js-send/extern minischeme-editor "getValue" (vector)))
      (js-value->string (js-ref input-node "value"))))

(define (minischeme-editor-set-source! input-node source)
  (if minischeme-editor
      (js-send minischeme-editor "setValue" (vector source))
      (js-set! input-node "value" source))
  (minischeme-save-source source))

(define (minischeme-refresh-highlight-tables!)
  (set! minischeme-keyword-table (make-hasheq))
  (set! minischeme-primitive-table (make-hasheq))
  (for-each (λ (name) (hash-set! minischeme-keyword-table name #t))
            (minischeme-keywords))
  (for-each (λ (name) (hash-set! minischeme-primitive-table name #t))
            (minischeme-primitives)))

(define (minischeme-js-string-escape s)
  (define s1 (string-replace s "\\" "\\\\"))
  (define s2 (string-replace s1 "\"" "\\\""))
  (define s3 (string-replace s2 "\n" "\\n"))
  (define s4 (string-replace s3 "\r" "\\r"))
  (string-replace s4 "\t" "\\t"))

(define (minischeme-symbols->js-array syms)
  (string-append
   "["
   (string-join
    (map (λ (sym)
           (string-append "\"" (minischeme-js-string-escape (symbol->string sym)) "\""))
         syms)
    ",")
   "]"))

(define (minischeme-build-js-overlay!)
  ;; Ensure keyword/primitive caches are initialized before generating JS sets.
  (when (and (null? (minischeme-keywords))
             (null? (minischeme-primitives)))
    (minischeme-reset-state!))
  (define forms-js (minischeme-symbols->js-array (minischeme-keywords)))
  (define prims-js (minischeme-symbols->js-array (minischeme-primitives)))
  (define js-source
    (string-append
     "(function(){"
     "const formSet = new Set(" forms-js ");"
     "const primSet = new Set(" prims-js ");"
     "const stringRe = /\\\"(?:[^\\\"\\\\\\\\]|\\\\\\\\.)*\\\"?/;"
     "const symbolRe = /[^\\\\s()\\\\[\\\\]{}\\\"';`,.]+/;"
     "let logged = false;"
     "window.__minischemeOverlay = {"
     " token: function(stream){"
     "   if (stream.eatSpace()) return null;"
     "   if (stream.match(';', false, false)) { stream.skipToEnd(); return null; }"
     "   if (stream.match(stringRe, true, false)) return null;"
     "   if (stream.match(symbolRe, true, false)) {"
     "     const tok = stream.current();"
     "     if (!logged) { logged = true; console.log('[minischeme] overlay active'); }"
     "     if (formSet.has(tok)) return 'keyword';"
     "     if (primSet.has(tok)) return 'builtin';"
     "     return null;"
     "   }"
     "   stream.next();"
     "   return null;"
     " }"
     "};"
     "})();"))
  (js-eval js-source)
  (js-ref (js-var "window") "__minischemeOverlay"))

(define (minischeme-token-style token)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (define sym (string->symbol token))
    (cond
      [(hash-has-key? minischeme-keyword-table sym) "ms-form"]
      [(hash-has-key? minischeme-primitive-table sym) "ms-builtin"]
      [else #f])))

(define (minischeme-overlay-delimiter? c)
  (or (char-whitespace? c)
      (char=? c #\()
      (char=? c #\))
      (char=? c #\[)
      (char=? c #\])
      (char=? c #\{)
      (char=? c #\})
      (char=? c #\")
      (char=? c #\')
      (char=? c #\`)
      (char=? c #\,)
      (char=? c #\;)
      (char=? c #\.)))

(define (minischeme-stream-peek-char stream)
  (define raw (js-send/extern stream "peek" (vector)))
  (if (js-nullish? raw)
      #f
      (with-handlers ([exn:fail? (λ (_) #f)])
        (define s (js-value->string raw))
        (if (= (string-length s) 0)
            #f
            (string-ref s 0)))))

(define (minischeme-stream-next-char stream)
  (define raw (js-send/extern stream "next" (vector)))
  (if (js-nullish? raw)
      #f
      (with-handlers ([exn:fail? (λ (_) #f)])
        (define s (js-value->string raw))
        (if (= (string-length s) 0)
            #f
            (string-ref s 0)))))

(define (make-minischeme-overlay-token-handler)
  (procedure->external
   (let ([tokenize
          (λ (stream)
            (define start-pos (js-number-value (js-ref stream "pos")))
            (define result
              (with-handlers ([exn:fail? (λ (_) #f)])
                (cond
                  [(js-send/truthy stream "eatSpace" (vector)) #f]
                  [(js-send/truthy stream "eol" (vector)) #f]
                  [else
                   (define ch (minischeme-stream-next-char stream))
                   (cond
                     [(not ch) #f]
                     [(char=? ch #\;)
                      (js-send stream "skipToEnd" (vector))
                      #f]
                     [(char=? ch #\")
                      (let loop ([escaped? #f])
                        (define c (minischeme-stream-next-char stream))
                        (cond
                          [(not c) (void)]
                          [escaped? (loop #f)]
                          [(char=? c #\\) (loop #t)]
                          [(char=? c #\") (void)]
                          [else (loop #f)]))
                      #f]
                     [(minischeme-overlay-delimiter? ch)
                      #f]
                     [else
                      (let loop ()
                        (define c (minischeme-stream-peek-char stream))
                        (when (and c (not (minischeme-overlay-delimiter? c)))
                          (minischeme-stream-next-char stream)
                          (loop)))
                      (minischeme-token-style
                       (js-value->string (js-send/extern stream "current" (vector))))])])))
            ;; Instrumentation + guard: ensure we always advance.
            (define end-pos (js-number-value (js-ref stream "pos")))
            (when (and (= start-pos end-pos)
                       (not (js-send/truthy stream "eol" (vector))))
              (js-log (format "[minischeme/overlay] non-advance start=~a end=~a" start-pos end-pos))
              (js-send stream "next" (vector)))
            result)])
     (case-lambda
       [(stream) (tokenize stream)]
       [(stream _state) (tokenize stream)]))))

(define (minischeme-expected-closer prefix)
  ;; Returns the closer that matches the most recent unmatched opener.
  (define n (string-length prefix))
  (let loop ([i 0] [stack '()] [in-string? #f] [escaped? #f] [in-comment? #f])
    (if (= i n)
        (if (null? stack) #f (car stack))
        (let ([ch (string-ref prefix i)])
          (cond
            [in-comment?
             (if (or (char=? ch #\newline) (char=? ch #\return))
                 (loop (+ i 1) stack in-string? escaped? #f)
                 (loop (+ i 1) stack in-string? escaped? #t))]
            [in-string?
             (cond
               [escaped?
                (loop (+ i 1) stack #t #f #f)]
               [(char=? ch #\\)
                (loop (+ i 1) stack #t #t #f)]
               [(char=? ch #\")
                (loop (+ i 1) stack #f #f #f)]
               [else
                (loop (+ i 1) stack #t #f #f)])]
            [else
             (cond
               [(char=? ch #\;)
                (loop (+ i 1) stack #f #f #t)]
               [(char=? ch #\")
                (loop (+ i 1) stack #t #f #f)]
               [(char=? ch #\()
                (loop (+ i 1) (cons #\) stack) #f #f #f)]
               [(char=? ch #\[)
                (loop (+ i 1) (cons #\] stack) #f #f #f)]
               [(char=? ch #\{)
                (loop (+ i 1) (cons #\} stack) #f #f #f)]
               [(and (pair? stack) (char=? ch (car stack)))
                (loop (+ i 1) (cdr stack) #f #f #f)]
               [else
                (loop (+ i 1) stack #f #f #f)])])))))

(define (minischeme-open-paren? ch)
  (or (char=? ch #\()
      (char=? ch #\[)
      (char=? ch #\{)))

(define (minischeme-close-paren? ch)
  (or (char=? ch #\))
      (char=? ch #\])
      (char=? ch #\})))

(define (minischeme-matching-close ch)
  (cond
    [(char=? ch #\() #\)]
    [(char=? ch #\[) #\]]
    [(char=? ch #\{) #\}]
    [else #f]))

(define (minischeme-matching-open ch)
  (cond
    [(char=? ch #\)) #\(]
    [(char=? ch #\]) #\[]
    [(char=? ch #\}) #\{]
    [else #f]))

(define (minischeme-find-matching-right text open-index open-ch)
  (define close-ch (minischeme-matching-close open-ch))
  (if (not close-ch)
      #f
      (let loop ([i (+ open-index 1)] [depth 1])
        (cond
          [(>= i (string-length text)) #f]
          [else
           (define ch (string-ref text i))
           (cond
             [(char=? ch open-ch) (loop (+ i 1) (+ depth 1))]
             [(char=? ch close-ch)
              (if (= depth 1)
                  i
                  (loop (+ i 1) (- depth 1)))]
             [else (loop (+ i 1) depth)])]))))

(define (minischeme-find-matching-left text close-index close-ch)
  (define open-ch (minischeme-matching-open close-ch))
  (if (not open-ch)
      #f
      (let loop ([i (- close-index 1)] [depth 1])
        (cond
          [(< i 0) #f]
          [else
           (define ch (string-ref text i))
           (cond
             [(char=? ch close-ch) (loop (- i 1) (+ depth 1))]
             [(char=? ch open-ch)
              (if (= depth 1)
                  i
                  (loop (- i 1) (- depth 1)))]
             [else (loop (- i 1) depth)])]))))

(define (minischeme-next-non-whitespace-index text start-index)
  (let loop ([i start-index])
    (cond
      [(>= i (string-length text)) #f]
      [(char-whitespace? (string-ref text i)) (loop (+ i 1))]
      [else i])))

(define (minischeme-prev-non-whitespace-index text start-index)
  (let loop ([i start-index])
    (cond
      [(< i 0) #f]
      [(char-whitespace? (string-ref text i)) (loop (- i 1))]
      [else i])))

(define (minischeme-init-codemirror! input-node on-run!)
  (when (and (not minischeme-editor) (minischeme-codemirror-ready?))
    (define codemirror (js-var "CodeMirror"))
    ;; Use a full WebRacket callback for token classification.
    (minischeme-refresh-highlight-tables!)
    (define overlay
      (js-object
       (vector
        (vector "name" "minischeme-overlay-webracket")
        (vector "token" (make-minischeme-overlay-token-handler)))))
    (define universal-close-handler
      (procedure->external
       (λ (cm)
         (define cursor (js-send/extern cm "getCursor" (vector)))
         (define line (js-ref cursor "line"))
         (define ch (js-ref cursor "ch"))
         (define start-pos (js-object (vector (vector "line" 0) (vector "ch" 0))))
         (define prefix (js-value->string (js-send/extern cm "getRange" (vector start-pos cursor))))
         (define expected (minischeme-expected-closer prefix))
         (define closer (if expected expected #\]))
         (define closer-text (string closer))
         (define next-pos (js-object (vector (vector "line" line) (vector "ch" (+ ch 1)))))
         (define next-char (js-value->string (js-send/extern cm "getRange" (vector cursor next-pos))))
         (if (string=? next-char closer-text)
             (js-send cm "setCursor" (vector next-pos))
             (js-send cm "replaceSelection" (vector closer-text)))
         (void))))
    (define run-shortcut-handler
      (procedure->external
       (λ (_cm)
         (on-run!)
         (void))))
    (define alt-right-paren-handler
      (procedure->external
       (λ (cm)
         (define cursor (js-send/extern cm "getCursor" (vector)))
         (define index (inexact->exact
                        (round (js-number-value
                                (js-send/extern cm "indexFromPos" (vector cursor))))))
         (define text (js-value->string (js-send/extern cm "getValue" (vector))))
         (define open-index (minischeme-next-non-whitespace-index text index))
         (when open-index
           (define ch (string-ref text open-index))
           (when (minischeme-open-paren? ch)
             (define match-index (minischeme-find-matching-right text open-index ch))
             (when match-index
               (define target-pos
                 (js-send/extern cm "posFromIndex" (vector (+ match-index 1))))
               (js-send cm "setCursor" (vector target-pos)))))
         (void))))
    (define alt-left-paren-handler
      (procedure->external
       (λ (cm)
         (define cursor (js-send/extern cm "getCursor" (vector)))
         (define index (inexact->exact
                        (round (js-number-value
                                (js-send/extern cm "indexFromPos" (vector cursor))))))
         (define text (js-value->string (js-send/extern cm "getValue" (vector))))
         (define close-index (minischeme-prev-non-whitespace-index text (- index 1)))
         (when close-index
           (define ch (string-ref text close-index))
           (when (minischeme-close-paren? ch)
             (define match-index (minischeme-find-matching-left text close-index ch))
             (when match-index
               (define target-pos
                 (js-send/extern cm "posFromIndex" (vector match-index)))
               (js-send cm "setCursor" (vector target-pos)))))
         (void))))
    (define extra-keys
      (js-object
       (vector
        (vector "]" universal-close-handler)
        (vector "Alt-Right" alt-right-paren-handler)
        (vector "Alt-Left" alt-left-paren-handler)
        (vector "Ctrl-Enter" run-shortcut-handler)
        (vector "Cmd-Enter" run-shortcut-handler))))
    (define options
      (js-object
       (vector
        (vector "mode" "scheme")
        (vector "lineNumbers" #t)
        (vector "lineWrapping" #t)
        (vector "indentUnit" 2)
        (vector "tabSize" 2)
        (vector "indentWithTabs" #f)
        (vector "matchBrackets" #t)
        (vector "autoCloseBrackets" #t)
        (vector "extraKeys" extra-keys))))
    (set! minischeme-editor
          (js-send/extern codemirror "fromTextArea" (vector input-node options)))
    (js-send minischeme-editor "addOverlay" (vector overlay))
    (js-send minischeme-editor "setSize" (vector "100%" "320px"))
    (js-send minischeme-editor "refresh" (vector))
    (set! minischeme-editor-change-handler
          (procedure->external
           (λ (_cm _change)
             (minischeme-save-source
              (js-value->string (js-send/extern minischeme-editor "getValue" (vector))))
             (void))))
    (js-send minischeme-editor "on" (vector "change" minischeme-editor-change-handler))
    (js-send minischeme-editor "focus" (vector))
    (void)))

(define (ensure-minischeme-codemirror-assets! input-node on-run!)
  (define head (js-document-head))

  (define (maybe-init-codemirror)
    (minischeme-init-codemirror! input-node on-run!))

  (define cm-style-id "minischeme-codemirror-css")
  (define cm-style-existing (js-get-element-by-id cm-style-id))
  (when (js-nullish? cm-style-existing)
    (define link (js-create-element "link"))
    (js-set-attribute! link "id" cm-style-id)
    (js-set-attribute! link "rel" "stylesheet")
    (js-set-attribute! link "href" minischeme-codemirror-css-url)
    (js-append-child! head link))

  (define cm-ui-style-id "minischeme-codemirror-ui-style")
  (define cm-ui-style-existing (js-get-element-by-id cm-ui-style-id))
  (when (js-nullish? cm-ui-style-existing)
    (define style (js-create-element "style"))
    (js-set-attribute! style "id" cm-ui-style-id)
    (js-set! style "textContent"
             (string-append
              ".CodeMirror {"
              "  width: 100%;"
              "  min-height: 320px;"
              "  height: 320px;"
              "  border-radius: 10px;"
              "  border: 1px solid rgba(120, 140, 220, 0.35);"
              "  background: #0A0C18;"
              "  color: #EEF2FF;"
              "  font-family: \"Iosevka\", \"Fira Code\", ui-monospace, monospace;"
              "  font-size: 0.96rem;"
              "  line-height: 1.45;"
              "}\n"
              ".CodeMirror-gutters {"
              "  border-right: 1px solid rgba(120, 140, 220, 0.25);"
              "  background: #080a14;"
              "}\n"
              ".CodeMirror-linenumber { color: #8FA0D8; }\n"
              ".CodeMirror-cursor { border-left: 1px solid #EAF0FF !important; }\n"
              ".cm-s-default .cm-comment { color: #7382B0; }\n"
              ".cm-s-default .cm-keyword { color: #44D5FF; font-weight: 700; }\n"
              ".cm-s-default .cm-number { color: #FFD166; }\n"
              ".cm-s-default .cm-string { color: #8fdb9f; }\n"
              ".cm-s-default .cm-atom { color: #F29F97; }\n"
              ".cm-s-default .cm-variable { color: #F6F8FF; }\n"
              ".cm-s-default .cm-def { color: #6EE7FF; font-weight: 700; }\n"
              ".cm-s-default .cm-builtin { color: #FF7AF6; font-weight: 700; }\n"
              ".cm-s-default .cm-bracket { color: #B6BDD6; }\n"
              ".cm-s-default .cm-paren { color: #B6BDD6; }\n"
              ".cm-s-default .cm-ms-form { color: #44D5FF; font-weight: 700; }\n"
              ".cm-s-default .cm-ms-builtin { color: #FF7AF6; font-weight: 700; }\n"))
    (js-append-child! head style))

  (define script-loaded-attr "data-minischeme-loaded")

  (define (script-marked-loaded? script)
    (define raw (js-get-attribute script script-loaded-attr))
    (and (not (js-nullish? raw))
         (string=? (js-value->string raw) "1")))

  (define (mark-script-loaded! script)
    (js-set-attribute! script script-loaded-attr "1"))

  (define (ensure-script! script-id script-url ready? on-ready!)
    (define onload-external
      (procedure->external
       (λ (_)
         (define loaded-script (js-get-element-by-id script-id))
         (when (not (js-nullish? loaded-script))
           (mark-script-loaded! loaded-script))
         (on-ready!)
         (void))))
    (define existing (js-get-element-by-id script-id))
    (cond
      [(not (js-nullish? existing))
       (cond
         [(or (script-marked-loaded? existing) (ready?))
          (mark-script-loaded! existing)
          (on-ready!)]
         [else
          ;; If another page instance already inserted this script, wait for
          ;; its load event instead of assuming it is ready.
          (js-add-event-listener! existing "load" onload-external)])]
      [else
       (define script (js-create-element "script"))
       (js-set-attribute! script "id" script-id)
       (js-set-attribute! script "src" script-url)
       ;; Ensure deterministic execution order for injected scripts.
       (js-set! script "async" #f)
       (js-add-event-listener! script "load" onload-external)
       (js-append-child! head script)]))

  (define (load-addons!)
    (ensure-script! "minischeme-codemirror-scheme-mode-js"
                    minischeme-codemirror-scheme-mode-js-url
                    minischeme-codemirror-scheme-mode-ready?
                    (λ ()
                      (ensure-script! "minischeme-codemirror-matchbrackets-js"
                                      minischeme-codemirror-matchbrackets-js-url
                                      minischeme-codemirror-matchbrackets-ready?
                                      (λ ()
                                        (ensure-script! "minischeme-codemirror-closebrackets-js"
                                                        minischeme-codemirror-closebrackets-js-url
                                                        minischeme-codemirror-closebrackets-ready?
                                                        maybe-init-codemirror))))))

  (if (minischeme-codemirror-ready?)
      (load-addons!)
      (ensure-script! "minischeme-codemirror-js"
                      minischeme-codemirror-js-url
                      minischeme-codemirror-ready?
                      load-addons!)))

(define (init-minischeme-page!)
  (when (not minischeme-page-started?)
    (set! minischeme-page-started? #t)
    (define input-node       (js-get-element-by-id "minischeme-input"))
    (define output-node      (js-get-element-by-id "minischeme-output"))
    (define run-button       (js-get-element-by-id "minischeme-run"))
    (define reset-button     (js-get-element-by-id "minischeme-reset"))
    (define sample-button    (js-get-element-by-id "minischeme-load-sample"))

    (when (or (js-nullish? input-node)
              (js-nullish? output-node)
              (js-nullish? run-button)
              (js-nullish? reset-button)
              (js-nullish? sample-button))
      (error 'minischeme-page "missing expected DOM nodes for MiniScheme page"))

    (minischeme-restore-source! input-node)

    (define (set-output! text)
      (js-set! output-node "textContent" text))

    (define (run! . _)
      (with-handlers ([exn:fail? (λ (e)
                                   (set-output!
                                    (string-append "error: " (exn-message e))))])
        (define source (minischeme-editor-get-source input-node))
        (set-output! (minischeme-process-input source))))

    (define (reset! . _)
      (minischeme-reset-state!)
      (set-output! (string-append "MiniScheme state reset. Build " minischeme-build-id ".")))

    (define (load-sample! . _)
      (minischeme-editor-set-source! input-node minischeme-sample-program)
      (set-output! "Sample program loaded. Click Run to evaluate."))

    (set! minischeme-input-handler
          (procedure->external
           (λ (_event)
             (minischeme-save-source (minischeme-editor-get-source input-node))
             (void))))
    (js-add-event-listener! input-node "input" minischeme-input-handler)

    (set! minischeme-run-handler   (procedure->external run!))
    (set! minischeme-reset-handler (procedure->external reset!))
    (set! minischeme-load-handler  (procedure->external load-sample!))

    (js-add-event-listener! run-button    "click" minischeme-run-handler)
    (js-add-event-listener! reset-button  "click" minischeme-reset-handler)
    (js-add-event-listener! sample-button "click" minischeme-load-handler)

    (ensure-minischeme-codemirror-assets! input-node run!)

    (minischeme-reset-state!)
    (set-output! (string-append "MiniScheme ready. Build " minischeme-build-id "."))))
