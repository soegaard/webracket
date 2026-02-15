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

(define (nullish? x)
  (cond
    [(not x) #t]
    [else
     (define s (js-value->string x))
     (or (string=? s "null")
         (string=? s "undefined"))]))

(include "minischeme/minischeme.rkt")

(define minischeme-page-started? #f)
(define minischeme-run-handler   #f)
(define minischeme-reset-handler #f)
(define minischeme-load-handler  #f)
(define minischeme-input-handler #f)
(define minischeme-editor-key-handler #f)
(define minischeme-editor-change-handler #f)
(define minischeme-editor #f)

(define (minischeme-codemirror-ready?)
  (not (string=? (js-typeof (js-var "CodeMirror")) "undefined")))

(define (minischeme-codemirror-scheme-mode-ready?)
  (and (minischeme-codemirror-ready?)
       (let* ([codemirror (js-var "CodeMirror")]
              [modes (js-ref codemirror "modes")]
              [scheme-mode (and (not (nullish? modes))
                                (js-ref modes "scheme"))])
         (and (not (nullish? scheme-mode))
              (not (string=? (js-typeof scheme-mode) "undefined"))))))

(define (minischeme-codemirror-option-handler-ready? option-name)
  (and (minischeme-codemirror-ready?)
       (let* ([codemirror (js-var "CodeMirror")]
              [handlers (js-ref codemirror "optionHandlers")]
              [handler (and (not (nullish? handlers))
                            (js-ref handlers option-name))])
         (and (not (nullish? handler))
              (not (string=? (js-typeof handler) "undefined"))))))

(define (minischeme-codemirror-matchbrackets-ready?)
  (minischeme-codemirror-option-handler-ready? "matchBrackets"))

(define (minischeme-codemirror-closebrackets-ready?)
  (minischeme-codemirror-option-handler-ready? "autoCloseBrackets"))

(define (minischeme-load-saved-source)
  (define storage (js-ref (js-var "window") "localStorage"))
  (if (nullish? storage)
      #f
      (let ([saved (js-send storage "getItem" (vector minischeme-storage-key))])
        (if (nullish? saved)
            #f
            (js-value->string saved)))))

(define (minischeme-save-source source)
  (define storage (js-ref (js-var "window") "localStorage"))
  (unless (nullish? storage)
    (js-send storage "setItem" (vector minischeme-storage-key source))))

(define (minischeme-restore-source! input-node)
  (define saved (minischeme-load-saved-source))
  (when (and saved (not (string=? saved "")))
    (js-set! input-node "value" saved)))

(define (minischeme-editor-get-source input-node)
  (if minischeme-editor
      (js-value->string (js-send minischeme-editor "getValue" (vector)))
      (js-value->string (js-ref input-node "value"))))

(define (minischeme-editor-set-source! input-node source)
  (if minischeme-editor
      (js-send minischeme-editor "setValue" (vector source))
      (js-set! input-node "value" source))
  (minischeme-save-source source))

(define (minischeme-init-codemirror! input-node on-run!)
  (when (and (not minischeme-editor) (minischeme-codemirror-ready?))
    (define codemirror (js-var "CodeMirror"))
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
        (vector "autoCloseBrackets" #t))))
    (set! minischeme-editor
          (js-send codemirror "fromTextArea" (vector input-node options)))
    (js-send minischeme-editor "setSize" (vector "100%" "320px"))
    (js-send minischeme-editor "refresh" (vector))
    (define wrapper (js-send minischeme-editor "getWrapperElement" (vector)))
    (set! minischeme-editor-key-handler
          (procedure->external
           (λ (event)
             (define key (js-value->string (js-ref event "key")))
             (when (and (string=? key "Enter")
                        (or (js-ref event "ctrlKey")
                            (js-ref event "metaKey")))
               (js-send event "preventDefault" (vector))
               (on-run!))
             (void))))
    (js-add-event-listener! wrapper "keydown" minischeme-editor-key-handler)
    (set! minischeme-editor-change-handler
          (procedure->external
           (λ (_cm _change)
             (minischeme-save-source
              (js-value->string (js-send minischeme-editor "getValue" (vector))))
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
  (when (nullish? cm-style-existing)
    (define link (js-create-element "link"))
    (js-set-attribute! link "id" cm-style-id)
    (js-set-attribute! link "rel" "stylesheet")
    (js-set-attribute! link "href" minischeme-codemirror-css-url)
    (js-append-child! head link))

  (define cm-ui-style-id "minischeme-codemirror-ui-style")
  (define cm-ui-style-existing (js-get-element-by-id cm-ui-style-id))
  (when (nullish? cm-ui-style-existing)
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
              ".cm-s-default .cm-comment { color: #8B96C7; }\n"
              ".cm-s-default .cm-keyword { color: #8ab4ff; }\n"
              ".cm-s-default .cm-number { color: #FFD166; }\n"
              ".cm-s-default .cm-string { color: #8fdb9f; }\n"
              ".cm-s-default .cm-atom { color: #F29F97; }\n"
              ".cm-s-default .cm-variable { color: #EAF0FF; }\n"
              ".cm-s-default .cm-def { color: #8AB4FF; }\n"
              ".cm-s-default .cm-builtin { color: #B6BDD6; }\n"
              ".cm-s-default .cm-bracket { color: #B6BDD6; }\n"
              ".cm-s-default .cm-paren { color: #B6BDD6; }\n"))
    (js-append-child! head style))

  (define script-loaded-attr "data-minischeme-loaded")

  (define (script-marked-loaded? script)
    (define raw (js-get-attribute script script-loaded-attr))
    (and (not (nullish? raw))
         (string=? (js-value->string raw) "1")))

  (define (mark-script-loaded! script)
    (js-set-attribute! script script-loaded-attr "1"))

  (define (ensure-script! script-id script-url ready? on-ready!)
    (define onload-external
      (procedure->external
       (λ (_)
         (define loaded-script (js-get-element-by-id script-id))
         (when (not (nullish? loaded-script))
           (mark-script-loaded! loaded-script))
         (on-ready!)
         (void))))
    (define existing (js-get-element-by-id script-id))
    (cond
      [(not (nullish? existing))
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

    (when (or (nullish? input-node)
              (nullish? output-node)
              (nullish? run-button)
              (nullish? reset-button)
              (nullish? sample-button))
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

    (set! minischeme-editor-key-handler
          (procedure->external
           (λ (event)
             (define key (js-value->string (js-ref event "key")))
             (when (and (string=? key "Enter")
                        (or (js-ref event "ctrlKey")
                            (js-ref event "metaKey")))
               (js-send event "preventDefault" (vector))
               (run!))
             (void))))
    (js-add-event-listener! input-node "keydown" minischeme-editor-key-handler)

    (ensure-minischeme-codemirror-assets! input-node run!)

    (minischeme-reset-state!)
    (set-output! (string-append "MiniScheme ready. Build " minischeme-build-id "."))))
