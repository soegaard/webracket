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

    (define (set-output! text)
      (js-set! output-node "textContent" text))

    (define (run! . _)
      (with-handlers ([exn:fail? (λ (e)
                                   (set-output!
                                    (string-append "error: " (exn-message e))))])
        (define source (js-ref input-node "value"))
        (set-output! (minischeme-process-input source))))

    (define (reset! . _)
      (minischeme-reset-state!)
      (set-output! "MiniScheme state reset."))

    (define (load-sample! . _)
      (js-set! input-node "value" minischeme-sample-program)
      (set-output! "Sample program loaded. Click Run to evaluate."))

    (set! minischeme-run-handler   (procedure->external run!))
    (set! minischeme-reset-handler (procedure->external reset!))
    (set! minischeme-load-handler  (procedure->external load-sample!))

    (js-add-event-listener! run-button    "click" minischeme-run-handler)
    (js-add-event-listener! reset-button  "click" minischeme-reset-handler)
    (js-add-event-listener! sample-button "click" minischeme-load-handler)

    (minischeme-reset-state!)
    (set-output! "MiniScheme ready.")))
