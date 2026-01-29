;;;
;;; MathJax 4 live preview (WebRacket site page)
;;;



(define mathjax-cdn-url
  "https://cdn.jsdelivr.net/npm/mathjax@4/tex-mml-chtml.js")

(define mathjax-default-expression
  "\\int_{0}^{\\pi} \\sin(x)\\,\\mathrm{d}x = 2")

(define (mathjax-display latex)
  (string-append "$$" latex "$$"))

(define (mathjax-page)
  `(div (@ (class "page page--mathjax"))
        ,(navbar)
        (section (@ (class "mathjax-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "MathJax 4")
                           (span (@ (class "pill")) "LaTeX")
                           (span (@ (class "pill")) "DOM + JS FFI"))
                      (h1 (@ (class "hero-title")) "MathJax live preview")
                      (p (@ (class "hero-lead"))
                         "Explore how WebRacket drives MathJax in the browser. "
                         "Type LaTeX on the left and watch the preview update instantly.")))
        ,(section-block
          "Live editor"
          "Use standard LaTeX syntax to render math with MathJax 4."
          (list
           `(div (@ (class "mathjax-grid"))
                 (div (@ (class "mathjax-pane"))
                      (div (@ (class "mathjax-pane-header"))
                           (span (@ (class "pane-label")) "LaTeX input")
                           (span (@ (class "pane-meta")) "updates on input"))
                      (textarea (@ (id "mathjax-input")
                                   (class "mathjax-input")
                                   (rows "12")
                                   (spellcheck "false"))
                                ,mathjax-default-expression)
                      (p (@ (class "mathjax-hint"))
                         "Try one of: " (br)
                         (code "\\frac{1}{1+x^2}")          (br)
                         (code "\\sum_{n=0}^{\\infty} x^n") (br)
                         (code "\\sqrt{x^2 + y^2} = r")))
                 (div (@ (class "mathjax-pane"))
                      (div (@ (class "mathjax-pane-header"))
                           (span (@ (class "pane-label")) "Preview")
                           (span (@ (class "pane-meta")) "MathJax 4"))
                      (div (@ (id "mathjax-preview")
                              (class "mathjax-preview")
                              (aria-live "polite"))
                           ,(mathjax-display mathjax-default-expression)))))
          #f
          "section--mathjax")
        ,(section-block
          "How it works"
          "MathJax is loaded from a CDN and triggered via the DOM + JS FFI."
          (list
           `(div (@ (class "mathjax-details"))
                 (p "The preview pane updates on each keystroke. "
                    "Once MathJax finishes loading, the preview is re-typeset automatically.")
                 (div (@ (class "mathjax-actions"))
                      ,(code-pill (gh-dir "examples/mathjax4") "Example source")
                      ,(code-pill (gh-file "web-site/src/examples/mathjax.rkt") "Page layout"))))
          #f
          "section--mathjax-details")
        ,(footer-section)))


(define mathjax-textarea #f)
(define mathjax-preview  #f)
(define mathjax-loaded?  #f)

(define (mathjax-typeset-clear element)
  (js-send (js-var "MathJax") "typesetClear" (vector (vector element))))

(define (mathjax-typeset-promise element)
  (js-send (js-var "MathJax") "typesetPromise" (vector (vector element))))

(define (render-mathjax-preview)  
  (when (and mathjax-textarea mathjax-preview)    
    (define latex (js-ref mathjax-textarea "value"))
    (when (string? latex)
      (when mathjax-loaded?
        (mathjax-typeset-clear mathjax-preview))
      (js-set! mathjax-preview "textContent"
               (mathjax-display latex))
      (when mathjax-loaded?
        (mathjax-typeset-promise mathjax-preview)))))

(define mathjax-input-handler
  (procedure->external
   (lambda (_evt)
     (render-mathjax-preview)
     (void))))

(define mathjax-loaded-handler
  (procedure->external
   (lambda (_evt)
     (set! mathjax-loaded? #t)
     (render-mathjax-preview)
     (void))))

(define (attach-mathjax-input-handler!)
  (when mathjax-textarea
    (js-add-event-listener! mathjax-textarea "input" mathjax-input-handler)))


; js-nullish? : extern -> boolean
;  checks whether an external value is null or undefined
(define (js-nullish? x)
  (cond
    [(not x) #t] ; in case some call really returns #f
    [else
     (define s (js-value->string x))
     (or (string=? s "null")
         (string=? s "undefined"))]))


(define (load-mathjax-script!)  
  (define existing (js-get-element-by-id "mathjax-script"))

  (cond
    [(not (js-nullish? existing))
     ;; Script tag already present: just wait for load
     ;; (or if already loaded, the load event may never fire again).
     (js-add-event-listener! existing "load" mathjax-loaded-handler)]

    [else
     (define head   (js-document-head))
     (define script (js-create-element "script"))
     (js-set-attribute! script "id" "mathjax-script")
     (js-set-attribute! script "defer" "") ; MathJax docs typically use defer
     (js-set-attribute! script "src" mathjax-cdn-url)
     (js-add-event-listener! script "load" mathjax-loaded-handler)
     (js-append-child! head script)]))


(define (init-mathjax-page!)
  (set! mathjax-textarea (js-get-element-by-id "mathjax-input"))
  (set! mathjax-preview  (js-get-element-by-id "mathjax-preview"))
  (js-set! (js-var "document") "title" "MathJax 4 live preview")
  (attach-mathjax-input-handler!)
  (render-mathjax-preview)
  (load-mathjax-script!))
