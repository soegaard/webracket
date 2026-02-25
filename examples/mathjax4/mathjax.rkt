;;;
;;; MathJax 4 live preview
;;;

;;; This example shows how to load MathJax 4 from a CDN and wire it up to
;;; a simple LaTeX playground. The UI is generated with `sxml->dom` from
;;; the browser standard library.
;;;
;;; Build the example with the standard library so that `sxml->dom` is
;;; available:
;;;
;;;   racket ../../webracket.rkt --ffi dom.ffi --ffi standard.ffi -b mathjax.rkt
;;;
;;; Serve the folder with `raco static-web` (or any static server) and
;;; open `mathjax.html`.

(define mathjax-cdn-url
  "https://cdn.jsdelivr.net/npm/mathjax@4/tex-mml-chtml.js")

(define default-expression
  "\\int_{0}^{\\pi} \\sin(x)\\,dx = 2")

(define page-css
  (string-append "body {\n"
                 "  margin: 0;\n"
                 "  background: #f7f8fb;\n"
                 "  font-family: 'Inter', 'Segoe UI', system-ui, -apple-system, sans-serif;\n"
                 "  color: #1f2933;\n"
                 "}\n"
                 ".mathjax-demo {\n"
                 "  max-width: 960px;\n"
                 "  margin: 0 auto;\n"
                 "  padding: 56px 24px 72px;\n"
                 "  box-sizing: border-box;\n"
                 "}\n"
                 ".mathjax-demo .title {\n"
                 "  margin: 0 0 12px;\n"
                 "  font-size: 34px;\n"
                 "  font-weight: 700;\n"
                 "  letter-spacing: -0.02em;\n"
                 "}\n"
                 ".mathjax-demo .intro {\n"
                 "  margin: 0;\n"
                 "  font-size: 16px;\n"
                 "  line-height: 1.6;\n"
                 "  color: #475569;\n"
                 "}\n"
                 ".mathjax-demo .playground {\n"
                 "  margin-top: 32px;\n"
                 "  display: flex;\n"
                 "  gap: 24px;\n"
                 "  align-items: stretch;\n"
                 "}\n"
                 ".mathjax-demo .pane {\n"
                 "  flex: 1 1 0;\n"
                 "  display: flex;\n"
                 "  flex-direction: column;\n"
                 "  gap: 16px;\n"
                 "}\n"
                 ".mathjax-demo .pane-label {\n"
                 "  font-size: 12px;\n"
                 "  font-weight: 600;\n"
                 "  letter-spacing: 0.12em;\n"
                 "  text-transform: uppercase;\n"
                 "  color: #64748b;\n"
                 "}\n"
                 ".mathjax-demo .editor {\n"
                 "  min-height: 280px;\n"
                 "  padding: 16px 18px;\n"
                 "  border-radius: 14px;\n"
                 "  border: 1px solid #d9e2ec;\n"
                 "  background: #ffffff;\n"
                 "  color: #0f172a;\n"
                 "  box-shadow: 0 18px 40px rgba(15, 23, 42, 0.08);\n"
                 "  font-size: 16px;\n"
                 "  line-height: 1.6;\n"
                 "  font-family: 'JetBrains Mono', 'Fira Code', 'SFMono-Regular', monospace;\n"
                 "  resize: vertical;\n"
                 "  outline: none;\n"
                 "}\n"
                 ".mathjax-demo .editor:focus {\n"
                 "  border-color: #4f46e5;\n"
                 "  box-shadow: 0 0 0 3px rgba(79, 70, 229, 0.15);\n"
                 "}\n"
                 ".mathjax-demo .hint {\n"
                 "  margin: 0;\n"
                 "  font-size: 13px;\n"
                 "  color: #6b7280;\n"
                 "}\n"
                 ".mathjax-demo .preview-box {\n"
                 "  min-height: 280px;\n"
                 "  padding: 20px 22px;\n"
                 "  border-radius: 14px;\n"
                 "  border: 1px solid #d9e2ec;\n"
                 "  background: #ffffff;\n"
                 "  box-shadow: inset 0 0 0 1px rgba(148, 163, 184, 0.16), 0 18px 40px rgba(15, 23, 42, 0.05);\n"
                 "  display: flex;\n"
                 "  align-items: center;\n"
                 "  justify-content: center;\n"
                 "}\n"
                 ".mathjax-demo .preview-box mjx-container {\n"
                 "  font-size: 120%;\n"
                 "}\n"
                 "@media (max-width: 900px) {\n"
                 "  .mathjax-demo .playground {\n"
                 "    flex-direction: column;\n"
                 "  }\n"
                 "  .mathjax-demo .preview-box,\n"
                 "  .mathjax-demo .editor {\n"
                 "    min-height: 200px;\n"
                 "  }\n"
                 "}\n"))

(define page-structure
  `(div (@ (class "mathjax-demo"))
        (h1 (@ (class "title")) "MathJax 4 live preview")
        (p (@ (class "intro"))
           "MathJax 4 parses LaTeX input and renders high-quality mathematics in the browser. " (br)
           "Type an expression in the editor to see the typesetting refresh instantly.")
        (div (@ (class "playground"))
             (div (@ (class "pane"))
                  (label (@ (for "latex-input") (class "pane-label")) "LaTeX input")
                  (textarea (@ (id "latex-input")
                               (class "editor")
                               (rows "12")
                               (spellcheck "false"))
                            ,default-expression)
                  (p (@ (class "hint"))
                     "Examples: " (br) (br)
                     "\\frac{1}{1+x^2}, \\sum_{n=0}^{\\infty} x^n, or \\sqrt{x^2 + y^2} = r."))
             (div (@ (class "pane"))
                  (div (@ (class "pane-label")) "Preview")
                  (div (@ (id "mathjax-preview")
                          (class "preview-box")
                          (aria-live "polite")
                          (role "presentation"))
                       ,(string-append "$$" default-expression "$$"))))))

(define latex-textarea    #f)
(define preview-container #f)
(define mathjax-loaded?   #f)

(define (inject-style!)
  (define head  (js-document-head))
  (define style (js-create-element "style"))
  (js-set! style "textContent" page-css)
  (js-append-child! head style))

(define (build-layout!)
  (define body (js-document-body))
  (define layout (sxml->dom page-structure))
  (js-append-child! body layout)
  (set! latex-textarea (js-get-element-by-id "latex-input"))
  (set! preview-container (js-get-element-by-id "mathjax-preview"))
  (void))

(define (mathjax-typeset-clear element)
  (js-send (js-var "MathJax") "typesetClear" (vector (vector element))))

(define (mathjax-typeset-promise element)
  (js-log "promise!")
  (js-send (js-var "MathJax") "typesetPromise" (vector (vector element))))

(define (render-preview)
  (js-log "render-preview")
  (when (and latex-textarea preview-container)
    (js-log "latex and preview")
    (define latex (js-ref latex-textarea "value"))
    (when (string? latex)
      (js-log "latex is string")
      (when mathjax-loaded?
        (js-log "mathjax is loaded")
        (mathjax-typeset-clear preview-container))
      (js-set! preview-container "textContent"
               (string-append "$$" latex "$$"))
      (when mathjax-loaded?
        (mathjax-typeset-promise preview-container))))
  (void))

(define update-preview-handler
  (procedure->external
   (lambda (_evt)
     (render-preview)
     (void))))

(define (attach-input-handler!)
  (when latex-textarea
    (js-add-event-listener! latex-textarea "input" update-preview-handler)))

(define mathjax-loaded-handler
  (procedure->external
   (lambda (_evt)
     (set! mathjax-loaded? #t)
     (render-preview)
     (void))))

(define (load-mathjax-script!)
  (define head   (js-document-head))
  (define script (js-create-element "script"))
  (js-set-attribute! script "id" "mathjax-script")
  (js-set-attribute! script "async" "")
  (js-set-attribute! script "src" mathjax-cdn-url)
  (js-add-event-listener! script "load" mathjax-loaded-handler)
  (js-append-child! head script))

(js-set! (js-var "document") "title" "MathJax 4 live preview")

(inject-style!)
(build-layout!)
(attach-input-handler!)
(render-preview)
(load-mathjax-script!)
