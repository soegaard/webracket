;;;
;;; WebRacket Website
;;;

;;;
;;; Build Instructions
;;;

;; Compile with:
;;     racket ../../webracket.rkt --browser --ffi dom --stdlib web-site.rkt
;; Build script which also copies assets:
;;     ./build.sh

;;;
;;; Color Helpers
;;;

;; byte->hex : Integer -> String
;;   Converts an 8-bit number into a two-character uppercase hex string.
(define (byte->hex n)
  (define s      (number->string n 16))
  (define padded (if (= (string-length s) 1) (string-append "0" s) s))
  (string-upcase padded))

;; make-color* : Integer Integer Integer -> String
;;   Builds a CSS hex color string from RGB byte components.
(define (make-color* r g b)
  (string-append "#" (byte->hex r) (byte->hex g) (byte->hex b)))

;;;
;;; Color Theme
;;;

(define purple (make-color* 101 79 240))  ; #654FF0
(define blue   (make-color* 74 108 255))  ; #4A6CFF
(define red    (make-color* 209 58 58))   ; #D13A3A
(define gold   (make-color* 242 183 5))   ; #F2B705

;;;
;;; SXML Helpers
;;;

;; GitHub base URLs for linking source files.
(define gh-base-file "https://github.com/soegaard/webracket/blob/main/")
(define gh-base-dir "https://github.com/soegaard/webracket/tree/main/")

;; gh-file : String -> String
;;   Builds a GitHub blob URL for a file path.
(define (gh-file path)
  (string-append gh-base-file path))

;; gh-dir : String -> String
;;   Builds a GitHub tree URL for a directory path.
(define (gh-dir path)
  (string-append gh-base-dir path))

;; ext : String Any ... -> List
;;   Builds an external link with target and rel attributes.
(define (ext url . content)
  `(a (@ (href ,url) (target "_blank") (rel "noreferrer noopener")) ,@content))

;; code-link : String String -> List
;;   Builds a code-styled external link.
(define (code-link url text)
  `(a (@ (class "code-link") (href ,url) (target "_blank") (rel "noreferrer noopener"))
      (code ,text)))

;; code-pill : String String -> List
;;   Builds a compact pill-style code link.
(define (code-pill url text)
  `(a (@ (class "code-pill") (href ,url) (target "_blank") (rel "noreferrer noopener"))
      ,text))

;; make-ul-list : (Listof String) (U #f String) -> List
;;   Builds a <ul> sxml node from a list of text items and an optional class.
(define (make-ul-list items [class-name #f])
  (define list-items (map (λ (item) `(li ,item)) items))
  (if class-name
      `(ul (@ (class ,class-name)) ,@list-items)
      `(ul ,@list-items)))

;; callout : (U Symbol String) (U #f String) List ... -> List
;;   Builds a reusable callout box with a variant and optional title.
(define (callout kind title . body)
  (define kind-key
    (cond
      [(symbol? kind) kind]
      [(string? kind) (string->symbol kind)]
      [else 'note]))
  (define kind-class
    (case kind-key
      [(info) "callout--info"]
      [(warn warning) "callout--warn"]
      [else "callout--note"]))
  (define icon
    (case kind-key
      [(info) "ℹ"]
      [(warn warning) "⚠"]
      [else "✦"]))
  (define show-title?
    (and title
         (not (and (string? title) (string=? title "")))))
  (define title-node
    (and show-title?
         `(div (@ (class "callout-title"))
               (span (@ (class "callout-icon")) ,icon)
               (span (@ (class "callout-label")) ,title))))
  `(div (@ (class ,(string-append "callout " kind-class)))
        ,@(if title-node (list title-node) '())
        ,@body))

;; section-block : String (U #f String) (Listof List) (U #f String) (U #f String) -> List
;;   Creates a section container with a title, optional subtitle, and content.
(define (section-block title subtitle content [section-id #f] [section-class #f])
  (define class-name (if section-class
                         (string-append "section " section-class)
                         "section"))
  (define attrs (if section-id
                    `(@ (class ,class-name) (id ,section-id))
                    `(@ (class ,class-name))))
  `(section ,attrs
            (div (@ (class "section-header"))
                 (h2 (@ (class "section-title")) ,title)
                 ,@(if subtitle
                       (list `(p (@ (class "section-lead")) ,subtitle))
                       '()))
            ,@content))

;; step-badge : -> List
;;   Shared numbered badge for step-based layouts.
(define (step-badge)
  `(span (@ (class "pipeline-step") (aria-hidden "true")) ""))

;; quick-start-step : Integer String (U #f String) (Listof List) -> List
;;   Quick Start step section with numbered badge.
(define (quick-start-step step-number title intro content)
  `(section (@ (class "section section--quick-start-step"))
            (div (@ (class "section-header qs-step-header"))
                 (span (@ (class "pipeline-step qs-step-badge")
                          (aria-hidden "true"))
                       "")
                 (div (@ (class "qs-step-titleblock"))
                      (h2 (@ (class "section-title qs-step-title"))
                          ,title)
                      ,@(if intro
                            (list `(p (@ (class "qs-step-intro")) ,intro))
                            '())))
            (div (@ (class "qs-step-body"))
                 ,@content)))

;; card-grid : (Listof (Listof List)) (U #f String) -> List
;;   Wraps card content lists into a grid container.
(define (card-grid cards [class-name #f])
  (define base-class (if class-name
                         (string-append "card-grid " class-name)
                         "card-grid"))
  `(div (@ (class ,base-class))
        ,@(map (λ (card) `(div (@ (class "card")) ,@card)) cards)))

;;;
;;; Page Layout
;;;

(include "completion.rkt")

;;;
;;; Examples Data
;;;

(define examples
  (list
   (make-hash (list (cons 'id "mathjax4")
                    (cons 'title "MathJax 4 Editor")
                    (cons 'path "examples/mathjax4")
                    (cons 'entry "mathjax.html")
                    (cons 'tags (list 'dom 'mathjax))
                    (cons 'summary "Live two-pane editor with instant MathJax 4 LaTeX preview.")
                    (cons 'features (list "MathJax 4 interop"
                                          "DOM + JS FFI"
                                          "Input handling"
                                          "Editor state"))))
   (make-hash (list (cons 'id "matrix-rain")
                    (cons 'title "Matrix Rain")
                    (cons 'path "examples/matrix-rain")
                    (cons 'entry "matrix-rain.html")
                    (cons 'tags (list 'xterm 'dom))
                    (cons 'summary "Matrix rain animation rendered in a browser terminal.")
                    (cons 'features (list "XtermJS integration"
                                          "DOM + JS FFI"
                                          "Timers / animation loop"))))
   (make-hash (list (cons 'id "xtermjs-demo")
                    (cons 'title "XtermJS Demo")
                    (cons 'path "examples/xtermjs-demo")
                    (cons 'entry "xtermjs-demo.html")
                    (cons 'tags (list 'xterm 'dom))
                    (cons 'summary "Interactive terminal with themed styling and built-in commands.")
                    (cons 'features (list "XtermJS add-ons"
                                          "Input handling"
                                          "Command dispatch"))))
   (make-hash (list (cons 'id "minischeme")
                    (cons 'title "MiniScheme REPL")
                    (cons 'path "examples/minischeme")
                    (cons 'entry "minischeme.html")
                    (cons 'tags (list 'repl 'xterm))
                    (cons 'summary "Browser-based Scheme REPL with evaluator, editor, and output.")
                    (cons 'features (list "XtermJS terminal"
                                          "Input handling + history"
                                          "Runtime evaluator"
                                          "Ports + printing"))))
   (make-hash (list (cons 'id "space-invaders")
                    (cons 'title "Space Invaders")
                    (cons 'path "examples/space-invaders")
                    (cons 'entry "space-invaders.html")
                    (cons 'tags (list 'canvas 'dom))
                    (cons 'summary "Arcade shooter on canvas with responsive keyboard controls.")
                    (cons 'features (list "Canvas API via DOM + JS FFI"
                                          "Keyboard events"
                                          "Timers / animation loop"
                                          "Mutable game state"))))
   (make-hash (list (cons 'id "pict")
                    (cons 'title "Canvas + Pict")
                    (cons 'path "examples/pict")
                    (cons 'entry "pict.html")
                    (cons 'tags (list 'canvas))
                    (cons 'summary "Racket pict rendering pipeline compiled for the browser canvas.")
                    (cons 'features (list "Canvas interop"
                                          "Graphics rendering pipeline"
                                          "Performance focus"))))
   (make-hash (list (cons 'id "raco-tiles")
                    (cons 'title "Raccoon Tiles")
                    (cons 'path "examples/raco")
                    (cons 'entry "tiles.html")
                    (cons 'tags (list 'canvas))
                    (cons 'summary "Pixel-art tile sheet drawn on canvas with a custom palette.")
                    (cons 'features (list "Canvas interop"
                                          "Palette mapping"
                                          "Grid layout"))))))

(define featured-example-ids
  (list "mathjax4"
        "minischeme"
        "space-invaders"
        "matrix-rain"))

;; example-demo-url : Hash -> (U #f String)
;;   Builds the local demo URL when an entry HTML file is available.
(define (example-demo-url example)
  (define entry (hash-ref example 'entry #f))
  (and entry (string-append (hash-ref example 'path) "/" entry)))

;; example-source-url : Hash -> String
;;   Builds the GitHub source URL for the example folder.
(define (example-source-url example)
  (string-append "https://github.com/soegaard/webracket/tree/main/"
                 (hash-ref example 'path)))

;; example-kind-class : (Listof Symbol) -> (U #f String)
;;   Picks a category class for subtle accent styling.
(define (example-kind-class tags)
  (cond
    [(member 'mathjax tags) "kind-mathjax"]
    [(member 'xterm tags) "kind-xterm"]
    [(member 'repl tags) "kind-repl"]
    [(member 'canvas tags) "kind-canvas"]
    [(member 'dom tags) "kind-dom"]
    [else #f]))

;; example-kind-label : (Listof Symbol) -> (U #f String)
;;   Labels the kind pill in example cards.
(define (example-kind-label tags)
  (cond
    [(member 'mathjax tags) "MathJax"]
    [(member 'xterm tags) "XtermJS"]
    [(member 'repl tags) "REPL"]
    [(member 'canvas tags) "Canvas"]
    [(member 'dom tags) "DOM"]
    [else #f]))

;; example-tags-string : (Listof Symbol) -> String
;;   Joins tags for data attributes.
(define (example-tags-string tags)
  (string-join (map symbol->string tags) " "))

;; example-card : Hash -> List
;;   Creates a card for a single example entry.
(define (example-card example)
  (define title       (hash-ref example 'title))
  (define summary     (hash-ref example 'summary))
  (define features    (hash-ref example 'features))
  (define tags        (hash-ref example 'tags '()))
  (define kind-class  (example-kind-class  tags))
  (define kind-label  (example-kind-label  tags))
  (define tags-string (example-tags-string tags))
  (define demo-url    (example-demo-url    example))
  (define source-url  (example-source-url  example))
  (define base-class "card example-card")
  `(div (@ (class ,(string-append base-class
                                 (if kind-class (string-append " " kind-class) "")))
           (data-tags ,tags-string))
        (div (@ (class "example-header"))
             (h3 ,title)
             ,@(if kind-label
                   (list `(span (@ (class "example-kind")) ,kind-label))
                   '()))
        (p ,summary)
        (div (@ (class "example-showcases"))
             (span (@ (class "sr-only")) "Showcases:")
             ,(make-ul-list features "feature-list"))
        (div (@ (class "example-actions"))
             ,@(if demo-url
                   (list `(a (@ (class "example-action action-primary")
                                (href ,demo-url))
                             "Open demo →"))
                   '())
             (a (@ (class "example-action action-secondary")
                   (href ,source-url))
                "View source →"))))

;; examples-grid : (Listof Hash) -> List
;;   Wraps example cards into a responsive grid.
(define (examples-grid cards [class-name #f])
  (define base-class (if class-name
                         (string-append "card-grid examples-grid " class-name)
                         "card-grid examples-grid"))
  `(div (@ (class ,base-class))
        ,@(map example-card cards)))

;; current-page : -> Symbol
;;   Determines which page to render based on the URL path.
(define (current-page)
  (define path (js-ref (js-window-location) "pathname"))
  (cond
    [(string-suffix? path "documentation.html")         'documentation]
    [(string-suffix? path "quick-start.html")           'quick-start]
    [(string-suffix? path "installation.html")          'installation]
    [(string-suffix? path "examples.html")              'examples]
    [(string-suffix? path "implementation-status.html") 'implementation-status]
    [(string-suffix? path "community.html")             'community]
    [(string-suffix? path "overview.html")              'overview]
    [(string-suffix? path "roadmap.html")               'roadmap]
    [(string-suffix? path "is-webracket-for-you.html")  'for-you]
    [else                                               'home]))

;; nav-link : String String Symbol Symbol -> List
;;   Creates a nav link with active state styling.
(define (nav-link label href page-id active-page)
  (define class-name (if (eq? active-page page-id)
                         "nav-link nav-link--active"
                         "nav-link"))
  `(a (@ (class ,class-name) (href ,href)) ,label))

;; navbar : -> List
;;   Shared navigation header for all pages.
(define (navbar)
  (define active-page (current-page))
  `(nav (@ (class "navbar"))
        (div (@ (class "nav-left"))
             (a (@ (class "nav-home") (href "index.html"))
                (img (@ (class "nav-logo")
                        (src "assets/hex-racket-wasm-logo.svg")
                        (alt "WebRacket logo")))
                (span "WebRacket")))
        (div (@ (class "nav-links"))
             #;,(nav-link "For You" "is-webracket-for-you.html" 'for-you active-page)
             #;,(nav-link "Overview" "overview.html" 'overview active-page)
             #;,(nav-link "Road Ahead" "roadmap.html" 'roadmap active-page)
             ,(nav-link "Status"        "implementation-status.html" 'implementation-status active-page)
             ,(nav-link "Documentation" "documentation.html"         'documentation         active-page)
             ,(nav-link "Quick Start"   "quick-start.html"           'quick-start           active-page)
             ,(nav-link "Installation"  "installation.html"          'installation          active-page)
             ,(nav-link "Community"     "community.html"             'community             active-page)
             ,(nav-link "Examples"      "examples.html"              'examples              active-page))))

;; footer-section : -> List
;;   Shared footer for all pages.
(define (footer-section)
  `(footer (@ (class "footer"))
           (span "WebRacket — Racket for the browser.")
           (span "Made for the Racket community.")))

;; home-page : -> List
;;   Homepage layout.
(define (home-page)
  `(div (@ (class "page"))
        ,(navbar)
        (section (@ (class "hero"))
                 (div (@ (class "hero-panel"))
                      (h1 (@ (class "hero-title")) "WebRacket")
                      (div (@ (class "hero-copy"))
                           (p  (@ (class "hero-lead"))
                               "A Racket to WebAssembly compiler. " (br)
                               "Build practical browser applications with Racket.")
                           #;(div (@ (class "pill-row"))
                                (span (@ (class "pill")) "JS + DOM FFI")
                                (span (@ (class "pill")) "Runs in browsers + Node"))
                           (div (@ (class "hero-cta"))
                                (a (@ (class "cta-button") (href "#examples")) "Try Live Demo")
                                (a (@ (class "cta-link") (href "https://github.com/soegaard/webracket")) "View on GitHub"))))
                 (div (@ (class "hero-carousel-panel"))
                      (div (@ (class "hero-carousel"))
                           (div (@ (class "carousel-header"))
                                (span "Example screenshots")
                                (span (@ (class "highlight")) "Live demos"))
                           (div (@ (class "carousel-frame"))
                                (img (@ (class "carousel-shot")
                                        (src   "assets/examples/screenshots/mathjax4.png")
                                        (alt   "MathJax 4 editor")
                                        (style "animation-delay: 0s;")))
                                (img (@ (class "carousel-shot")
                                        (src   "assets/examples/screenshots/matrix-rain.png")
                                        (alt   "Matrix digital rain demo")
                                        (style "animation-delay: 5s;")))
                                (img (@ (class "carousel-shot")
                                        (src   "assets/examples/screenshots/xtermjs.png")
                                        (alt   "XtermJS terminal demo")
                                        (style "animation-delay: 10s;")))
                                (img (@ (class "carousel-shot")
                                        (src   "assets/examples/screenshots/minischeme.png")
                                        (alt   "MiniScheme browser REPL")
                                        (style "animation-delay: 15s;")))
                                (img (@ (class "carousel-shot")
                                        (src   "assets/examples/screenshots/space-invaders.png")
                                        (alt   "Space Invaders canvas game")
                                        (style "animation-delay: 20s;")))
                                (img (@ (class "carousel-shot")
                                        (src   "assets/examples/screenshots/pict.png")
                                        (alt   "Pict rendering demo")
                                        (style "animation-delay: 25s;")))))))
        ,(section-block
          "Why WebRacket?"
          "WebRacket is a subset of Racket that compiles to WebAssembly, so you can target modern browsers while staying in a familiar language."
          (list
           (make-ul-list
            (list
             "Compile Racket programs into WebAssembly that runs in Chrome, Firefox, and Safari."
             "Leverage a JavaScript FFI for DOM, Canvas, MathJax, XtermJS, and JSXGraph integrations."
             "Use WebRacket to prototype ideas that could influence a future Racket WebAssembly backend."))))
        ,(section-block
          "Language Coverage"
          "WebRacket implements a substantial portion of Racket with a focus on practical web applications."
          (list
           (card-grid
            (list
             (list `(h3 "Numbers")
                   `(p "Flonums and fixnums."))
             (list `(h3 "Hash Tables")
                   `(p "Mutable hash tables for "
                       (strong (code "eq?"))
                       ", "
                       (strong (code "eqv?"))
                       ", "
                       (strong (code "equal?"))
                       ", and "
                       (strong (code "always?"))
                       " comparisons."))
             (list `(h3 "Structures")
                   `(p "Support for structure properties, super structures, and applicable structs."))
             (list `(h3 "Syntax")
                   `(p "Standard Racket expander support, including for and match forms."))
             (list `(h3 "Control Flow")
                   `(p "Tail calls, multiple values, and upward exceptions."))
             (list `(h3 "Builtins")
                   `(p "Large parts of racket/base is available."))
             (list `(h3 "Concurrency")
                   `(p "Single-threaded execution only."))
             (list `(h3 "Modules")
                   `(p "Work in progress. Use include for now.")))
            "coverage-grid"))
          #f
          "section--coverage")
        ,(section-block
          "Toolchain Essentials"
          "A small set of tools power the WebRacket workflow from source to the browser."
          (list
           `(div (@ (class "toolchain-panel"))
                 (p (@ (class "toolchain-lede")) "Three tools cover build, run, and serve with minimal setup.")
                 ,(make-ul-list
                   (list
                    (string-append "wasm-tools builds and validates the WebAssembly output. ")
                    (string-append "Node.js enables running generated programs in the terminal. ")
                    (string-append "raco-static-web makes it easy to serve compiled artifacts locally."))
                   "toolchain-list")))
          #f
          "section--toolchain")
        ,(section-block
          "Compiler Pipeline"
          "WebRacket uses a direct-style compiler with NanoPass transformations before emitting WebAssembly."
          (list
           (card-grid
            (list
             (list `(div (@ (class "pipeline-header"))
                         ,(step-badge)
                         (h3 (@ (class "pipeline-title")) "Frontend"))
                   `(p "Racket syntax is expanded, then normalized into a compiler-friendly core."))
             (list `(div (@ (class "pipeline-header"))
                         ,(step-badge)
                         (h3 (@ (class "pipeline-title")) "Middle End"))
                   `(p "Nanopass passes like closure conversion and ANF make environments and intermediate values explicit."))
             (list `(div (@ (class "pipeline-header"))
                         ,(step-badge)
                         (h3 (@ (class "pipeline-title")) "Backend"))
                   `(p "Destination-driven code generation emits folded WebAssembly code."))
             (list `(div (@ (class "pipeline-header"))
                         ,(step-badge)
                         (h3 (@ (class "pipeline-title")) "Runtime"))
                   `(p "A custom runtime avoids reliance on host functionality where possible.")))
            "pipeline-grid"))
          #f
          "section--pipeline")
        ,(section-block
          "Roadmap"
          "The long-term goal is full Racket support, but there is plenty more to tackle next."
          (list
           (make-ul-list
            (list
             "Fix bugs reported by early adopters and stabilize the current runtime."
             "Unlock modules by completing linklet support."
             "Add complex numbers, bignums, impersonators, and chaperones."
             "Improve regular expression support and consider CPS for continuations.")))
          #f
          "section--roadmap")
        ,(section-block
          "Example Projects"
          "WebRacket already powers interactive demos that showcase browser APIs."
          (list
           (card-grid
            (list
             (list `(h3 "MathJax 4 Editor")
                   `(p "Live formula preview with WebRacket + MathJax.")
                   `(a (@ (class "example-link") (href "examples.html")) "Open demo"))
             (list `(h3 "Matrix Rain")
                   `(p "Terminal-style animation powered by XtermJS.")
                   `(a (@ (class "example-link") (href "examples.html")) "Open demo"))
             (list `(h3 "MiniScheme REPL")
                   `(p "Interactive Scheme session running in the browser.")
                   `(a (@ (class "example-link") (href "examples.html")) "Open demo"))
             (list `(h3 "Canvas + Pict")
                   `(p "Space Invaders and Pict rendering showcase.")
                   `(a (@ (class "example-link") (href "examples.html")) "Open demo")))
            "examples-grid"))
          "examples"
          "section--examples")
        ,(footer-section)))

;; examples-page : -> List
;;   Examples page layout.
(define (examples-page)
  (define featured-examples
    (filter (λ (example)
              (member (hash-ref example 'id) featured-example-ids))
            examples))
  (define all-examples
    (filter (λ (example)
              (not (member (hash-ref example 'id) featured-example-ids)))
            examples))
  `(div (@ (class "page"))
        ,(navbar)
        (section (@ (class "examples-hero"))
                 (div (@ (class "hero-panel"))
                      (div (@ (class "pill-row"))
                           (span (@ (class "pill")) "DOM + JS FFI")
                           (span (@ (class "pill")) "Canvas")
                           (span (@ (class "pill")) "MathJax")
                           (span (@ (class "pill")) "XtermJS")
                           (span (@ (class "pill")) "REPL"))
                      (h1 (@ (class "hero-title")) "Examples")
                      (p (@ (class "hero-lead"))
                         "Interactive demos showcasing browser APIs and WebRacket features.")
                      (p (@ (class "hero-sublead"))
                         "Each example links to a live demo (when available) and source on GitHub.")))
        ,@(if (null? featured-examples)
              '()
              (list
               (section-block
                "Featured"
                "Start here for the most representative WebRacket demos."
                (list (examples-grid featured-examples "examples-grid--featured"))
                #f
                "section--examples section-featured")))
        ,(section-block
          "All examples"
          "Browse every example in the repository."
          (list (examples-grid all-examples))
          #f
          "section--examples")
        ,(section-block
          "Next steps"
          "Ready to build your own? Here are a few good places to continue."
          (list
           `(ul (@ (class "next-steps-links"))
                (li (a (@ (class "example-action action-primary")
                          (href "installation.html"))
                       "Installation"))
                (li (a (@ (class "example-action action-secondary")
                          (href "overview.html"))
                       "Overview"))
                (li (a (@ (class "example-action action-secondary")
                          (href "roadmap.html"))
                       "Road Ahead"))))
          #f
          "section--examples section-next-steps")
        ,(footer-section)))

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
          "Short Compiler Overview"
          #f
          (list
           `(div (@ (class "doc-content"))
                 (p "The WebRacket compiler is a direct-style compiler. This choice has made it easier "
                    "to relate the generated code to the source program. In the future we will probably "
                    "need to add a CPS-pass in order to support continuations and continuation marks.")
                 ,(callout
                   'note
                   "Note"
                   `(p "WebRacket stays in direct style to keep the compiler pipeline easy to trace. "
                       "A future CPS pass may be added to support continuations and continuation marks."))
                 (p "The frontend of the WebRacket compiler uses "
                    ,(code-link "https://docs.racket-lang.org/reference/reader.html#%28def._%28%28quote._~23~25kernel%29._read-syntax%29%29"
                                "read-syntax")
                    " to read a WebRacket program from a file. The resulting syntax object is fed into "
                    "the normal Racket expander to produce a program in fully expanded form.")
                 (p "The middle end of the compiler consists of several passes implemented using the "
                    ,(code-link "https://github.com/nanopass/nanopass-framework-racket" "NanoPass")
                    " framework.")
                 (p "The passes are as follows:")
                 (div (@ (class "pass-list-block"))
                      (div (@ (class "pass-list-label pass-list-label--quiet")) "Compiler passes")
                      (pre (@ (class "pass-list"))
                           (code "unexpand\nparse\nflatten-topbegin\ninfer-names\nconvert-quotations\nexplicit-begin\nexplicit-case-lambda\nα-rename\nassignment-conversion\ncategorize-applications\nanormalize\nclosure-conversion\nflatten-begin\n(classify-variables)\ngenerate-code")))
                 (p "See the comments in \"" ,(code-link (gh-file "compiler.rkt") "compiler.rkt")
                    "\" for an explanation of each pass.")
                 ,(callout
                   'note
                   "Why so many passes?"
                   `(p "Small, focused passes keep each transformation understandable, testable, and easier to evolve."))
                 (p (@ (class "doc-spacer-top"))
                    "The code generator generates WebAssembly in the form of " (code "S-expressions")
                    " in the \"folded\" format.")
                 (p "This code generator is inspired by \""
                    ,(ext "https://scholarworks.iu.edu/dspace/handle/2022/34138"
                          "Destination-driven Code Generation")
                    "\" by Dybvig, "
                    "Hieb and Butler. There are some differences, however. The code generator in the paper "
                    "generates \"flat\" code (assembler) whereas we generate nested WebAssembly instructions.")
                 ,(callout
                   'info
                   "Source paper"
                   `(p "Read the full \"Destination-driven Code Generation\" paper on "
                       ,(ext "https://scholarworks.iu.edu/dspace/handle/2022/34138" "IU ScholarWorks")
                       " for background on the generator’s design and terminology."))
                 (p (@ (class "doc-spacer-top"))
                    "Finally, the external tool "
                    ,(code-link "https://github.com/bytecodealliance/wasm-tools" "wasm-tools")
                    " "
                    ,(code-link "https://bytecodealliance.github.io/wasm-tools/parse"
                                "parse")
                    " converts the S-expression representation into bytecode format.")
                 (p "The main part of the compiler is in "
                    ,(code-pill (gh-file "compiler.rkt") "compiler.rkt")
                    ". The WebAssembly runtime is in "
                    ,(code-pill (gh-file "runtime-wasm.rkt") "runtime-wasm.rkt")
                    ". The standard library (implemented in WebRacket) is found in "
                    ,(code-pill (gh-dir "stdlib/") "stdlib/")
                    ". FFI bindings for popular libraries are in "
                    ,(code-pill (gh-dir "ffi/") "ffi/") ".")
                 (p "It has been a design goal to avoid relying on functionality provided by the "
                    "WebAssembly host if possible. Who knows - maybe someone needs a non-JavaScript host "
                    "at some point? For browser functionality there is no way around interfacing with "
                    "the JavaScript host. The JavaScript part of the runtime support is in "
                    ,(code-link (gh-file "assembler.rkt") "assembler.rkt") ".")
                 ,(callout
                   'note
                   "Runtime goal"
                   `(p "The runtime deliberately minimizes host dependencies so WebRacket can target "
                       "non-JavaScript environments when they become viable."))
                 (div (@ (class "doc-cta-card"))
                      ,(callout
                        'info
                        "Further reading"
                        `(p "Explore the compiler and runtime sources to see how WebRacket’s pipeline fits together. "
                            "Want a closer look? Browse the real compiler and runtime code.")
                        `(div (@ (class "doc-cta-group"))
                              (a (@ (class "doc-cta doc-cta--primary")
                                    (href ,(gh-file "compiler.rkt"))
                                    (target "_blank")
                                    (rel "noreferrer noopener"))
                                 "Open compiler.rkt")
                              (a (@ (class "doc-cta")
                                    (href ,(gh-file "runtime-wasm.rkt"))
                                    (target "_blank")
                                    (rel "noreferrer noopener"))
                                 "View runtime")
                              (a (@ (class "doc-cta")
                                    (href ,(gh-dir "stdlib/"))
                                    (target "_blank")
                                    (rel "noreferrer noopener"))
                                 "Browse stdlib"))))))
          #f
          #f)
        ,(footer-section)))

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
               "You’ll need:"
               (list
                `(ul
                  (li "Racket (9.0 or newer)")
                  (li "Node.js")
                  (li "wasm-tools"))
                `(p "If you already installed everything, skip to Step 2.")
                `(p "Otherwise, follow the full " (a (@ (href "installation.html")) "Installation") " guide.")))
             ,(quick-start-step
               2
               "Get WebRacket"
               "Clone the repository and enter it:"
               (list
                `(pre (code "git clone https://github.com/soegaard/webracket.git\ncd webracket"))))
             ,(quick-start-step
               3
               "Run a Demo Site"
               "WebRacket includes ready-made demos. To serve them locally:"
               (list
                `(pre (code "cd examples\nraco static-web"))
                `(p "This starts a local web server.")
                `(p "Now open your browser at:")
                `(p (a (@ (href "http://localhost:8000")) "http://localhost:8000"))))
             ,(quick-start-step
               4
               "Try a Demo"
               "Pick one of the demos and open it in your browser."
               (list
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
               "Each demo has a Racket source file in the examples/ directory."
               (list
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
        ,(section-block
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
          "Short-version checklist for a working WebRacket setup."
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
                      (p "Open http://localhost:8000/ and select an example - find the html file.")))
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
;; Builds and attaches the page DOM plus its CSS styles.
(define (init-dom)
  (define head (js-document-head))
  (define body (js-document-body))

  (define style
    (sxml->dom
     `(style
       ,(format
         #<<CSS
:root {
  --purple: ~a;
  --blue:   ~a;
  --red:    ~a;
  --gold:   ~a;
  --bg:           #0C0D1A;
  --surface:      #14162B;
  --surface-soft: rgba(255, 255, 255, 0.03);
  --text:         #E6E8F2;
  --muted:        #B6BDDD;
  --card-padding: 20px;
}
* { box-sizing: border-box; }
body {
  margin: 0;
  font-family: 'Inter', 'Fira Code', system-ui, sans-serif;
  background: radial-gradient(circle at top, rgba(101, 79, 240, 0.25), transparent 55%), var(--bg);
  color: var(--text);
  line-height: 1.6;
  min-height: 100vh;
}
a { color: var(--blue); text-decoration: none; }
a.code-link { color: inherit; }
a.code-link code { color: inherit; }
a.code-link:hover { text-decoration: underline; }
a.code-link:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.6);
  outline-offset: 2px;
  border-radius: 4px;
}
a.code-pill {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 2px 11px;
  border-radius: 999px;
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid rgba(255, 255, 255, 0.12);
  color: #E9ECFA;
  font-size: 0.85rem;
  font-family: "Fira Code", "JetBrains Mono", ui-monospace, SFMono-Regular, monospace;
  line-height: 1.4;
  cursor: pointer;
  box-shadow: 0 0 0 rgba(101, 79, 240, 0);
  transition: border-color 150ms ease, background 150ms ease, box-shadow 150ms ease, transform 150ms ease;
}
a.code-pill:hover {
  border-color: rgba(255, 255, 255, 0.22);
  background: rgba(255, 255, 255, 0.1);
  box-shadow: 0 0 12px rgba(101, 79, 240, 0.25);
}
a.code-pill:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.7);
  outline-offset: 3px;
}
.page {
  width: min(1200px, 92vw);
  margin: 0 auto;
  padding: 32px 0 80px;
  display: flex;
  flex-direction: column;
  gap: 0;
}
.navbar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 24px;
}
.nav-left {
  display: flex;
  align-items: center;
  gap: 12px;
  font-weight: 600;
  font-size: 1.05rem;
}
.nav-home {
  display: inline-flex;
  align-items: center;
  gap: 12px;
  color: var(--text);
}
.nav-home:hover,
.nav-home:focus-visible {
  opacity: 0.9;
}
.nav-home:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.6);
  outline-offset: 4px;
  border-radius: 999px;
  padding-right: 4px;
}
.nav-logo {
  width: 34px;
  height: 34px;
}
.nav-links {
  display: flex;
  flex-wrap: wrap;
  gap: 16px;
  font-size: 0.95rem;
}
.nav-link {
  color: var(--text);
  opacity: 0.8;
}
.nav-link:hover {
  opacity: 1;
}
.nav-link--active {
  opacity: 1;
  font-weight: 600;
}
.nav-link--active::after {
  content: "";
  display: block;
  height: 2px;
  margin-top: 6px;
  background: rgba(74, 108, 255, 0.85);
  border-radius: 999px;
}
.hero {
  display: grid;
  grid-template-columns: 2fr 1fr;
  gap: 32px;
  align-items: center;
  margin-top: 32px;
}
.hero-panel {
  background: linear-gradient(140deg, rgba(101, 79, 240, 0.2), rgba(74, 108, 255, 0.08));
  border: 1px solid rgba(101, 79, 240, 0.3);
  border-radius: 28px;
  padding: 36px;
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.35);
  display: flex;
  flex-direction: column;
  gap: 18px;
}
.examples-hero {
  margin-top: 32px;
}
.docs-hero {
  margin-top: 32px;
}
.hero-sublead {
  margin: 0;
  color: var(--muted);
  font-size: 0.95rem;
}
.hero-copy {
  flex: none;
}
.hero-title {
  font-size: clamp(2rem, 4vw, 3rem);
  margin: 0 0 12px;
}
.hero-lead {
  /* Hero spacing polish */
  margin: 0 0 22px;
  color: var(--muted);
  line-height: 1.6;
}
.hero-carousel {
  display: flex;
  flex-direction: column;
  gap: 12px;
}
.hero-carousel-panel {
  background: var(--surface);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 24px;
  padding: 20px;
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.35);
  display: flex;
  flex-direction: column;
}
.carousel-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  font-size: 0.8rem;
  color: var(--muted);
}
.carousel-frame {
  position: relative;
  border-radius: 16px;
  overflow: hidden;
  box-shadow: 0 12px 26px rgba(0, 0, 0, 0.35);
  aspect-ratio: 4 / 3;
  background: var(--surface);
}
.carousel-shot {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  object-fit: contain;
  opacity: 0;
  transform: scale(1.02);
  animation: carouselFade 30s infinite;
}
@keyframes carouselFade {
  0% { opacity: 0; transform: scale(1.02); }
  6% { opacity: 1; transform: scale(1); }
  18% { opacity: 1; transform: scale(1); }
  26% { opacity: 0; transform: scale(1.02); }
  100% { opacity: 0; transform: scale(1.02); }
}
.pill-row {
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
  /* Hero spacing polish */
  margin-bottom: 8px;
}
.pill {
  border-radius: 999px;
  padding: 6px 14px;
  background: var(--surface-soft);
  border: 1px solid rgba(255, 255, 255, 0.08);
  color: var(--muted);
  font-size: 0.85rem;
}
.install-hero {
  margin-top: 32px;
}
.install-hero-panel {
  background: linear-gradient(145deg, rgba(101, 79, 240, 0.18), rgba(74, 108, 255, 0.08));
  border: 1px solid rgba(101, 79, 240, 0.28);
  border-radius: 28px;
  padding: 36px;
  display: flex;
  flex-direction: column;
  gap: 16px;
  box-shadow: 0 18px 36px rgba(0, 0, 0, 0.35);
}
.install-hero-note {
  margin: 0;
  color: var(--muted);
  max-width: 70ch;
}
.install-grid {
  display: grid;
  gap: 16px;
  margin: 0;
}
.install-grid-item {
  background: var(--surface);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 18px;
  padding: 16px 18px;
  display: flex;
  align-items: flex-start;
  gap: 12px;
  box-shadow: 0 12px 24px rgba(0, 0, 0, 0.24);
}
.install-grid-icon {
  width: 24px;
  height: 24px;
  border-radius: 999px;
  background: rgba(101, 79, 240, 0.25);
  border: 1px solid rgba(101, 79, 240, 0.45);
  color: #F5F6FF;
  display: grid;
  place-items: center;
  font-size: 0.8rem;
  font-weight: 600;
  flex-shrink: 0;
  margin-top: 2px;
}
.install-grid-text {
  color: var(--text);
  font-size: 0.95rem;
  line-height: 1.6;
}
.install-steps {
  display: grid;
  gap: 24px;
}
.install-step-card {
  background: var(--surface);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 24px;
  padding: 26px;
  box-shadow: 0 16px 30px rgba(0, 0, 0, 0.3);
  position: relative;
  overflow: hidden;
  display: flex;
  flex-direction: column;
  gap: 18px;
}
/* Installation: spacing polish */
.install-step-card p + pre,
.install-step-card pre + pre,
.install-step-card pre + p,
.install-step-card p + .callout {
  margin-top: 4px;
}
/* Installation: progression cue */
.install-steps .install-step-card::after {
  content: "";
  position: absolute;
  inset: 0;
  border-radius: inherit;
  background: radial-gradient(130% 120% at 10% 8%, rgba(101, 79, 240, 0.08), rgba(101, 79, 240, 0) 62%);
  pointer-events: none;
}
.install-steps .install-step-card:nth-child(2)::after {
  background: radial-gradient(130% 120% at 25% 8%, rgba(101, 79, 240, 0.07), rgba(101, 79, 240, 0) 62%);
}
.install-steps .install-step-card:nth-child(3)::after {
  background: radial-gradient(130% 120% at 40% 8%, rgba(101, 79, 240, 0.065), rgba(101, 79, 240, 0) 62%);
}
.install-steps .install-step-card:nth-child(4)::after {
  background: radial-gradient(130% 120% at 60% 8%, rgba(101, 79, 240, 0.06), rgba(101, 79, 240, 0) 62%);
}
.install-steps .install-step-card:nth-child(5)::after {
  background: radial-gradient(130% 120% at 75% 8%, rgba(101, 79, 240, 0.055), rgba(101, 79, 240, 0) 62%);
}
.install-steps .install-step-card:nth-child(6)::after {
  background: radial-gradient(130% 120% at 90% 8%, rgba(101, 79, 240, 0.07), rgba(101, 79, 240, 0) 62%);
}
.install-step-card h3 {
  margin: 0;
  font-size: 1.25rem;
  color: #F7F8FF;
}
.install-step-card p {
  margin: 0;
  color: var(--muted);
}
/* Installation: troubleshooting callout refinement */
.callout {
  background: rgba(101, 79, 240, 0.065);
  border: 1px solid rgba(101, 79, 240, 0.18);
  border-left: 3px solid hsla(var(--accent-h, 252), 78%, 72%, 0.6);
  border-radius: 16px;
  padding: 12px 16px;
  color: rgba(207, 214, 238, 0.92);
  font-size: 0.92rem;
  line-height: 1.7;
  margin: 0;
  box-shadow: 0 10px 22px rgba(0, 0, 0, 0.18);
}
.callout--note {
  background: rgba(101, 79, 240, 0.065);
  border-color: rgba(101, 79, 240, 0.18);
}
.callout--info {
  background: rgba(74, 108, 255, 0.065);
  border-color: rgba(74, 108, 255, 0.2);
}
.callout--warn {
  background: rgba(242, 183, 5, 0.09);
  border-color: rgba(242, 183, 5, 0.2);
}
.section .callout {
  margin: 18px 0;
}
.install-step-card .callout {
  margin: 0;
}
.callout-title {
  display: flex;
  align-items: center;
  gap: 8px;
  color: #F2F4FF;
  font-weight: 600;
  margin: 0;
  line-height: 1.2;
}
.callout-icon {
  width: 20px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  flex-shrink: 0;
  opacity: 0.9;
}
.callout-title + * {
  margin-top: 4px;
}
.callout p,
.callout pre {
  margin: 0;
}
.callout p + p,
.callout p + pre,
.callout pre + p,
.callout pre + pre {
  margin-top: 6px;
}
.callout a {
  overflow-wrap: anywhere;
}
/* Installation: Step 6 completion emphasis */
.install-step-card:nth-child(6) {
  border-color: rgba(101, 79, 240, 0.32);
  box-shadow: 0 18px 34px rgba(0, 0, 0, 0.34);
}
.install-step-card:nth-child(6) h3::after {
  content: " ✓";
  color: rgba(230, 232, 242, 0.7);
  font-size: 0.95rem;
}
.install-next-steps {
  margin-top: 32px;
  background: var(--surface);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 22px;
  padding: 22px 24px;
  box-shadow: 0 14px 26px rgba(0, 0, 0, 0.28);
  display: flex;
  flex-direction: column;
  gap: 10px;
}
.install-next-steps h3 {
  margin: 0;
  font-size: 1.15rem;
  color: #F7F8FF;
}
.install-next-steps p {
  margin: 0;
  color: var(--muted);
}
.install-next-steps-list {
  margin: 0;
  padding-left: 18px;
  color: var(--muted);
  display: grid;
  gap: 6px;
}
pre {
  background: #0B0D1D;
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 14px;
  padding: 14px 16px;
  color: #F7F8FF;
  font-size: 0.9rem;
  margin: 0;
  white-space: pre-wrap;
  word-break: break-word;
}
.pass-list-block {
  display: flex;
  flex-direction: column;
  gap: 8px;
  max-width: 72ch;
}
.pass-list-label {
  font-size: 0.78rem;
  letter-spacing: 0.02em;
  text-transform: none;
  color: var(--muted);
}
.pass-list {
  padding: 16px 18px;
  line-height: 1.75;
}
.pass-list-label--quiet {
  opacity: 0.72;
}
.doc-content {
  display: flex;
  flex-direction: column;
  gap: 0;
}
.page--docs .doc-spacer-top {
  margin-top: 18px;
}
.page--docs .callout {
  box-shadow: 0 9px 20px rgba(0, 0, 0, 0.16);
}
.page--docs .card {
  box-shadow: 0 12px 24px rgba(0, 0, 0, 0.22);
}
.page--docs .card:hover,
.page--docs .card:focus-within {
  box-shadow: 0 16px 30px rgba(0, 0, 0, 0.26);
}
.page--quick-start .docs-hero .hero-panel {
  padding: 32px;
  gap: 16px;
}
.page--quick-start {
  --qs-step-padding-left: 26px;
  --qs-step-padding-right: 20px;
  --qs-step-padding-y: 14px;
  --qs-step-badge-size: 28px;
  --qs-step-gap: 12px;
}
.page--quick-start .hero-lead {
  margin-bottom: 18px;
}
.page--quick-start .section {
  margin-top: 66px;
  padding: 26px 24px;
}
.page--quick-start .section--quick-start-step {
  margin-top: 0;
  padding: var(--qs-step-padding-y) var(--qs-step-padding-right) var(--qs-step-padding-y) var(--qs-step-padding-left);
  background: var(--surface);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 20px;
  box-shadow: 0 12px 24px rgba(0, 0, 0, 0.22);
  position: relative;
  z-index: 1;
}
.page--quick-start .section--quick-start-what {
  padding-bottom: 22px;
}
.page--quick-start .section--quick-start-what ul {
  margin: 0;
  padding-left: 18px;
}
.page--quick-start .section--quick-start-what ul li {
  margin: 0;
  line-height: 1.55;
}
.page--quick-start .section--quick-start-what ul li + li {
  margin-top: 4px;
}
.page--quick-start .section-header {
  margin-bottom: 12px;
  gap: 5px;
}
.page--quick-start .section--quick-start-what .section-header {
  margin-bottom: 10px;
  gap: 4px;
}
.page--quick-start .section-title + * {
  margin-top: 8px;
}
.page--quick-start .section > p {
  margin: 0 0 12px;
}
.page--quick-start .section > p:last-child {
  margin-bottom: 0;
}
.page--quick-start .section > pre {
  margin: 8px 0 12px;
}
.page--quick-start .section > pre:last-child {
  margin-bottom: 0;
}
.page--quick-start .section > p + pre {
  margin-top: 6px;
}
.page--quick-start .section > pre + p {
  margin-top: 6px;
}
.page--quick-start .section--quick-start-step .section-title {
  font-weight: 675;
}
.page--quick-start .section--quick-start-step .qs-step-title::after {
  content: none;
}
.page--quick-start .section--quick-start-step .qs-step-header {
  flex-direction: row;
  align-items: baseline;
  gap: 10px;
  margin-bottom: 6px;
}
.page--quick-start .qs-step-badge {
  width: var(--qs-step-badge-size);
  height: var(--qs-step-badge-size);
  font-size: 0.85rem;
  font-weight: 600;
  transform: none;
  margin-top: 0;
  line-height: 1;
  align-self: baseline;
  background: rgba(101, 79, 240, 0.3);
  border: 1px solid rgba(101, 79, 240, 0.45);
  box-shadow: 0 6px 12px rgba(0, 0, 0, 0.28), 0 0 0 1px rgba(255, 255, 255, 0.1);
}
.page--quick-start .quick-start-steps {
  counter-reset: step;
  display: flex;
  flex-direction: column;
  gap: var(--qs-step-gap);
  margin-top: 36px;
  position: relative;
}
.page--quick-start .qs-step-titleblock {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  gap: 1px;
  flex: 1;
  min-width: 0;
}
.page--quick-start .qs-step-title {
  font-size: 1.28rem;
  line-height: 1.2;
  color: #fbfbff;
  font-weight: 600;
}
.page--quick-start .qs-step-title + .qs-step-intro {
  margin-top: 0;
}
.page--quick-start .qs-step-intro {
  margin: 0;
  color: rgba(182, 189, 221, 0.92);
  font-size: 0.98rem;
  line-height: 1.45;
}
.page--quick-start .qs-step-body {
  display: flex;
  flex-direction: column;
  gap: 4px;
  line-height: 1.6;
}
.page--quick-start .qs-step-body > p {
  margin: 0;
}
.page--quick-start .qs-step-body > ul {
  margin: 0;
  padding-left: 18px;
  color: rgba(245, 247, 255, 0.86);
}
.page--quick-start .qs-step-body > ul li {
  margin: 0;
  line-height: 1.5;
}
.page--quick-start .qs-step-body > ul li + li {
  margin-top: 1px;
}
.page--quick-start .qs-step-body > pre {
  margin: 1px 0;
  padding: 6px 9px;
  font-size: 0.88rem;
}
.page--quick-start .qs-step-body > p + pre,
.page--quick-start .qs-step-body > ul + pre,
.page--quick-start .qs-step-body > pre + p,
.page--quick-start .qs-step-body > pre + ul {
  margin-top: 3px;
}
.page--quick-start .qs-step-body > pre:last-child,
.page--quick-start .qs-step-body > p:last-child,
.page--quick-start .qs-step-body > ul:last-child {
  margin-bottom: 0;
}
.page--quick-start .section--quick-start-callout .callout {
  margin: 14px 0 0;
}
.card-grid--quick-start .card {
  align-items: flex-start;
}
.card-grid--quick-start .card .doc-cta-group {
  margin-top: auto;
}
.page--status {
  --status-card-padding: 10px;
  --status-card-gap: 5.2px;
  --status-stack-gap-base: 2.6px;
  --status-stack-gap: var(--status-stack-gap-base);
  --status-section-spacing: 30px;
  --status-section-padding-y: 22.5px;
  --status-section-padding-x: 20px;
  --status-section-header-gap: calc(var(--status-card-gap) * 1.3);
  --status-section-title-gap: calc(var(--status-card-gap) * 0.9);
  --status-progress-height: 2.1px;
  --status-progress-label-height: 1.05rem;
  --status-progress-gap: calc(var(--status-stack-gap) - 0.4px);
  --status-row-padding: 1px;
  --status-progress-row-height: calc(var(--status-progress-label-height) + var(--status-progress-height) + var(--status-stack-gap));
  --status-title-line-height: 1.2em;
  --status-meta-line-height: 1.2em;
  --status-desc-line-height: 1.35em;
  --status-cta-line-height: 1.1em;
  --status-cta-height: 26px;
  --status-attention-padding: 7px;
  --status-attention-gap: 3px;
}
.page--status .card {
  background: rgba(255, 255, 255, 0.03);
  padding: var(--status-card-padding);
  gap: var(--status-card-gap);
}
.page--status .card:nth-child(even) {
  padding-top: var(--status-card-padding);
}
.page--status .card-grid {
  gap: calc(var(--status-card-gap) * 2);
}
.page--status .card-grid > * {
  min-width: 0;
}
.status-hero {
  margin-top: 32px;
}
.page--status .section {
  padding: var(--status-section-padding-y) var(--status-section-padding-x);
}
.page--status .section-title::after {
  height: 4px;
  width: 76px;
  opacity: 0.7;
}
.section--status {
  margin-top: var(--status-section-spacing);
}
.section--status .section-header {
  margin-bottom: var(--status-section-header-gap);
}
.section--status .section-title + * {
  margin-top: var(--status-section-title-gap);
}
.status-insight {
  margin-top: calc(var(--status-card-gap) * 1.3);
}
.status-insight .callout {
  box-shadow: 0 12px 24px rgba(0, 0, 0, 0.28);
}
.hero-note {
  margin: 8px 0 0;
  color: rgba(182, 189, 221, 0.82);
  font-size: 0.92rem;
}
.status-summary-grid .status-metric {
  margin: 0 0 12px;
  font-size: clamp(2rem, 3vw, 2.6rem);
  font-weight: 600;
  color: var(--text);
}
.attention-grid .card {
  --status-stack-gap: calc(var(--status-stack-gap-base) - 1px);
  padding: var(--status-attention-padding);
  gap: var(--status-stack-gap);
}
.attention-grid {
  grid-template-columns: minmax(0, 1fr);
}
@media (min-width: 680px) {
  .attention-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}
@media (min-width: 1180px) {
  .attention-grid {
    grid-template-columns: repeat(3, minmax(0, 1fr));
  }
}
.attention-header {
  display: flex;
  justify-content: space-between;
  align-items: baseline;
  gap: 12px;
  min-width: 0;
  padding: var(--status-row-padding) 0;
  flex: 0 0 auto;
}
.attention-header h3 {
  margin: 0;
  line-height: var(--status-title-line-height);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  min-width: 0;
}
.attention-percent {
  font-weight: 600;
  color: var(--text);
  font-size: 0.95rem;
  line-height: var(--status-title-line-height);
}
.attention-count {
  margin: 0;
  color: var(--muted);
  font-size: 0.85rem;
  line-height: var(--status-meta-line-height);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.attention-note {
  margin: 0;
  color: rgba(182, 189, 221, 0.92);
  font-size: 0.9rem;
  line-height: var(--status-desc-line-height);
  min-height: calc(var(--status-desc-line-height) * 2);
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
}
.attention-link {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  font-size: 0.85rem;
  font-weight: 600;
  color: #C9D5FF;
  text-decoration: none;
  border-radius: 999px;
  padding: 0 calc(var(--status-attention-gap) + 4px);
  background: rgba(74, 108, 255, 0.15);
  border: 1px solid rgba(74, 108, 255, 0.35);
  transition: transform 150ms ease, box-shadow 150ms ease, color 150ms ease;
  min-height: var(--status-cta-height);
  padding-block: var(--status-row-padding);
  flex: 0 0 auto;
}
.attention-link:hover,
.attention-link:focus-visible {
  color: #F7F8FF;
  box-shadow: 0 10px 20px rgba(74, 108, 255, 0.18);
  transform: translateY(-1px);
}
.attention-link:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.7);
  outline-offset: 3px;
}
.status-legend {
  display: flex;
  flex-direction: column;
  gap: var(--status-card-gap);
  padding: var(--status-card-padding);
  border-radius: 16px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  background: rgba(255, 255, 255, 0.02);
  color: var(--muted);
  font-size: 0.85rem;
}
.status-legend-title {
  font-size: 0.75rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: rgba(182, 189, 221, 0.7);
}
.status-legend-list {
  list-style: none;
  margin: 0;
  padding: 0;
  display: grid;
  gap: 4px;
}
.status-legend-term {
  color: #DCE2FF;
  font-weight: 600;
}
.status-helper {
  margin: calc(var(--status-card-gap) / 2) 0 0;
  color: rgba(182, 189, 221, 0.8);
  font-size: 0.9rem;
}
.status-chapters {
  display: flex;
  flex-direction: column;
  gap: calc(var(--status-card-gap) * 1.5);
}
.status-chapter {
  display: flex;
  flex-direction: column;
  gap: calc(var(--status-card-gap) * 1.2);
}
.status-chapter-header h3 {
  margin: 0;
  font-size: 1.4rem;
}
.status-chapter-grid {
  display: grid;
  grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: calc(var(--status-card-gap) * 1.6);
  align-items: stretch;
}
@media (max-width: 1100px) {
  .status-chapter-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}
@media (max-width: 700px) {
  .status-chapter-grid {
    grid-template-columns: minmax(0, 1fr);
  }
}
.status-chapter-grid > * {
  width: 100%;
  min-width: 0;
}
.status-section {
  background: var(--surface);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 20px;
  transition: transform 150ms ease, border-color 150ms ease;
  min-width: 0;
  width: 100%;
}
.status-section[open] {
  grid-column: 1 / -1;
  z-index: 2;
  overflow: visible;
}
.status-summary {
  display: grid;
  grid-template-columns: minmax(0, 1fr) auto;
  column-gap: calc(var(--status-card-gap) * 1.6);
  row-gap: var(--status-stack-gap);
  align-items: center;
  padding: var(--status-card-padding);
  cursor: pointer;
  list-style: none;
  transition: background 150ms ease, border-color 150ms ease;
  --meter-w: 150px;
}
.status-summary > * {
  min-width: 0;
}
.status-summary:hover {
  background: rgba(74, 108, 255, 0.08);
}
.status-summary:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.7);
  outline-offset: 4px;
}
.status-summary::-webkit-details-marker {
  display: none;
}
.status-summary-main {
  display: flex;
  flex-direction: column;
  gap: var(--status-stack-gap);
  min-width: 0;
}
.status-title {
  margin: 0;
  font-size: 1.05rem;
  line-height: var(--status-title-line-height);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  min-width: 0;
}
.status-count {
  margin: 0;
  color: var(--muted);
  font-size: 0.85rem;
  line-height: var(--status-meta-line-height);
  overflow: hidden;
  text-overflow: ellipsis;
  min-height: calc(var(--status-meta-line-height) * 2);
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
}
.status-summary-metric {
  display: flex;
  flex-direction: column;
  align-items: flex-end;
  justify-content: center;
  gap: var(--status-progress-gap);
  min-width: 0;
  min-height: var(--status-progress-row-height);
  padding: var(--status-row-padding) 0;
  flex: 0 0 auto;
}
.status-summary-action {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  color: var(--muted);
  font-size: 0.75rem;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  min-height: var(--status-cta-line-height);
  line-height: var(--status-cta-line-height);
  padding: var(--status-row-padding) 0;
  flex: 0 0 auto;
}
.status-summary-chevron {
  font-size: 1rem;
  transition: transform 150ms ease, color 150ms ease;
}
.status-section[open] .status-summary-chevron {
  transform: rotate(90deg);
  color: #E9ECFF;
}
.status-percent {
  font-weight: 600;
  color: var(--text);
  line-height: var(--status-progress-label-height);
}
.status-bar {
  width: min(var(--meter-w), 100%);
  max-width: var(--meter-w);
  height: var(--status-progress-height);
  background: rgba(255, 255, 255, 0.1);
  border-radius: 999px;
  overflow: hidden;
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.03);
  --pct: 0;
  --p: clamp(0, calc(var(--pct) / 100), 1);
  --p-safe: max(var(--p), 0.001);
}
.status-bar-fill {
  height: 100%;
  width: calc(var(--p) * 100%);
  background: var(--progress-gradient);
  background-size: calc(100% / var(--p-safe)) 100%;
  background-position: 0 50%;
  background-repeat: no-repeat;
  border-radius: inherit;
  box-shadow: 0 0 2px rgba(74, 108, 255, 0.06);
  transition: width 220ms ease;
  --progress-gradient: linear-gradient(90deg, rgba(209, 58, 58, 0.5),
                                               rgba(242, 183, 5, 0.5),
                                               rgba(74, 108, 255, 0.52));
}
.status-bar-fill--low {
  --progress-gradient: linear-gradient(90deg, rgba(209, 58, 58, 0.54),
                                               rgba(242, 183, 5, 0.4));
  box-shadow: 0 0 3px rgba(209, 58, 58, 0.08);
}
.status-bar-fill--mid {
  --progress-gradient: linear-gradient(90deg, rgba(242, 183, 5, 0.44),
                                               rgba(74, 108, 255, 0.48));
  box-shadow: 0 0 3px rgba(74, 108, 255, 0.06);
}
.status-bar-fill--strong {
  --progress-gradient: linear-gradient(90deg, rgba(74, 108, 255, 0.52),
                                               rgba(101, 79, 240, 0.54));
  box-shadow: 0 0 3px rgba(101, 79, 240, 0.08);
}
@media (min-width: 720px) and (max-width: 999px) {
  .status-summary {
    --meter-w: 220px;
  }
}
@media (max-width: 719px) {
  .status-summary {
    grid-template-columns: 1fr;
    row-gap: 8px;
    --meter-w: 100%;
  }
  .status-title {
    overflow: hidden;
    text-overflow: ellipsis;
  }
}
.status-body {
  padding: calc(var(--status-card-gap) / 2) var(--status-card-padding) calc(var(--status-card-gap) * 0.75);
  min-width: 0;
}
.status-section[open] .status-body {
  margin-top: calc(var(--status-card-gap) * 0.8);
  padding-top: calc(var(--status-card-gap) * 0.8);
  border-top: 1px solid rgba(255, 255, 255, 0.06);
  background: rgba(255, 255, 255, 0.02);
}
.status-body-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  flex-wrap: wrap;
  padding: 0 2px calc(var(--status-card-gap) / 3);
}
.status-body-legend {
  display: flex;
  flex-wrap: wrap;
  gap: var(--status-card-gap);
}
.status-body-hint {
  color: rgba(182, 189, 221, 0.75);
  font-size: 0.72rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  background: transparent;
  border: 0;
  padding: 0;
  cursor: pointer;
  font: inherit;
}
.status-body-hint:hover {
  color: rgba(210, 217, 245, 0.95);
}
.status-body-hint:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.6);
  outline-offset: 2px;
  border-radius: 999px;
}
.status-list {
  list-style: none;
  padding: 0;
  margin: 0;
  display: grid;
  gap: calc(var(--status-card-gap) / 2);
  margin-top: calc(var(--status-card-gap) / 4);
  overflow-x: auto;
  max-width: 100%;
  min-width: 0;
  scrollbar-color: rgba(255, 255, 255, 0.18) transparent;
}
.status-list::-webkit-scrollbar {
  height: 8px;
}
.status-section .status-list,
.status-section .status-list > li {
  list-style: none;
  margin: 0;
  padding: 0;
}
.status-section .status-list > li {
  border: 0;
  background: transparent;
}
.prim-row {
  display: grid;
  grid-template-columns: 132px minmax(0, 1fr);
  align-items: center;
  column-gap: 12px;
  padding: calc(var(--status-card-gap) - 3px) calc(var(--status-card-gap) + 1px);
  border-radius: 10px;
  background: rgba(255, 255, 255, 0.02);
  border: 1px solid rgba(255, 255, 255, 0.035);
  width: max-content;
  min-width: 100%;
  white-space: nowrap;
}
.prim-row--link {
  text-decoration: none;
  color: inherit;
  transition: background 150ms ease, border-color 150ms ease;
}
.prim-row--link:hover {
  background: rgba(74, 108, 255, 0.06);
  border-color: rgba(74, 108, 255, 0.2);
}
.prim-row--link:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.7);
  outline-offset: 3px;
}
.prim-badge {
  justify-self: start;
  width: 132px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  padding: 4px 10px;
  border-radius: 999px;
  font-size: 0.74rem;
  letter-spacing: 0.06em;
  line-height: 1.1;
}
.prim-name {
  min-width: 0;
  text-align: left;
  white-space: nowrap;
  overflow: visible;
  text-overflow: clip;
  justify-self: start;
  font-family: "Fira Code", "JetBrains Mono", ui-monospace, SFMono-Regular, monospace;
  font-size: 0.9rem;
}
.status-chip {
  border-radius: 999px;
  padding: 2px 8px;
  font-size: 0.7rem;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  font-weight: 600;
}
.status-chip--filter {
  cursor: pointer;
}
.status-chip--filter:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.6);
  outline-offset: 2px;
}
.status-chip--active {
  box-shadow: 0 0 0 1px rgba(233, 236, 255, 0.6);
}
.status-chip--done {
  background: rgba(74, 108, 255, 0.18);
  color: #C9D5FF;
  border: 1px solid rgba(74, 108, 255, 0.35);
}
.status-chip--stdlib {
  background: rgba(242, 183, 5, 0.18);
  color: #F7E3A1;
  border: 1px solid rgba(242, 183, 5, 0.35);
}
.status-chip--todo {
  background: rgba(209, 58, 58, 0.18);
  color: #F7B1B1;
  border: 1px solid rgba(209, 58, 58, 0.35);
}
.status-cta {
  display: flex;
  flex-direction: column;
  gap: 10px;
  padding: 7.5px 18px;
  background: rgba(255, 255, 255, 0.02);
  border-radius: 20px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  box-shadow: 0 12px 24px rgba(0, 0, 0, 0.24);
}
.status-cta-actions {
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
  align-items: center;
}
code {
  font-family: "Fira Code", "JetBrains Mono", ui-monospace, SFMono-Regular, monospace;
}
pre code {
  white-space: pre-wrap;
}
.section {
  position: relative;
  display: flex;
  flex-direction: column;
  gap: 0;
  margin: 72px 0 0;
  padding: 28px 24px;
  border-radius: 28px;
  overflow: hidden;
}
.install-section {
  margin-top: 84px;
}
.section::before {
  content: "";
  position: absolute;
  inset: 0;
  border-radius: inherit;
  background: var(--section-band, radial-gradient(120% 120% at 10% 0%, rgba(101, 79, 240, 0.16), rgba(12, 13, 26, 0.15) 48%, transparent 72%)); /* Section accent band token */
  opacity: var(--section-band-opacity, 0.6);
  pointer-events: none;
}
.section > * {
  position: relative;
  z-index: 1;
}
.section-header {
  display: flex;
  flex-direction: column;
  gap: 6px;
  margin-bottom: 22px;
}
.section-title {
  margin: 0;
  font-size: 1.75rem;
  font-weight: 600;
  letter-spacing: -0.01em;
  line-height: 1.2;
}
.section-title + * {
  margin-top: 22px;
}
.section-lead {
  margin: 0;
  color: var(--muted);
  line-height: 1.6;
  max-width: 72ch;
}
.section p {
  max-width: 72ch;
}
.section ul {
  padding-left: 26px;
  margin: 4px 0 0;
  max-width: 72ch;
}
.section li {
  margin-bottom: 12px;
  line-height: 1.68;
  color: var(--muted);
}
.section li:last-child { margin-bottom: 0; }
.card-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 22px;
}
.card {
  background: var(--surface);
  border: 1px solid var(--section-card-border, rgba(255, 255, 255, 0.08)); /* Per-section card tint */
  border-radius: 20px;
  padding: var(--card-padding);
  display: flex;
  flex-direction: column;
  gap: 12px;
  box-shadow: 0 14px 28px rgba(0, 0, 0, 0.25);
  transition: border-color 200ms ease, transform 200ms ease, box-shadow 200ms ease;
}
.card:nth-child(even) {
  border-color: var(--section-card-border-strong, rgba(255, 255, 255, 0.12));
  padding-top: 22px;
}
.card:hover,
.card:focus-within {
  border-color: var(--section-card-border-hover, rgba(255, 255, 255, 0.18));
  transform: translateY(-2px);
  box-shadow: 0 18px 32px rgba(0, 0, 0, 0.3);
}
.card h3 {
  margin: 0;
  font-size: 1.18rem;
  font-weight: 600;
  letter-spacing: -0.01em;
  color: #F6F7FF;
}
.card h3::after {
  content: "";
  display: block;
  height: 1px;
  margin-top: 8px;
  background: rgba(255, 255, 255, 0.08);
}
.card p {
  margin: 0;
  color: rgba(182, 189, 221, 0.92);
  line-height: 1.65;
  font-size: 0.94rem;
  max-width: none;
}
.steps {
  display: grid;
  gap: 14px;
  background: var(--surface);
  border-radius: 18px;
  padding: 26px 28px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  max-width: 70ch;
}
.steps li { margin-bottom: 12px; color: var(--muted); line-height: 1.65; }
.steps li:last-child { margin-bottom: 0; }
.section-title::after {
  content: "";
  display: block;
  height: 3px;
  width: 64px;
  margin-top: 10px;
  background: hsla(var(--accent-h, 252), 78%, 72%, var(--accent-a, 0.55));
  border-radius: 999px;
  pointer-events: none;
}

/* Section bands + per-section accent tokens */
.section {
  overflow: hidden;
  overflow: clip;
  --accent-h: 252;
  --accent-a: 0.55;
  --accent-band-a: 0.1;
  --accent-border-a: 0.1;
  --accent-border-strong-a: 0.14;
  --accent-border-hover-a: 0.18;
  --section-band-opacity: 0.65;
  --section-band: radial-gradient(140% 120% at 12% -10%, hsla(var(--accent-h), 70%, 66%, var(--accent-band-a)), rgba(12, 13, 26, 0) 60%);
  --section-card-border: hsla(var(--accent-h), 75%, 75%, var(--accent-border-a));
  --section-card-border-strong: hsla(var(--accent-h), 75%, 75%, var(--accent-border-strong-a));
  --section-card-border-hover: hsla(var(--accent-h), 75%, 80%, var(--accent-border-hover-a));
}
.section::before {
  inset: -18%;
  opacity: var(--section-band-opacity);
  filter: blur(22px);
  z-index: 0;
}
.section--coverage {
  --accent-h: 262;
}
.section--toolchain {
  --accent-h: 244;
}
.section--pipeline {
  --accent-h: 232;
}
.section--roadmap {
  --accent-h: 250;
}
.section--examples {
  --accent-h: 238;
  --example-card-border: hsla(var(--accent-h), 75%, 75%, 0.09);
  --example-card-shadow: 0 13px 26px rgba(0, 0, 0, 0.23);
  --example-divider: rgba(255, 255, 255, 0.084);
}
.doc-cta-group {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
}
.doc-cta {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  padding: 8px 14px;
  border-radius: 999px;
  border: 1px solid rgba(255, 255, 255, 0.16);
  background: rgba(255, 255, 255, 0.06);
  color: #F4F6FF;
  font-size: 0.85rem;
  font-weight: 600;
  transition: transform 150ms ease, border-color 150ms ease, background 150ms ease;
}
.doc-cta:hover,
.doc-cta:focus-visible {
  border-color: rgba(255, 255, 255, 0.28);
  background: rgba(255, 255, 255, 0.12);
  transform: translateY(-1px);
}
.doc-cta:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.7);
  outline-offset: 3px;
}
.doc-cta--primary {
  background: linear-gradient(120deg, rgba(101, 79, 240, 0.9), rgba(74, 108, 255, 0.8));
  border-color: rgba(74, 108, 255, 0.55);
  box-shadow: 0 10px 20px rgba(74, 108, 255, 0.22);
  font-weight: 700;
}
.doc-cta-card .callout-title {
  font-size: 1rem;
}

/* Coverage grid */
.coverage-grid {
  gap: 24px;
}
.coverage-grid .card {
  padding: 22px;
}
.coverage-grid .card:nth-child(even) {
  background: rgba(255, 255, 255, 0.02);
  border-color: hsla(var(--accent-h), 75%, 80%, 0.14);
}
.coverage-grid .card h3 {
  font-size: 1.18rem;
  color: #F8F9FF;
}
.coverage-grid .card p {
  font-size: 0.92rem;
  line-height: 1.62;
}
@supports (-webkit-line-clamp: 2) {
  .coverage-grid .card p {
    display: -webkit-box;
    -webkit-line-clamp: 2;
    -webkit-box-orient: vertical;
    overflow: hidden;
  }
}
@media (min-width: 1000px) {
  .coverage-grid {
    grid-template-columns: repeat(3, minmax(0, 1fr));
  }
}
@media (min-width: 720px) and (max-width: 999px) {
  .coverage-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}

/* Toolchain panel */
.toolchain-panel {
  background: var(--surface);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 20px;
  padding: 30px;
  max-width: 70ch;
  box-shadow: 0 16px 30px rgba(0, 0, 0, 0.25);
  display: flex;
  flex-direction: column;
  gap: 16px;
}
.toolchain-lede {
  margin: 0;
  color: var(--text);
  font-size: 1rem;
  line-height: 1.6;
}
.toolchain-list {
  list-style: none;
  padding-left: 0;
  margin: 0;
}
.toolchain-list li {
  margin-bottom: 12px;
  padding-left: 26px;
  position: relative;
  line-height: 1.7;
}
.toolchain-list li::before {
  content: "✓";
  position: absolute;
  left: 0;
  top: 0;
  color: rgba(101, 79, 240, 0.85);
  font-weight: 700;
}

/* Pipeline flow */
.pipeline-grid {
  counter-reset: step;
  position: relative;
}
.pipeline-grid .card {
  position: relative;
  padding-top: 20px;
}
/* Pipeline: inline step badge + title */
.pipeline-grid .card::before {
  content: none;
}
.pipeline-header {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-bottom: 8px;
}
.pipeline-step {
  counter-increment: step;
  width: 28px;
  height: 28px;
  border-radius: 999px;
  display: grid;
  place-items: center;
  font-size: 0.85rem;
  font-weight: 600;
  color: #F3F4FF;
  background: rgba(101, 79, 240, 0.3);
  border: 1px solid rgba(101, 79, 240, 0.45);
  box-shadow: 0 8px 16px rgba(0, 0, 0, 0.3), 0 0 0 1px rgba(255, 255, 255, 0.12);
  transform: translate(-2px, -6px);
  flex-shrink: 0;
}
.pipeline-step::before {
  content: counter(step);
}
.pipeline-title {
  margin: 0;
  line-height: 1.2;
}
.pipeline-grid .card > * {
  position: relative;
  z-index: 1;
}
/* Subtle directional cue */
.pipeline-grid .card::after {
  content: "";
  position: absolute;
  inset: 0;
  border-radius: inherit;
  background: radial-gradient(120% 120% at 12% 8%, rgba(101, 79, 240, 0.08), rgba(101, 79, 240, 0) 60%);
  pointer-events: none;
  z-index: 0;
}
.pipeline-grid .card:nth-child(2)::after {
  background: radial-gradient(120% 120% at 38% 8%, rgba(101, 79, 240, 0.08), rgba(101, 79, 240, 0) 60%);
}
.pipeline-grid .card:nth-child(3)::after {
  background: radial-gradient(120% 120% at 62% 8%, rgba(101, 79, 240, 0.08), rgba(101, 79, 240, 0) 60%);
}
.pipeline-grid .card:nth-child(4)::after {
  background: radial-gradient(120% 120% at 88% 8%, rgba(101, 79, 240, 0.08), rgba(101, 79, 240, 0) 60%);
}
@media (min-width: 1000px) {
  .pipeline-grid::after {
    content: none;
  }
}

/* Hero CTA */
.hero-cta {
  display: flex;
  flex-wrap: wrap;
  gap: 14px;
  align-items: center;
  /* Hero spacing polish */
  margin-top: 12px;
}
.cta-button {
  display: inline-flex;
  align-items: center;
  gap: 10px;
  padding: 10px 18px;
  border-radius: 999px;
  background: linear-gradient(120deg, rgba(101, 79, 240, 0.95), rgba(74, 108, 255, 0.85));
  border: 1px solid rgba(74, 108, 255, 0.6);
  color: #F9FAFF;
  font-weight: 600;
  font-size: 0.95rem;
  box-shadow: 0 12px 24px rgba(74, 108, 255, 0.25);
  position: relative;
  overflow: hidden;
}
.cta-button:focus-visible {
  outline: 2px solid rgba(242, 183, 5, 0.8);
  outline-offset: 3px;
}
.cta-link {
  color: var(--text);
  font-weight: 500;
  opacity: 0.85;
}
.cta-link:hover,
.cta-link:focus-visible {
  opacity: 1;
  text-decoration: underline;
}
.cta-link:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.6);
  outline-offset: 3px;
}

/* Examples emphasis */
/* Examples: featured accent line */
.section-featured .section-title::after {
  width: 72px;
}
.section-featured .card.example-card {
  padding: calc(var(--card-padding) + 8px);
  border-color: var(--example-accent-strong, rgba(101, 79, 240, 0.28));
  box-shadow: 0 16px 30px rgba(0, 0, 0, 0.28), 0 0 0 1px var(--example-accent, rgba(101, 79, 240, 0.09));
  background: linear-gradient(180deg, var(--example-accent, rgba(101, 79, 240, 0.07)), rgba(0, 0, 0, 0)) , var(--surface);
}
.sr-only {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  white-space: nowrap;
  border: 0;
}
.examples-grid {
  gap: 20px;
}
.example-card {
  gap: 14px;
  position: relative;
  overflow: hidden;
  --example-accent: rgba(101, 79, 240, 0.2);
  --example-accent-strong: rgba(101, 79, 240, 0.36);
  border-color: var(--example-card-border, rgba(255, 255, 255, 0.08));
  box-shadow: var(--example-card-shadow, 0 14px 28px rgba(0, 0, 0, 0.25));
}
.example-card h3::after {
  background: var(--example-divider, rgba(255, 255, 255, 0.084));
}
.example-card:hover,
.example-card:focus-within {
  border-color: var(--example-accent-strong, rgba(101, 79, 240, 0.36));
  transform: translateY(-2px);
  box-shadow: 0 18px 32px rgba(0, 0, 0, 0.3), 0 12px 20px var(--example-accent, rgba(101, 79, 240, 0.1));
}
.example-header {
  display: flex;
  justify-content: space-between;
  align-items: baseline;
  gap: 12px;
}
.example-kind {
  font-size: 0.68rem;
  font-weight: 600;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  padding: 4px 10px;
  border-radius: 999px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  color: var(--muted);
  background: var(--surface-soft);
  box-shadow: inset 0 0 0 1px var(--example-accent, rgba(101, 79, 240, 0.18));
  white-space: nowrap;
}
/* Examples: category cue strip */
.example-card::before {
  content: "";
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  height: 2px;
  background: linear-gradient(90deg, var(--example-accent, rgba(101, 79, 240, 0.22)), rgba(0, 0, 0, 0));
  pointer-events: none;
  transition: background 200ms ease;
}
.example-card:hover::before,
.example-card:focus-within::before {
  background: linear-gradient(90deg, var(--example-accent-strong, rgba(101, 79, 240, 0.36)), rgba(0, 0, 0, 0));
}
.section-featured .example-card::before {
  background: linear-gradient(90deg, var(--example-accent-strong, rgba(101, 79, 240, 0.36)), rgba(0, 0, 0, 0));
}
.example-card.kind-dom {
  --example-accent: rgba(82, 110, 255, 0.2);
  --example-accent-strong: rgba(82, 110, 255, 0.38);
}
.example-card.kind-canvas {
  --example-accent: rgba(86, 138, 255, 0.2);
  --example-accent-strong: rgba(86, 138, 255, 0.38);
}
.example-card.kind-repl {
  --example-accent: rgba(132, 96, 255, 0.22);
  --example-accent-strong: rgba(132, 96, 255, 0.4);
}
.example-card.kind-mathjax {
  --example-accent: rgba(146, 112, 255, 0.22);
  --example-accent-strong: rgba(146, 112, 255, 0.41);
}
.example-card.kind-xterm {
  --example-accent: rgba(88, 92, 255, 0.22);
  --example-accent-strong: rgba(88, 92, 255, 0.4);
}
.example-showcases {
  display: flex;
  flex-direction: column;
  gap: 8px;
}
.feature-list {
  margin: 0;
  padding-left: 18px;
  display: grid;
  gap: 3px;
  color: var(--muted);
  font-size: 0.87rem;
}
.feature-list li {
  margin-bottom: 0;
  line-height: 1.5;
}
.example-actions {
  margin-top: auto;
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
}
.next-steps-links {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 8px;
}
.next-steps-links li {
  margin: 0;
}
.example-action {
  color: var(--text);
  font-weight: 500;
  font-size: 0.9rem;
  opacity: 0.8;
  transition: opacity 150ms ease;
}
.example-action:hover,
.example-action:focus-visible {
  opacity: 1;
  text-decoration: underline;
}
.action-primary {
  color: var(--blue);
  font-weight: 600;
  opacity: 1;
  display: inline-flex;
  align-items: center;
  gap: 4px;
  transition: transform 200ms ease, color 200ms ease;
}
.action-primary:hover,
.action-primary:focus-visible {
  transform: translateX(2px);
  text-decoration: underline;
}
.action-primary:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.7);
  outline-offset: 3px;
}
.action-secondary {
  opacity: 0.72;
}
.action-secondary:hover,
.action-secondary:focus-visible {
  opacity: 0.88;
}
.example-link {
  margin-top: auto;
  color: var(--blue);
  font-weight: 600;
  font-size: 0.92rem;
}
.example-link::after {
  content: " →";
}
.example-link:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.6);
  outline-offset: 3px;
}
@media (min-width: 900px) {
  .examples-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}

/* Motion + reduced motion */
.cta-button::after {
  content: "";
  position: absolute;
  inset: 0;
  background: linear-gradient(120deg, transparent, rgba(255, 255, 255, 0.35), transparent);
  transform: translateX(-120%);
  transition: transform 0.6s ease;
}
.cta-button:hover::after,
.cta-button:focus-visible::after {
  transform: translateX(120%);
}
.hero-carousel-panel {
  animation: heroPulse 14s ease-in-out infinite;
}
@keyframes heroPulse {
  0%, 100% { box-shadow: 0 20px 40px rgba(0, 0, 0, 0.35); }
  50% { box-shadow: 0 24px 46px rgba(101, 79, 240, 0.2); }
}
@media (prefers-reduced-motion: reduce) {
  .cta-button::after { transition: none; transform: none; }
  .hero-carousel-panel { animation: none; }
  .section-title::after { transition: none; background-position: 0% 50%; }
  .card { transition: none; }
  .card:hover,
  .card:focus-within { transform: none; }
}
.footer {
  display: flex;
  justify-content: space-between;
  flex-wrap: wrap;
  gap: 12px;
  color: var(--muted);
  border-top: 1px solid rgba(255, 255, 255, 0.08);
  padding-top: 20px;
}
.highlight { color: var(--gold); font-weight: 600; }
.accent    { color: var(--blue); }
.warning   { color: var(--red); }
@media (min-width: 860px) {
  .install-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}
@media (min-width: 1000px) {
  .install-steps {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}
@media (max-width: 520px) {
  .prim-row {
    grid-template-columns: 120px minmax(0, 1fr);
    gap: 10px;
    padding: 9px 12px;
  }
  .prim-badge {
    width: 120px;
  }
}
@media (max-width: 900px) {
  .page { padding: 24px 0 64px; }
  .hero { grid-template-columns: 1fr; gap: 24px; }
  .section { margin-top: 56px; padding: 24px 18px; }
  .card-grid { grid-template-columns: 1fr; gap: 20px; }
}
@media (min-width: 720px) and (max-width: 900px) {
  .coverage-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
}
CSS
         purple blue red gold))))
  
  (js-append-child! head style)

  (define page-structure
    (case (current-page)
      [(implementation-status) (implementation-status-page)]
      [(documentation)         (documentation-page)]
      [(examples)              (examples-page)]
      [(quick-start)           (quick-start-page)]
      [(installation)          (installation-page)]
      [(community)             (community-page)]
      [else                    (home-page)]))
  
  (define page (sxml->dom page-structure))
  
  (js-append-child! body page)

  (when (eq? (current-page) 'implementation-status)
    (init-status-page-handlers!)))

;;;
;;; Entry Point
;;;

(init-dom)
