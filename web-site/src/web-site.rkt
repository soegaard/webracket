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

;; make-ul-list : (Listof String) (U #f String) -> List
;;   Builds a <ul> sxml node from a list of text items and an optional class.
(define (make-ul-list items [class-name #f])
  (define list-items (map (λ (item) `(li ,item)) items))
  (if class-name
      `(ul (@ (class ,class-name)) ,@list-items)
      `(ul ,@list-items)))

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
    [(string-suffix? path "documentation.html") 'documentation]
    [(string-suffix? path "installation.html") 'installation]
    [(string-suffix? path "examples.html") 'examples]
    [(string-suffix? path "overview.html") 'overview]
    [(string-suffix? path "roadmap.html") 'roadmap]
    [(string-suffix? path "is-webracket-for-you.html") 'for-you]
    [else 'home]))

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
             ,(nav-link "For You" "is-webracket-for-you.html" 'for-you active-page)
             ,(nav-link "Overview" "overview.html" 'overview active-page)
             ,(nav-link "Road Ahead" "roadmap.html" 'roadmap active-page)
             ,(nav-link "Documentation" "documentation.html" 'documentation active-page)
             ,(nav-link "Installation" "installation.html" 'installation active-page)
             ,(nav-link "Examples" "examples.html" 'examples active-page))))

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
                         (span (@ (class "pipeline-step") (aria-hidden "true")) "")
                         (h3 (@ (class "pipeline-title")) "Frontend"))
                   `(p "Racket syntax is expanded, then normalized into a compiler-friendly core."))
             (list `(div (@ (class "pipeline-header"))
                         (span (@ (class "pipeline-step") (aria-hidden "true")) "")
                         (h3 (@ (class "pipeline-title")) "Middle End"))
                   `(p "Nanopass passes like closure conversion and ANF make environments and intermediate values explicit."))
             (list `(div (@ (class "pipeline-header"))
                         (span (@ (class "pipeline-step") (aria-hidden "true")) "")
                         (h3 (@ (class "pipeline-title")) "Backend"))
                   `(p "Destination-driven code generation emits folded WebAssembly code."))
             (list `(div (@ (class "pipeline-header"))
                         (span (@ (class "pipeline-step") (aria-hidden "true")) "")
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
  `(div (@ (class "page"))
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
           `(p "The WebRacket compiler is a direct-style compiler. This choice has made it easier "
               "to relate the generated code to the source program. In the future we will probably "
               "need to add a CPS-pass in order to support continuations and continuation marks.")
           `(p "The frontend of the WebRacket compiler uses "
               ,(code "read-syntax")
               " to read a WebRacket program from a file. The resulting syntax object is fed into "
               "the normal Racket expander to produce a program in fully expanded form.")
           `(p "The middle end of the compiler consists of several passes implemented using the "
               ,(code "NanoPass")
               " framework.")
           `(p "The passes are as follows:")
           `(pre (code "unexpand\nparse\nflatten-topbegin\ninfer-names\nconvert-quotations\nexplicit-begin\nexplicit-case-lambda\nα-rename\nassignment-conversion\ncategorize-applications\nanormalize\nclosure-conversion\nflatten-begin\n(classify-variables)\ngenerate-code"))
           `(p "See the comments in \"" ,(code "compiler.rkt") "\" for an explanation of each pass.")
           `(p "The code generator generates WebAssembly in the form of " ,(code "S-expressions")
               " in the \"folded\" format.")
           `(p "This code generator is inspired by \"Destination-driven Code Generation\" by Dybvig, "
               "Hieb and Butler. There are some differences, however. The code generator in the paper "
               "generates \"flat\" code (assembler) whereas we generate nested WebAssembly instructions.")
           `(p "Finally, the external tool " ,(code "wasm-tools") " " ,(code "parse")
               " converts the S-expression representation into bytecode format.")
           `(p "The main part of the compiler is in \"" ,(code "compiler.rkt") "\". The WebAssembly "
               "runtime is in \"" ,(code "runtime-wasm.rkt") "\". The standard library (implemented in "
               "WebRacket) is found in " ,(code "stdlib/") ". FFI bindings for popular libraries are in "
               ,(code "ffi/") ".")
           `(p "It has been a design goal to avoid relying on functionality provided by the "
               "WebAssembly host if possible. Who knows - maybe someone needs a non-JavaScript host "
               "at some point? For browser functionality there is no way around interfacing with "
               "the JavaScript host. The JavaScript part of the runtime support is in "
               ,(code "assembler.rkt") "."))
          #f
          #f)
        ,(section-block
          "Next steps"
          "Keep exploring with deeper dives into the WebRacket roadmap and guides."
          (list
           `(ul (@ (class "next-steps-links"))
                (li (a (@ (class "example-action action-primary")
                          (href "overview.html"))
                       "Overview"))
                (li (a (@ (class "example-action action-secondary")
                          (href "roadmap.html"))
                       "Road Ahead"))
                (li (a (@ (class "example-action action-secondary")
                          (href "installation.html"))
                       "Installation"))
                (li (a (@ (class "example-action action-secondary")
                          (href "examples.html"))
                       "Examples"))))
          #f
          "section--examples section-next-steps")
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
  background: rgba(101, 79, 240, 0.09);
  border: 1px solid rgba(101, 79, 240, 0.22);
  border-radius: 14px;
  padding: 12px 14px;
  color: var(--muted);
  font-size: 0.9rem;
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
      [(documentation) (documentation-page)]
      [(examples) (examples-page)]
      [(installation) (installation-page)]
      [else (home-page)]))
  
  (define page (sxml->dom page-structure))
  
  (js-append-child! body page))

;;;
;;; Entry Point
;;;

(init-dom)
