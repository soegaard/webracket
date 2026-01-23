;;
;; WebRacket Website
;;

(define (byte->hex n)
  (define s (number->string n 16))
  (define padded (if (= (string-length s) 1) (string-append "0" s) s))
  (string-upcase padded))

(define (make-color* r g b)
  (string-append "#" (byte->hex r) (byte->hex g) (byte->hex b)))

(define purple (make-color* 101 79 240))  ; #654FF0
(define blue   (make-color* 74 108 255))  ; #4A6CFF
(define red    (make-color* 209 58 58))   ; #D13A3A
(define gold   (make-color* 242 183 5))   ; #F2B705

(define (make-element tag [class-name #f] [text #f])
  (define node (js-create-element tag))
  (when class-name
    (js-set-attribute! node "class" class-name))
  (when text
    (js-set! node "textContent" text))
  node)

(define (append-children! parent children)
  (for-each (λ (child) (js-append-child! parent child)) children)
  parent)

(define (make-list items [class-name #f])
  (define list-node (make-element "ul" class-name))
  (for-each
   (λ (item)
     (define li (make-element "li" #f item))
     (js-append-child! list-node li))
   items)
  list-node)

(define (section-block title subtitle)
  (define section (make-element "section" "section"))
  (define header (make-element "div" "section-header"))
  (define title-node (make-element "h2" "section-title" title))
  (js-append-child! header title-node)
  (when subtitle
    (js-append-child! header (make-element "p" "section-lead" subtitle)))
  (js-append-child! section header)
  section)

(define (card-grid cards)
  (define grid (make-element "div" "card-grid"))
  (for-each
   (λ (card)
     (define item (make-element "div" "card"))
     (append-children! item card)
     (js-append-child! grid item))
   cards)
  grid)

(define (init-dom)
  (define head (js-document-head))
  (define body (js-document-body))

  (define style (js-create-element "style"))
  (js-set!
   style "textContent"
   (string-append
    ":root {\n"
    "  --purple: " purple ";\n"
    "  --blue: " blue ";\n"
    "  --red: " red ";\n"
    "  --gold: " gold ";\n"
    "  --bg: #0C0D1A;\n"
    "  --surface: #14162B;\n"
    "  --surface-soft: rgba(255, 255, 255, 0.03);\n"
    "  --text: #E6E8F2;\n"
    "  --muted: #B6BDDD;\n"
    "}\n"
    "* { box-sizing: border-box; }\n"
    "body {\n"
    "  margin: 0;\n"
    "  font-family: 'Inter', 'Fira Code', system-ui, sans-serif;\n"
    "  background: radial-gradient(circle at top, rgba(101, 79, 240, 0.25), transparent 55%), var(--bg);\n"
    "  color: var(--text);\n"
    "  min-height: 100vh;\n"
    "}\n"
    "a { color: var(--blue); text-decoration: none; }\n"
    ".page {\n"
    "  width: min(1200px, 92vw);\n"
    "  margin: 0 auto;\n"
    "  padding: 56px 0 80px;\n"
    "  display: flex;\n"
    "  flex-direction: column;\n"
    "  gap: 56px;\n"
    "}\n"
    ".hero {\n"
    "  display: grid;\n"
    "  grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));\n"
    "  gap: 32px;\n"
    "  align-items: center;\n"
    "}\n"
    ".hero-panel {\n"
    "  background: linear-gradient(140deg, rgba(101, 79, 240, 0.2), rgba(74, 108, 255, 0.08));\n"
    "  border: 1px solid rgba(101, 79, 240, 0.3);\n"
    "  border-radius: 28px;\n"
    "  padding: 36px;\n"
    "  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.35);\n"
    "}\n"
    ".hero-title {\n"
    "  font-size: clamp(2rem, 4vw, 3rem);\n"
    "  margin: 0 0 12px;\n"
    "}\n"
    ".hero-lead {\n"
    "  margin: 0 0 22px;\n"
    "  color: var(--muted);\n"
    "  line-height: 1.6;\n"
    "}\n"
    ".pill-row {\n"
    "  display: flex;\n"
    "  flex-wrap: wrap;\n"
    "  gap: 12px;\n"
    "}\n"
    ".pill {\n"
    "  border-radius: 999px;\n"
    "  padding: 6px 14px;\n"
    "  background: var(--surface-soft);\n"
    "  border: 1px solid rgba(255, 255, 255, 0.08);\n"
    "  color: var(--muted);\n"
    "  font-size: 0.85rem;\n"
    "}\n"
    ".logo-card {\n"
    "  background: var(--surface);\n"
    "  border-radius: 24px;\n"
    "  padding: 32px;\n"
    "  display: flex;\n"
    "  align-items: center;\n"
    "  justify-content: center;\n"
    "  border: 1px solid rgba(255, 255, 255, 0.08);\n"
    "}\n"
    ".logo-card img {\n"
    "  width: min(260px, 60vw);\n"
    "  height: auto;\n"
    "}\n"
    ".section {\n"
    "  display: flex;\n"
    "  flex-direction: column;\n"
    "  gap: 24px;\n"
    "}\n"
    ".section-header {\n"
    "  display: flex;\n"
    "  flex-direction: column;\n"
    "  gap: 8px;\n"
    "}\n"
    ".section-title {\n"
    "  margin: 0;\n"
    "  font-size: 1.6rem;\n"
    "}\n"
    ".section-lead {\n"
    "  margin: 0;\n"
    "  color: var(--muted);\n"
    "  line-height: 1.6;\n"
    "}\n"
    ".card-grid {\n"
    "  display: grid;\n"
    "  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));\n"
    "  gap: 18px;\n"
    "}\n"
    ".card {\n"
    "  background: var(--surface);\n"
    "  border: 1px solid rgba(255, 255, 255, 0.08);\n"
    "  border-radius: 20px;\n"
    "  padding: 20px;\n"
    "  display: flex;\n"
    "  flex-direction: column;\n"
    "  gap: 10px;\n"
    "}\n"
    ".card h3 { margin: 0; font-size: 1.05rem; }\n"
    ".card p { margin: 0; color: var(--muted); line-height: 1.5; }\n"
    ".steps {\n"
    "  display: grid;\n"
    "  gap: 12px;\n"
    "  background: var(--surface);\n"
    "  border-radius: 18px;\n"
    "  padding: 22px;\n"
    "  border: 1px solid rgba(255, 255, 255, 0.08);\n"
    "}\n"
    ".steps li { margin-bottom: 10px; color: var(--muted); }\n"
    ".steps li:last-child { margin-bottom: 0; }\n"
    ".footer {\n"
    "  display: flex;\n"
    "  justify-content: space-between;\n"
    "  flex-wrap: wrap;\n"
    "  gap: 12px;\n"
    "  color: var(--muted);\n"
    "  border-top: 1px solid rgba(255, 255, 255, 0.08);\n"
    "  padding-top: 20px;\n"
    "}\n"
    ".highlight { color: var(--gold); font-weight: 600; }\n"
    ".accent { color: var(--blue); }\n"
    ".warning { color: var(--red); }\n"))
  (js-append-child! head style)

  (define page (make-element "div" "page"))
  (js-append-child! body page)

  (define hero (make-element "section" "hero"))
  (define hero-panel (make-element "div" "hero-panel"))
  (define hero-pill (make-element "div" "pill" "Racket → WebAssembly"))
  (define hero-title (make-element "h1" "hero-title" "WebRacket"))
  (define hero-lead
    (make-element
     "p"
     "hero-lead"
     "A Racket-to-WebAssembly compiler and runtime that makes it possible to build practical browser experiences without leaving the Racket ecosystem."))
  (define hero-pill-row (make-element "div" "pill-row"))
  (append-children!
   hero-pill-row
   (list (make-element "span" "pill" "Wasm-GC compatible")
         (make-element "span" "pill" "JS + DOM FFI")
         (make-element "span" "pill" "Runs in browsers + Node")))
  (append-children! hero-panel (list hero-pill hero-title hero-lead hero-pill-row))

  (define logo-card (make-element "div" "logo-card"))
  (define logo (js-create-element "img"))
  (js-set-attribute! logo "src" "assets/hex-racket-wasm-logo.svg")
  (js-set-attribute! logo "alt" "WebRacket hex logo")
  (js-append-child! logo-card logo)

  (append-children! hero (list hero-panel logo-card))
  (js-append-child! page hero)

  (define intro (section-block
                 "Why WebRacket?"
                 "WebRacket is a subset of Racket that compiles to WebAssembly, so you can target modern browsers while staying in a familiar language."))
  (define intro-copy
    (make-list
     (list
      "Compile Racket programs into WebAssembly that runs in Chrome, Firefox, and Safari."
      "Leverage a JavaScript FFI for DOM, Canvas, MathJax, XtermJS, and JSXGraph integrations."
      "Use WebRacket to prototype ideas that could influence a future Racket WebAssembly backend.")))
  (js-append-child! intro intro-copy)
  (js-append-child! page intro)

  (define capabilities (section-block
                        "Language Coverage"
                        "WebRacket implements a substantial portion of Racket with a focus on practical web applications."))
  (define capability-cards
    (card-grid
     (list
      (list (make-element "h3" #f "Numbers")
            (make-element "p" #f "Flonums and fixnums with fast WebAssembly execution."))
      (list (make-element "h3" #f "Hash Tables")
            (make-element "p" #f "Mutable hash tables for eq?, eqv?, equal?, and always? comparisons."))
      (list (make-element "h3" #f "Structures")
            (make-element "p" #f "Support for structure properties, super structures, and applicable structs."))
      (list (make-element "h3" #f "Syntax")
            (make-element "p" #f "Standard Racket expander support, including for and match forms."))
      (list (make-element "h3" #f "Control Flow")
            (make-element "p" #f "Tail calls, multiple values, and upward exceptions are all supported."))
      (list (make-element "h3" #f "Concurrency")
            (make-element "p" #f "Single-threaded execution today, with a future path toward richer models."))))))
  (js-append-child! capabilities capability-cards)
  (js-append-child! page capabilities)

  (define tooling (section-block
                  "Toolchain Essentials"
                  "A small set of tools power the WebRacket workflow from source to the browser."))
  (define tooling-list
    (make-list
     (list
      (string-append "wasm-tools builds and validates the WebAssembly output. ")
      (string-append "Node.js enables running generated programs in the terminal. ")
      (string-append "raco-static-web makes it easy to serve compiled artifacts locally."))
     "steps"))
  (js-append-child! tooling tooling-list)
  (js-append-child! page tooling)

  (define pipeline (section-block
                    "Compiler Pipeline"
                    "WebRacket uses a direct-style compiler with NanoPass transformations before emitting WebAssembly."))
  (define pipeline-cards
    (card-grid
     (list
      (list (make-element "h3" #f "Frontend")
            (make-element "p" #f "Racket syntax is expanded, then normalized into a compiler-friendly core."))
      (list (make-element "h3" #f "Middle End")
            (make-element "p" #f "Nanopass phases like closure conversion and ANF shape the program."))
      (list (make-element "h3" #f "Backend")
            (make-element "p" #f "Destination-driven code generation emits folded WebAssembly code."))
      (list (make-element "h3" #f "Runtime")
            (make-element "p" #f "A custom runtime avoids reliance on host functionality where possible.")))))
  (js-append-child! pipeline pipeline-cards)
  (js-append-child! page pipeline)

  (define roadmap (section-block
                  "Roadmap"
                  "The long-term goal is full Racket support, but there is plenty more to tackle next."))
  (define roadmap-list
    (make-list
     (list
      "Fix bugs reported by early adopters and stabilize the current runtime."
      "Unlock modules by completing linklet support."
      "Add complex numbers, bignums, impersonators, and chaperones."
      "Improve regular expression support and consider CPS for continuations.")))
  (js-append-child! roadmap roadmap-list)
  (js-append-child! page roadmap)

  (define examples (section-block
                    "Example Projects"
                    "WebRacket already powers interactive demos that showcase browser APIs."))
  (define examples-list
    (make-list
     (list
      "MathJax 4 editor with live formula preview."
      "Matrix digital rain with XtermJS terminal rendering."
      "MiniScheme interactive REPL in the browser."
      "Space Invaders canvas game and Pict rendering demo.")))
  (js-append-child! examples examples-list)
  (js-append-child! page examples)

  (define footer (make-element "footer" "footer"))
  (define footer-left (make-element "span" #f "WebRacket — Racket for the browser."))
  (define footer-right (make-element "span" #f "Made for the Racket community."))
  (append-children! footer (list footer-left footer-right))
  (js-append-child! page footer))

(init-dom)
