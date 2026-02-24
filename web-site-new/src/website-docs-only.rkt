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
;;; 
;;;

(require (for-syntax racket/base)
         (for-syntax racket/file))

(include "generated/ffi-doc-pages.inc.rkt")
(include "generated/ffi-doc-pages-structured.inc.rkt")

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
(define gh-base-dir  "https://github.com/soegaard/webracket/tree/main/")

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

;; sanitize-sxml : Any -> Any
;;   Defensive sanitizer to avoid unsupported SXML shapes.
(define (sanitize-sxml exp)
  (cond
    [(string? exp) exp]
    [(list? exp)
     (match exp
       [(list (? symbol? tag) (list '@ attrs ...) children ...)
        (define clean-attrs
          (for/list ([attr (in-list attrs)]
                     #:when (and (list? attr) (= (length attr) 2)))
            (define name (first attr))
            (define value (second attr))
            (list name (if (string? value) value (format "~a" value)))))
        (list* tag (list* '@ clean-attrs) (map sanitize-sxml children))]
       [(list (? symbol? tag) children ...)
        (list* tag (map sanitize-sxml children))]
       [_
        (format "~a" exp)])]
    [else
     (format "~a" exp)]))

;; card-grid : (Listof (Listof List)) (U #f String) -> List
;;   Wraps card content lists into a grid container.
(define (card-grid cards [class-name #f])
  (define base-class (if class-name
                         (string-append "card-grid " class-name)
                         "card-grid"))
  `(div (@ (class ,base-class))
        ,@(map (λ (card) `(div (@ (class "card")) ,@card)) cards)))

;; trim-hyphens : String -> String
;;   Removes leading/trailing hyphens from a slug.
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

;; doc-toc-list : (Listof (List Integer String String)) -> List
;;   Builds the TOC list from recorded heading data.
(define (toc-number+title text)
  (define parts (string-split (string-trim text) " "))
  (define token (if (pair? parts) (car parts) ""))
  (define segments (if (string=? token "") '() (string-split token ".")))
  (define numeric-prefix?
    (and (>= (length segments) 2)
         (for/and ([seg (in-list segments)])
           (and (not (string=? seg ""))
                (for/and ([ch (in-string seg)])
                  (char-numeric? ch))))))
  (if numeric-prefix?
      (values token (string-trim (substring (string-trim text) (string-length token))))
      (values #f text)))

(define (toc-kind level text)
  (cond
    [(string-prefix? (string-trim text) "Chapter ") "chapter"]
    [(= level 3) "section"]
    [(let-values ([(num _) (toc-number+title text)]) (and num #t)) "section"]
    [else "chapter"]))

(define (doc-toc-list toc-items)
  `(div (@ (class "toc-list"))
        ,@(for/list ([item (in-list toc-items)])
            (define level (car item))
            (define id (cadr item))
            (define text (caddr item))
            (define kind (toc-kind level text))
            (define-values (num label) (toc-number+title text))
            `(div (@ (class ,(string-append "toc-entry toc-entry--" kind))
                     (data-toc-kind ,kind))
                  (a (@ (class ,(if (string=? kind "section")
                                    "toc-link toc-link--sub toc-link--section"
                                    "toc-link toc-link--chapter"))
                        (href ,(string-append "#" id))
                        (data-toc-link ,id)
                        (data-toc-kind ,kind))
                     ,@(if num
                           (list `(span (@ (class "toc-num")) ,num)
                                 `(span (@ (class "toc-title")) ,label))
                           (list `(span (@ (class "toc-title")) ,text))))))))

;; doc-ffi runtime helpers
(define doc-js-ffi-handler-store '())

;; remember-doc-js-ffi-handler!: procedure? -> void?
;;   Store event handlers so they stay reachable.
(define (remember-doc-js-ffi-handler! handler)
  (set! doc-js-ffi-handler-store (cons handler doc-js-ffi-handler-store)))

;; node-list->list: any/c -> list?
;;   Convert a DOM NodeList into a Racket list.
(define (node-list->list node-list)
  (define len   (js-ref node-list "length"))
  (define count (if (number? len) (inexact->exact len) 0))
  (let loop ([idx 0]
             [acc '()])
    (if (>= idx count)
        (reverse acc)
        (loop (add1 idx)
              (cons (js-send/extern node-list "item" (vector idx)) acc)))))

;; element-text: any/c -> string?
;;   Extract trimmed textContent from an element.
(define (element-text element)
  (define content (and element (js-ref element "textContent")))
  (if (string? content)
      (string-trim content)
      ""))

;; element-class-string: any/c -> string?
;;   Extract className if present; otherwise fall back to the class attribute.
(define (element-class-string element)
  (define class-name (and element (js-ref element "className")))
  (cond
    [(string? class-name) class-name]
    [else
     (define attr (and element (js-get-attribute element "class")))
     (if (string? attr) attr "")]))

;; classlist-add!: any/c string? -> void?
;;   Adds a CSS class via classList when available, otherwise updates className.
(define (classlist-add! element class-name)
  (define class-list (and element (js-ref element "classList")))
  (cond
    [class-list
     (when (string? class-name)
       (js-send class-list "add" (vector class-name)))]
    [else
     (define current (element-class-string element))
     (define tokens (if (string? current) (string-split current) '()))
     (when (and (string? class-name) (not (member class-name tokens)))
       (let ([next (append tokens (list class-name))])
         (js-set! element "className" (string-join next " "))))]))

;; classlist-remove!: any/c string? -> void?
;;   Removes a CSS class via classList when available, otherwise updates className.
(define (classlist-remove! element class-name)
  (define class-list (and element (js-ref element "classList")))
  (cond
    [class-list
     (when (string? class-name)
       (js-send class-list "remove" (vector class-name)))]
    [else
     (define current (element-class-string element))
     (define tokens (if (string? current) (string-split current) '()))
     (define next (filter (λ (t) (not (string=? t class-name))) tokens))
     (js-set! element "className" (string-join next " "))]))

;; trim-trailing-newline : String -> String
;;   Removes a single trailing newline if present.
(define (trim-trailing-newline s)
  (define len (string-length s))
  (if (and (> len 0) (char=? (string-ref s (sub1 len)) #\newline))
      (substring s 0 (sub1 len))
      s))

;; escape-html : String -> String
;;   Escapes HTML entities for safe innerHTML.
(define (escape-html value)
  (define with-amp (string-replace value "&" "&amp;"))
  (define with-lt (string-replace with-amp "<" "&lt;"))
  (string-replace with-lt ">" "&gt;"))

;;;
;;; Page Layout
;;;

(include "completion.rkt")

;;;
;;; PAGES
;;;

;; current-page : -> Symbol
;;   Determines which page to render based on the URL path.
(define (current-page)
  (define path (js-ref (js-window-location) "pathname"))
  (cond
    [(string-suffix? path "documentation.html")                   'documentation]
    [(string-suffix? path "documentation-compiler-overview.html") 'doc-compiler-overview]
    [(string-suffix? path "documentation-js-ffi.html")            'doc-js-ffi]
    [(string-suffix? path "documentation-ffi-standard.html")      'doc-ffi-standard]
    [(string-suffix? path "documentation-ffi-dom.html")           'doc-ffi-dom]
    [(string-suffix? path "documentation-ffi-js.html")            'doc-ffi-js]
    [(string-suffix? path "documentation-ffi-math.html")          'doc-ffi-math]
    [(string-suffix? path "documentation-ffi-jsxgraph.html")      'doc-ffi-jsxgraph]
    [(string-suffix? path "documentation-ffi-xtermjs.html")       'doc-ffi-xtermjs]
    [(string-suffix? path "documentation-extended-example-jsxgraph-board-points.html")
                                                              'doc-extended-example-jsxgraph-board-points]
    [(string-suffix? path "documentation-extended-example-jsxgraph-geometry-constructors.html")
                                                              'doc-extended-example-jsxgraph-geometry-constructors]
    [else                                                         'documentation]))

(define (nav-active-page)
  (define page (current-page))
  (if (memq page '(doc-compiler-overview doc-js-ffi
                 doc-ffi-standard doc-ffi-dom doc-ffi-js
                 doc-ffi-math doc-ffi-jsxgraph doc-ffi-xtermjs
                 doc-extended-example-jsxgraph-board-points
                 doc-extended-example-jsxgraph-geometry-constructors))
      'documentation
      page))

;; nav-link : String String Symbol Symbol -> List
;;   Creates a nav link with active state styling.
(define (nav-link label href page-id active-page)
  (define class-name (if (eq? active-page page-id)
                         "nav-link nav-link--active"
                         "nav-link"))
  `(a (@ (class ,class-name) (href ,href)) ,label))

;; current-pathname : -> String
;;   Returns the browser pathname.
(define (current-pathname)
  (js-ref (js-window-location) "pathname"))

;; string-replace-first : String String String -> String
;;   Replaces the first occurrence of old with new.
(define (string-replace-first s old new)
  (define idx (path-find-substring-index s old))
  (if idx
      (string-append (substring s 0 idx)
                     new
                     (substring s (+ idx (string-length old))))
      s))

;; path-find-substring-index : String String -> (U #f Integer)
;;   Returns the index of the first occurrence of needle in s.
(define (path-find-substring-index s needle)
  (define s-len (string-length s))
  (define n-len (string-length needle))
  (let loop ([i 0])
    (cond
      [(> (+ i n-len) s-len) #f]
      [(string=? (substring s i (+ i n-len)) needle) i]
      [else (loop (add1 i))])))

;; path-in-new-site? : String -> Boolean
;;   Detects whether the current path is under the /new/ mirror.
(define (path-in-new-site? path)
  (or (string-prefix? path "/new/")
      (string=? path "/new")
      (and (path-find-substring-index path "/new/") #t)
      (string-suffix? path "/new")))

;; site-path->old : String -> String
;;   Converts a path to the old-site (non-/new/) form.
(define (site-path->old path)
  (cond
    [(path-find-substring-index path "/new/")
     (string-replace-first path "/new/" "/")]
    [(string-suffix? path "/new")
     (substring path 0 (- (string-length path) 4))]
    [else path]))

;; site-path->new : String -> String
;;   Converts a path to the /new/ mirrored form.
(define (site-path->new path)
  (cond
    [(path-in-new-site? path) path]
    [else
     (define last-slash
       (let loop ([i (sub1 (string-length path))])
         (cond
           [(< i 0) -1]
           [(char=? (string-ref path i) #\/) i]
           [else (loop (sub1 i))])))
     (if (= last-slash -1)
         (string-append "/new/" path)
         (let ([prefix (substring path 0 last-slash)]
               [suffix (substring path last-slash)])
           (if (string=? prefix "")
               (string-append "/new" suffix)
               (string-append prefix "/new" suffix))))]))

;; site-switch-option : String String Boolean -> List
;;   One side of the site-toggle segmented control.
(define (site-switch-option label href active?)
  (define class-name (if active?
                         "nav-switch-option nav-switch-option--active"
                         "nav-switch-option"))
  (if active?
      `(span (@ (class ,class-name) (aria-current "page")) ,label)
      `(a (@ (class ,class-name) (href ,href)) ,label)))

;; site-switch-toggle : -> List
;;   Segmented control for toggling between new and old site views.
(define (site-switch-toggle)
  (define path (current-pathname))
  (define in-new? (path-in-new-site? path))
  (define old-href (site-path->old path))
  (define new-href (site-path->new path))
  `(div (@ (class "nav-switch") (aria-label "Site version switch"))
        ,(site-switch-option "New" new-href in-new?)
        ,(site-switch-option "Old" old-href (not in-new?))))

;; navbar : -> List
;;   Shared navigation header for all pages.
(define (navbar)
  (define active-page (nav-active-page))
  `(div (@ (class "navbar-shell"))
        (nav (@ (class "navbar"))
             (div (@ (class "nav-left"))
                  (a (@ (class "nav-home") (href "index.html"))
                     (img (@ (class "nav-logo")
                             (src "assets/hex-racket-wasm-logo.svg")
                             (alt "WebRacket logo")))
                     (span (@ (class "nav-wordmark"))
                           (span (@ (class "nav-initial")) "W")
                           "eb"
                           (span (@ (class "nav-initial")) "R")
                           "acket")))
             (div (@ (class "nav-links"))
                  ,(nav-link "Documentation" "documentation.html" 'documentation active-page)))
        (div (@ (class "navbar-site-switch"))
             ,(site-switch-toggle)
             (p (@ (class "navbar-site-switch-note"))
                "We are working on a new web design. "
                "The new web design is not finished."))))

;; footer-section : -> List
;;   Shared footer for all pages.
(define (footer-section)
  `(footer (@ (class "footer"))
           (span "WebRacket — Racket for the browser.")
           (span "Made for the Racket community.")))

;;;

;;;
;;; DOCUMENTATION PAGE
;;;

(include "documentation.rkt")

;; Builds and attaches the page DOM plus its CSS styles.
(define (init-dom)
  (define head (js-document-head))
  (define body (js-document-body))

  (define racket-css-links
    (list
     `(link (@ (rel "stylesheet") (href "css/fonts/fonts.css") (media "screen")))))
  
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
  border-color: rgba(255, 255, 255, 0.24);
  background: rgba(255, 255, 255, 0.12);
  box-shadow: 0 10px 18px rgba(0, 0, 0, 0.26), 0 0 12px rgba(101, 79, 240, 0.2);
  transform: translateY(-1px);
}
a.code-pill:active {
  transform: translateY(0);
  background: rgba(255, 255, 255, 0.09);
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
.page--ffi-reference {
  width: min(1560px, 98vw);
}
.navbar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 24px;
}
.navbar-shell {
  display: block;
}
.navbar-site-switch {
  position: fixed;
  top: 92px;
  right: 20px;
  z-index: 90;
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  max-width: 220px;
}
.navbar-site-switch-note {
  margin: 8px 0 0;
  max-width: 220px;
  font-size: 0.72rem;
  line-height: 1.3;
  color: var(--muted);
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
  flex: 1 1 auto;
  justify-content: center;
  flex-wrap: wrap;
  min-width: 0;
  gap: 16px;
  font-size: 0.95rem;
}
.nav-link {
  display: inline-flex;
  align-items: center;
  min-height: 30px;
  padding: 2px 8px;
  border-radius: 999px;
  border: 1px solid transparent;
  color: var(--text);
  opacity: 0.8;
  transition: background 120ms ease, border-color 120ms ease, box-shadow 120ms ease, transform 120ms ease, opacity 120ms ease;
}
.nav-link:hover {
  opacity: 1;
  background: rgba(255, 255, 255, 0.05);
  border-color: rgba(255, 255, 255, 0.16);
  box-shadow: 0 8px 14px rgba(0, 0, 0, 0.22);
  transform: translateY(-1px);
}
.nav-link:focus-visible {
  opacity: 1;
  outline: 2px solid rgba(74, 108, 255, 0.62);
  outline-offset: 2px;
  background: rgba(255, 255, 255, 0.04);
  border-color: rgba(255, 255, 255, 0.2);
}
.nav-link:active {
  transform: translateY(0);
}
.nav-link--active {
  opacity: 1;
  font-weight: 600;
  background: rgba(255, 255, 255, 0.07);
  border-color: rgba(255, 255, 255, 0.2);
}
.nav-link--active::after {
  content: "";
  display: block;
  height: 2px;
  margin-top: 6px;
  background: rgba(74, 108, 255, 0.85);
  border-radius: 999px;
}
.nav-switch {
  display: inline-flex;
  align-items: center;
  gap: 2px;
  padding: 2px;
  border-radius: 999px;
  border: 1px solid rgba(112, 136, 215, 0.62);
  background: rgba(26, 42, 92, 0.18);
}
.nav-switch-option {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-width: 50px;
  min-height: 30px;
  padding: 4px 12px;
  border-radius: 999px;
  color: rgba(38, 86, 130, 0.95);
  font-size: 0.84rem;
  font-weight: 600;
  letter-spacing: 0.02em;
  text-decoration: none;
  opacity: 1;
  border: 1px solid rgba(112, 136, 215, 0.28);
  background: rgba(255, 255, 255, 0.62);
  transition: background 120ms ease, color 120ms ease, border-color 120ms ease;
}
.nav-switch-option:hover {
  background: rgba(255, 255, 255, 0.82);
  border-color: rgba(112, 136, 215, 0.44);
  color: rgba(26, 67, 108, 0.98);
}
.nav-switch-option:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.68);
  outline-offset: 2px;
}
.nav-switch-option--active {
  color: #ffffff;
  border-color: rgba(74, 108, 255, 0.92);
  background: rgba(74, 108, 255, 0.9);
}
@media (max-width: 1480px) {
  .navbar-site-switch {
    top: 84px;
    right: 12px;
    align-items: flex-start;
  }
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
.f1-countdown {
  display: flex;
  align-items: baseline;
  gap: 0.35rem;
  font-variant-numeric: tabular-nums;
}
.f1-countdown-value {
  font-size: clamp(2.3rem, 5.5vw, 3.2rem);
  font-weight: 780;
  line-height: 1;
  letter-spacing: -0.02em;
  font-family: "Fira Code", "JetBrains Mono", ui-monospace, SFMono-Regular, monospace;
}
.f1-countdown-unit {
  font-size: 1rem;
  color: var(--muted);
}
.f1-countdown-label {
  margin: 6px 0 0;
  color: var(--muted);
  font-size: 0.9rem;
}
.f1-countdown-detail {
  margin: 8px 0 0;
  font-size: 0.83rem;
  color: rgba(230, 232, 242, 0.6);
}
.f1-countdown-meta {
  margin-top: 12px;
  display: flex;
  align-items: center;
  gap: 10px;
  flex-wrap: wrap;
}
.f1-data-badge {
  display: inline-flex;
  align-items: center;
  border-radius: 999px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  background: rgba(255, 255, 255, 0.05);
  color: var(--muted);
  font-size: 0.75rem;
  font-weight: 600;
  letter-spacing: 0.02em;
  padding: 2px 8px;
}
.f1-data-badge.is-cached {
  border-color: rgba(124, 142, 216, 0.35);
}
.f1-countdown-status {
  margin: 0;
  color: rgba(230, 232, 242, 0.65);
  font-size: 0.83rem;
}
.f1-error-row {
  margin-top: 10px;
  display: inline-flex;
  align-items: center;
  gap: 10px;
  padding: 8px 10px;
  border-radius: 10px;
  border: 1px solid rgba(230, 119, 119, 0.38);
  background: rgba(230, 119, 119, 0.08);
  color: #f0c4c4;
  font-size: 0.84rem;
}
.f1-retry-button {
  border-radius: 8px;
  border: 1px solid rgba(255, 255, 255, 0.22);
  background: rgba(255, 255, 255, 0.06);
  color: var(--text);
  font-size: 0.8rem;
  padding: 4px 10px;
  cursor: pointer;
  transition: background 120ms ease, border-color 120ms ease, transform 120ms ease;
}
.f1-retry-button:hover {
  background: rgba(255, 255, 255, 0.1);
  border-color: rgba(255, 255, 255, 0.32);
  transform: translateY(-1px);
}
.f1-retry-button:active {
  transform: translateY(0);
}
.f1-retry-button:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.7);
  outline-offset: 2px;
}
.f1-loading-skeleton {
  display: none;
  margin-bottom: 8px;
}
.f1-skel-line {
  display: block;
  border-radius: 8px;
  background: linear-gradient(90deg, rgba(255,255,255,0.06), rgba(255,255,255,0.14), rgba(255,255,255,0.06));
  background-size: 200% 100%;
  animation: f1Skel 1.25s ease-in-out infinite;
}
.f1-skel-title { width: 58%; height: 14px; margin-bottom: 10px; }
.f1-skel-countdown { width: 42%; height: 42px; margin-bottom: 8px; }
.f1-skel-meta { width: 70%; height: 12px; }
.f1-countdown-card.is-loading .f1-loading-skeleton {
  display: block;
}
@keyframes f1Skel {
  0% { background-position: 180% 0; }
  100% { background-position: -20% 0; }
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
.docs-layout {
  display: grid;
  grid-template-columns: minmax(0, 1fr) 260px;
  gap: 28px;
  align-items: start;
}
.page--ffi-reference .docs-layout {
  grid-template-columns: minmax(0, 1fr) 280px;
  gap: 14px;
}
.page--ffi-reference .docs-layout.is-toc-collapsed {
  grid-template-columns: minmax(0, 1fr) 84px;
}
.page--ffi-reference .section--doc-body {
  padding-right: 8px;
}
.page--ffi-reference .docs-toc {
  margin-right: -6px;
}
.docs-article {
  min-width: 0;
}
.doc-prose {
  width: 100%;
}
.doc-prose > p,
.doc-prose > ul,
.doc-prose > ol,
.doc-prose > h2,
.doc-prose > h3,
.doc-prose > h4 {
  max-width: 72ch;
}
.doc-prose > p,
.doc-prose > li {
  color: #ECEFFA;
  font-size: 1.02rem;
  line-height: 1.75;
}
.doc-prose code {
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid rgba(255, 255, 255, 0.12);
  padding: 1px 6px;
  border-radius: 8px;
  color: #F4F6FF;
  font-size: 0.95em;
}
.doc-prose ul li code,
.doc-prose p code {
  white-space: nowrap;
}
.section--doc-body {
  margin-top: 44px;
  padding: 24px 22px 28px;
}
.doc-prose > p {
  margin: 0 0 12px;
}
.doc-prose > p + p {
  margin-top: 8px;
}
.doc-prose > ul,
.doc-prose > ol {
  padding-left: 20px;
  margin: 0 0 12px;
}
.doc-prose > ul li,
.doc-prose > ol li {
  margin: 0 0 6px;
  line-height: 1.7;
  color: #E6EBFF;
}
.doc-prose .ffi-table {
  max-width: 72ch;
  margin: 16px 0 18px;
}
.page--ffi-reference .doc-prose .ffi-table {
  max-width: none;
  width: 100%;
}
.doc-prose table th {
  text-align: left;
}
.ffi-table {
  overflow-x: auto;
  position: relative;
  padding-bottom: 4px;
  scrollbar-gutter: stable both-edges;
  border-radius: 18px;
}
.ffi-table::after {
  content: "";
  position: sticky;
  right: 0;
  top: 0;
  display: block;
  width: 32px;
  height: 100%;
  pointer-events: none;
  background: linear-gradient(90deg, rgba(12, 14, 28, 0), rgba(12, 14, 28, 0.85));
  opacity: 0.8;
}
.ffi-type-table {
  width: 100%;
  min-width: 560px;
  border-collapse: separate;
  border-spacing: 0;
  background: rgba(12, 14, 28, 0.86);
  border: 1px solid rgba(255, 255, 255, 0.1);
  border-radius: 16px;
  overflow: hidden;
  font-size: 0.98rem;
  box-shadow: 0 12px 24px rgba(0, 0, 0, 0.22);
}
.ffi-type-table caption {
  caption-side: top;
  text-align: left;
  padding: 12px 14px 8px;
  color: #C4CDEE;
  font-size: 0.78rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}
.ffi-type-table thead th {
  text-align: left;
  font-weight: 700;
  color: #F1F4FF;
  padding: 14px 16px;
  border-bottom: 1px solid rgba(255, 255, 255, 0.12);
  background: rgba(18, 22, 45, 0.82);
}
.ffi-type-table tbody td {
  padding: 12px 16px;
  vertical-align: top;
  border-bottom: 1px solid rgba(255, 255, 255, 0.08);
  color: #E9EEFF;
}
.ffi-type-table thead tr {
  box-shadow: inset 0 -1px 0 rgba(255, 255, 255, 0.12);
}
.ffi-type-table tbody tr:nth-child(even) td {
  background: rgba(255, 255, 255, 0.03);
}
.ffi-type-table tbody tr:last-child td {
  border-bottom: none;
}
.ffi-type-table th:first-child,
.ffi-type-table td:first-child {
  width: 140px;
}
.ffi-type-table td:last-child {
  color: #AEB9E2;
  font-size: 0.92rem;
}
.ffi-type-table th.ffi-use-when-col {
  color: #D9E2FF;
}
.ffi-type-table td.ffi-use-when-col {
  color: #9BA8D4;
  font-size: 0.86rem;
  line-height: 1.4;
}
.ffi-type-table th.ffi-function-col,
.ffi-type-table td.ffi-function-col,
.ffi-type-table td.ffi-function-col a,
.ffi-type-table td.ffi-function-col code {
  white-space: nowrap;
  word-break: normal;
  overflow-wrap: normal;
}
.ffi-type-table .ffi-type-cell {
  position: sticky;
  left: 0;
  background: inherit;
  z-index: 1;
  font-family: "Fira Code", "JetBrains Mono", ui-monospace, SFMono-Regular, monospace;
}
.ffi-type-table .ffi-type-cell code {
  white-space: nowrap;
  font-weight: 600;
  color: #F5F7FF;
  background: rgba(255, 255, 255, 0.08);
  border-color: rgba(255, 255, 255, 0.18);
}
.ffi-type-table tbody tr:nth-child(even) .ffi-type-cell {
  background: rgba(255, 255, 255, 0.03);
}
.ffi-type-table tbody tr:nth-child(odd) .ffi-type-cell {
  background: rgba(12, 14, 28, 0.86);
}
.ffi-type-table .ffi-note-cell {
  color: #BBC6EA;
}
.ffi-type-table .ffi-note-cell.is-empty {
  color: #8B96BF;
}
.doc-prose > h2,
.doc-prose > h3 {
  margin: 28px 0 10px;
  letter-spacing: -0.01em;
  color: #F4F6FF;
}
.doc-prose > h2:not(:first-child) {
  margin-top: 44px;
  padding-top: 14px;
  border-top: 1px solid rgba(255, 255, 255, 0.1);
}
.doc-prose > h2::before {
  content: "";
  display: block;
  height: 1px;
  margin-bottom: 12px;
  background: linear-gradient(90deg, rgba(101, 79, 240, 0.35), rgba(101, 79, 240, 0));
  opacity: 0.6;
}
.doc-prose > h2:first-child::before {
  display: none;
}
.doc-prose > h2 {
  font-size: 1.4rem;
}
.doc-prose > h3 {
  font-size: 1.18rem;
}
.doc-prose > h2:first-child {
  margin-top: 0;
}
.doc-prose pre {
  margin: 14px 0 18px;
}
.page--ffi-reference .doc-prose .ffi-chapter-title {
  position: relative;
  margin: 62px 0 14px;
  padding: 14px 16px 14px 20px;
  border-radius: 14px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  background: rgba(18, 22, 45, 0.52);
}
.page--ffi-reference .doc-prose > .ffi-chapter-title::before {
  display: none;
}
.page--ffi-reference .doc-prose .ffi-chapter-title::after {
  content: "";
  position: absolute;
  left: 10px;
  top: 10px;
  bottom: 10px;
  width: 2px;
  border-radius: 999px;
  background: rgba(182, 197, 238, 0.32);
}
.page--ffi-reference .doc-prose > .ffi-chapter-title:not(:first-child) {
  border-top: 1px solid rgba(255, 255, 255, 0.08);
  padding-top: 14px;
}
.page--ffi-reference .doc-prose .ffi-chapter-lead {
  margin: 6px 0 24px;
  color: rgba(200, 210, 240, 0.86);
  font-size: 0.98rem;
  line-height: 1.62;
  font-style: italic;
}
.page--ffi-reference .doc-prose > h3 {
  margin-top: 1.3em;
  margin-bottom: 0.42em;
}
.page--ffi-reference .doc-prose .ffi-type-table {
  background: rgba(12, 14, 28, 0.8);
}
.page--ffi-reference .doc-prose .ffi-type-table thead th {
  background: rgba(18, 22, 45, 0.74);
}
.page--ffi-reference .doc-prose .ffi-table {
  margin: 16px 0 28px;
}
.page--ffi-reference .doc-prose pre {
  margin: 14px 0 24px;
}
.page--ffi-reference .code-block pre {
  padding: 4px 10px 8px;
}
.page--ffi-reference .code-block pre code {
  display: block;
  background: transparent;
  border: none;
  border-radius: 0;
  padding: 0;
  color: inherit;
}
.toc-mobile {
  display: none;
  margin-top: 12px;
}
.toc-accordion {
  border-radius: 16px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  background: rgba(18, 20, 40, 0.82);
  padding: 10px 14px;
  box-shadow: 0 10px 24px rgba(0, 0, 0, 0.25);
}
.toc-accordion summary {
  display: flex;
  align-items: center;
  justify-content: space-between;
  cursor: pointer;
  list-style: none;
  font-weight: 600;
  color: #E9EEFF;
}
.toc-accordion summary::-webkit-details-marker {
  display: none;
}
.toc-accordion .toc-list {
  margin-top: 10px;
}
.docs-toc {
  position: sticky;
  top: 24px;
  align-self: start;
  margin-top: 44px;
}
.toc-card {
  background: rgba(18, 20, 40, 0.82);
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 18px;
  padding: 16px 16px 14px;
  box-shadow: 0 14px 32px rgba(0, 0, 0, 0.28);
  backdrop-filter: blur(12px);
  max-height: calc(100vh - 64px);
  display: flex;
  flex-direction: column;
}
.toc-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
  margin-bottom: 10px;
}
.toc-title {
  font-size: 0.85rem;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: #B9C3E8;
  margin: 0;
}
.toc-toggle {
  border: 1px solid rgba(255, 255, 255, 0.16);
  background: rgba(29, 35, 71, 0.68);
  color: #DCE5FF;
  font-size: 0.74rem;
  line-height: 1;
  padding: 6px 9px;
  border-radius: 8px;
  cursor: pointer;
}
.toc-toggle:hover {
  background: rgba(74, 108, 255, 0.22);
  border-color: rgba(120, 150, 255, 0.45);
}
.toc-toggle:focus-visible {
  outline: 2px solid rgba(120, 150, 255, 0.75);
  outline-offset: 2px;
}
.toc-list {
  display: flex;
  flex-direction: column;
  gap: 6px;
}
.toc-nav {
  overflow-y: auto;
  overscroll-behavior: contain;
  padding-right: 4px;
}
.toc-card.is-collapsed {
  max-height: none;
  padding: 10px 8px;
  min-height: 0;
}
.toc-card.is-collapsed .toc-nav {
  display: none;
}
.toc-card.is-collapsed .toc-title {
  display: none;
}
.toc-card.is-collapsed .toc-header {
  margin: 0;
  justify-content: center;
}
.toc-card.is-collapsed .toc-toggle {
  width: 100%;
  padding: 6px 0;
  text-align: center;
}
.toc-link {
  display: block;
  padding: 6px 10px;
  border-radius: 10px;
  color: #C9D3F3;
  font-size: 0.92rem;
  line-height: 1.35;
  transition: background 0.2s ease, color 0.2s ease;
  word-break: break-word;
}
.toc-link.toc-link--sub {
  padding-left: 18px;
  font-size: 0.88rem;
  color: #AAB6DF;
}
.toc-link.is-active {
  background: rgba(74, 108, 255, 0.2);
  color: #E9EEFF;
}
.toc-link:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.6);
  outline-offset: 2px;
}
.page--ffi-reference .toc-list {
  gap: 4px;
}
.page--ffi-reference .docs-toc .toc-list {
  gap: 0;
}
.page--ffi-reference .toc-entry--chapter {
  margin-top: 7px;
  padding-top: 6px;
  border-top: 1px solid rgba(255, 255, 255, 0.08);
}
.page--ffi-reference .toc-entry--chapter:first-child {
  margin-top: 0;
  padding-top: 0;
  border-top: none;
}
.page--ffi-reference .toc-link {
  line-height: 1.35;
}
.page--ffi-reference .toc-link--chapter {
  font-size: 0.99rem;
  font-weight: 650;
  padding: 7px 10px;
  color: #DCE6FF;
}
.page--ffi-reference .toc-link--section {
  font-size: 0.9rem;
  font-weight: 460;
  line-height: 1.25;
  padding: 4px 10px 4px 20px;
  display: flex;
  gap: 8px;
  align-items: start;
  color: #B8C5EC;
}
.page--ffi-reference .docs-toc .toc-link--section {
  line-height: 1.15;
  padding-top: 0;
  padding-bottom: 0;
}
.page--ffi-reference .toc-link .toc-title {
  flex: 1 1 auto;
  text-transform: none;
  letter-spacing: normal;
}
.page--ffi-reference .toc-link .toc-num {
  display: inline-block;
  font-size: 0.84em;
  opacity: 0.62;
  font-variant-numeric: tabular-nums;
}
.page--ffi-reference .toc-link--section .toc-num {
  flex: 0 0 2.9em;
}
.page--ffi-reference .toc-link:hover {
  background: rgba(74, 108, 255, 0.14);
  color: #EEF3FF;
}
.page--ffi-reference .toc-link.is-active {
  background: rgba(74, 108, 255, 0.22);
  color: #F2F6FF;
  box-shadow: inset 3px 0 0 rgba(122, 151, 255, 0.95);
}
.page--ffi-reference .toc-link.is-active-parent {
  background: rgba(74, 108, 255, 0.1);
  color: #E7EEFF;
}
.page--ffi-reference .toc-link:focus-visible {
  outline: 2px solid rgba(120, 150, 255, 0.75);
  outline-offset: 1px;
}
.page--ffi-reference .doc-prose .ffi-main-toc-list {
  list-style: none;
  margin: 6px 0 24px;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 3px;
}
.page--ffi-reference .doc-prose .ffi-main-toc-entry {
  list-style: none;
  margin: 0;
  padding: 0;
}
.page--ffi-reference .doc-prose .ffi-main-toc-entry--chapter {
  margin-top: 7px;
  padding-top: 6px;
  border-top: 1px solid rgba(255, 255, 255, 0.08);
}
.page--ffi-reference .doc-prose .ffi-main-toc-entry--chapter:first-child {
  margin-top: 0;
  padding-top: 0;
  border-top: none;
}
.page--ffi-reference .doc-prose .ffi-main-toc-list .toc-link {
  display: flex;
  align-items: baseline;
  gap: 8px;
  text-decoration: none;
}
.page--ffi-reference .doc-prose .ffi-main-toc-list .toc-link--section {
  line-height: 1.2;
  padding-top: 3px;
  padding-bottom: 3px;
}
.page--ffi-reference .doc-prose .ffi-main-toc-list .toc-link--chapter .toc-num {
  opacity: 0.82;
}
.heading-anchor {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 26px;
  height: 26px;
  margin-left: 8px;
  border-radius: 10px;
  border: 1px solid rgba(255, 255, 255, 0.14);
  background: rgba(15, 18, 36, 0.7);
  color: #C7D2FF;
  font-size: 0.85rem;
  opacity: 0;
  transform: translateY(-1px);
  transition: opacity 0.2s ease, transform 0.2s ease, border 0.2s ease;
}
.doc-prose h2:hover .heading-anchor,
.doc-prose h3:hover .heading-anchor,
.doc-prose h2:focus-within .heading-anchor,
.doc-prose h3:focus-within .heading-anchor {
  opacity: 1;
  transform: translateY(0);
}
.heading-anchor:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.6);
  outline-offset: 2px;
  opacity: 1;
}
.doc-prose h2,
.doc-prose h3 {
  scroll-margin-top: 96px;
}
.code-block {
  position: relative;
  background: #0B0D1D;
  border: 1px solid rgba(255, 255, 255, 0.08);
  border-radius: 16px;
  padding: 12px 12px 14px;
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.02), 0 12px 26px rgba(0, 0, 0, 0.35);
  overflow: hidden;
}
.code-block::after {
  content: "";
  position: absolute;
  top: 40px;
  right: 0;
  width: 48px;
  height: calc(100% - 40px);
  background: linear-gradient(90deg, rgba(11, 13, 29, 0) 0%, rgba(11, 13, 29, 0.9) 100%);
  pointer-events: none;
}
.code-toolbar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  padding: 4px 8px 10px;
  color: #9FB0E6;
  font-size: 0.78rem;
  text-transform: uppercase;
  letter-spacing: 0.08em;
}
.code-toolbar-label {
  display: inline-flex;
  align-items: center;
  gap: 6px;
}
.code-copy {
  border: 1px solid rgba(255, 255, 255, 0.16);
  background: rgba(20, 24, 52, 0.7);
  color: #E9EDFF;
  font-size: 0.78rem;
  padding: 6px 10px;
  border-radius: 10px;
  cursor: pointer;
  transition: background 0.2s ease, border 0.2s ease;
}
.code-copy:hover {
  background: rgba(74, 108, 255, 0.2);
  border-color: rgba(74, 108, 255, 0.45);
}
.code-copy:focus-visible {
  outline: 2px solid rgba(74, 108, 255, 0.6);
  outline-offset: 2px;
}
.code-block pre {
  margin: 0;
  padding: 0 8px 6px;
  background: transparent;
  border: none;
  color: #F5F7FF;
  font-size: 1.02rem;
  line-height: 1.7;
  white-space: pre;
  word-break: normal;
  overflow: auto;
}
.code-block pre code {
  display: block;
  background: transparent;
  border: 0;
  border-radius: 0;
  padding: 0;
  color: inherit;
  white-space: pre;
  font-family: "fira-mono", ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace;
}
.code-block[data-line-numbers="true"] pre {
  counter-reset: none;
}
.code-block[data-line-numbers="true"] .code-line {
  display: block;
  padding-left: 0;
  position: static;
}
.code-block[data-line-numbers="true"] .code-line::before {
  content: none;
}
.copy-toast {
  position: fixed;
  bottom: 24px;
  right: 24px;
  background: rgba(20, 24, 52, 0.92);
  border: 1px solid rgba(255, 255, 255, 0.14);
  color: #E9EEFF;
  padding: 10px 14px;
  border-radius: 12px;
  font-size: 0.9rem;
  opacity: 0;
  transform: translateY(8px);
  transition: opacity 0.2s ease, transform 0.2s ease;
  pointer-events: none;
  z-index: 40;
}
.copy-toast.is-visible {
  opacity: 1;
  transform: translateY(0);
}
.roadmap {
  margin-top: 26px;
}
.roadmap-panel {
  border-radius: 18px;
  border: 1px solid rgba(255, 255, 255, 0.08);
  background: rgba(18, 20, 40, 0.62);
  padding: 8px 12px 12px;
}
.roadmap-summary {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 12px;
  cursor: pointer;
  list-style: none;
  color: #E9EEFF;
  font-weight: 600;
}
.roadmap-summary::-webkit-details-marker {
  display: none;
}
.roadmap-kicker {
  font-size: 0.78rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: #9FB0E6;
}
.roadmap-list {
  display: grid;
  gap: 10px;
  margin-top: 14px;
}
.roadmap-item {
  display: grid;
  grid-template-columns: auto 1fr;
  gap: 10px;
  align-items: center;
  padding: 8px 10px;
  border-radius: 12px;
  background: rgba(12, 14, 30, 0.65);
  border: 1px solid rgba(255, 255, 255, 0.06);
  transition: border 0.2s ease, background 0.2s ease;
}
.roadmap-item:focus-within {
  border-color: rgba(74, 108, 255, 0.4);
  background: rgba(16, 20, 40, 0.85);
}
.roadmap-badge {
  font-size: 0.72rem;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  padding: 4px 8px;
  border-radius: 999px;
  border: 1px solid transparent;
}
.roadmap-badge--planned {
  background: rgba(74, 108, 255, 0.18);
  color: #C8D4FF;
  border-color: rgba(74, 108, 255, 0.4);
}
.roadmap-link {
  color: #E4EAFF;
  text-decoration: none;
}
.roadmap-link.is-disabled {
  color: #9CA7CF;
  cursor: default;
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
  font-family: "fira-mono", ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace;
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
/* Examples: category cue strip */
.section-featured .example-card::before {
  background: linear-gradient(90deg, var(--example-accent-strong, rgba(101, 79, 240, 0.36)), rgba(0, 0, 0, 0));
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
.pane-label {
  font-weight: 600;
}
.pane-meta {
  font-size: 0.68rem;
  letter-spacing: 0.08em;
  text-transform: none;
  opacity: 0.8;
}
.arcade-frame {
  display: flex;
  justify-content: center;
  padding: 18px 10px 12px;
}
.arcade-bezel {
  --bezel-radius: 14px;
  --bezel-pad: 14px;
  --bezel-border: rgba(128, 118, 210, 0.28);
  --bezel-shadow: rgba(7, 5, 20, 0.75);
  --bezel-glow: rgba(120, 92, 210, 0.16);
  padding: var(--bezel-pad);
  border-radius: var(--bezel-radius);
  border: 1.5px solid var(--bezel-border);
  background: linear-gradient(160deg, rgba(22, 18, 48, 0.92), rgba(10, 8, 24, 0.96));
  box-shadow:
    0 18px 40px rgba(6, 4, 20, 0.55),
    0 0 28px var(--bezel-glow);
  position: relative;
}
.arcade-bezel::after {
  content: "";
  position: absolute;
  inset: 8px;
  border-radius: calc(var(--bezel-radius) - 6px);
  box-shadow: inset 0 10px 22px var(--bezel-shadow);
  pointer-events: none;
}
.arcade-bezel canvas {
  display: block;
  border-radius: calc(var(--bezel-radius) - 8px);
  background: #000;
}
@media (min-width: 900px) {
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
  .docs-toc { display: none; }
  .toc-mobile { display: block; }
}
@media (min-width: 720px) and (max-width: 900px) {
  .coverage-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
}
@media (max-width: 640px) {
  .toc-card {
    padding: 14px 14px 12px;
  }
  .doc-prose > h2 { font-size: 1.3rem; }
  .doc-prose > h3 { font-size: 1.12rem; }
}

/* Racket-lang.org inspired override for generated pages */
:root {
  --ink: #1f2a37 !important;
  --muted: #52606d !important;
  --link: #0679a7 !important;
  --link-hover: #055c86 !important;
  --rule: #b9d3e6 !important;
  --rule2: #d8e7f3 !important;
  --teal: #0679a7 !important;
  --panel: #f6fbff !important;
  --white: #ffffff !important;
  --purple: #0679a7 !important;
  --blue: #0679a7 !important;
  --red: #0679a7 !important;
  --gold: #0679a7 !important;
  --bg: #ffffff !important;
  --surface: #ffffff !important;
  --surface-soft: #f6fbff !important;
  --text: #1f2a37 !important;
}
html {
  width: 100% !important;
  background: var(--white) !important;
  font-size: 16px !important;
}
body {
  width: 100% !important;
  min-width: 100% !important;
  margin: 0 !important;
  font-family: "cooper-hewitt", Helvetica, Arial, sans-serif !important;
  font-size: 1.125rem !important;
  line-height: 1.5 !important;
  text-rendering: optimizeLegibility !important;
  font-feature-settings: 'kern' 1, 'liga' 1 !important;
  background: var(--white) !important;
  color: var(--ink) !important;
}
.page {
  width: min(980px, calc(100vw - 36px)) !important;
  margin: 0 auto !important;
  padding: 0 0 44px !important;
}
.hero {
  box-shadow: none !important;
}
.navbar,
.hero-panel,
.hero-carousel-panel,
.section,
.card,
.docs-toc,
.toc-card,
.doc-prose,
.install-hero-panel,
.footer {
  background: var(--white) !important;
  border: 1px solid var(--rule) !important;
  border-radius: 6px !important;
  box-shadow: none !important;
}
.navbar {
  background: var(--white) !important;
  border: none !important;
  border-radius: 0 !important;
  border-bottom: 1px solid var(--rule) !important;
}
.hero-panel,
.hero .hero-panel,
.hero .hero-carousel-panel,
.examples-hero .hero-panel,
.docs-hero .hero-panel,
.mathjax-hero .hero-panel,
.minischeme-hero .hero-panel,
.install-hero-panel {
  border: none !important;
}
.hero-panel .pill-row,
.examples-hero .pill-row,
.docs-hero .pill-row,
.mathjax-hero .pill-row,
.minischeme-hero .pill-row {
  display: none !important;
}
h1, h2, h3, .hero-title, .section-title {
  font-family: "cooper-hewitt", Helvetica, Arial, sans-serif !important;
  font-weight: 600 !important;
  color: var(--ink) !important;
}
h1, .hero-title {
  border-bottom: 1px solid var(--rule) !important;
}
a {
  color: var(--link) !important;
  text-decoration: none !important;
}
a:hover,
a:focus-visible {
  color: var(--link-hover) !important;
  text-decoration: underline !important;
}
.navbar a,
.nav a,
.menu a {
  font-family: "fira-mono", ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace !important;
  letter-spacing: 0.05em !important;
  text-transform: uppercase !important;
  font-size: 13px !important;
  font-weight: 650 !important;
  border: none !important;
  box-shadow: none !important;
  border-radius: 0 !important;
  background: transparent !important;
  padding: 7px 14px !important;
}
.navbar a:hover,
.nav a:hover,
.menu a:hover {
  background: transparent !important;
  color: var(--link-hover) !important;
  text-decoration: underline !important;
}
/* Keep the new/old switch visually distinct from generic navbar links. */
.navbar-site-switch .nav-switch {
  border: 1px solid rgba(112, 136, 215, 0.75) !important;
  background: rgba(26, 42, 92, 0.2) !important;
  border-radius: 999px !important;
  padding: 2px !important;
}
.navbar-site-switch .nav-switch-option {
  font-family: "cooper-hewitt", Helvetica, Arial, sans-serif !important;
  text-transform: none !important;
  letter-spacing: 0 !important;
  font-size: 0.84rem !important;
  font-weight: 650 !important;
  min-height: 30px !important;
  min-width: 50px !important;
  border-radius: 999px !important;
  border: 1px solid rgba(112, 136, 215, 0.35) !important;
  background: rgba(255, 255, 255, 0.66) !important;
  color: rgba(21, 106, 164, 1) !important;
  text-decoration: none !important;
  padding: 4px 12px !important;
  cursor: pointer !important;
}
.navbar-site-switch a.nav-switch-option {
  box-shadow: 0 1px 0 rgba(255, 255, 255, 0.65) inset !important;
}
.navbar-site-switch a.nav-switch-option:not(.nav-switch-option--active) {
  background: transparent !important;
  border: none !important;
  border-radius: 0 !important;
  box-shadow: none !important;
}
.navbar-site-switch .nav-switch-option:hover,
.navbar-site-switch .nav-switch-option:focus-visible {
  background: rgba(255, 255, 255, 0.86) !important;
  color: rgba(16, 92, 145, 1) !important;
  text-decoration: none !important;
}
.navbar-site-switch a.nav-switch-option:not(.nav-switch-option--active):hover,
.navbar-site-switch a.nav-switch-option:not(.nav-switch-option--active):focus-visible {
  background: transparent !important;
  border: none !important;
  border-radius: 0 !important;
  box-shadow: none !important;
}
.navbar-site-switch .nav-switch-option--active {
  border-color: transparent !important;
  background: rgba(74, 108, 255, 0.72) !important;
  color: rgba(236, 243, 255, 0.98) !important;
  text-decoration: none !important;
  cursor: default !important;
  box-shadow: none !important;
}
.navbar-site-switch .nav-switch-option--active:hover,
.navbar-site-switch .nav-switch-option--active:focus-visible {
  border-color: transparent !important;
  background: rgba(74, 108, 255, 0.72) !important;
  color: rgba(236, 243, 255, 0.98) !important;
  text-decoration: none !important;
  outline: none !important;
}
.navbar-site-switch span.nav-switch-option--active {
  pointer-events: none !important;
}
code,
pre,
kbd,
samp {
  background: var(--panel) !important;
  border-color: var(--rule2) !important;
  color: var(--ink) !important;
}
.code-block pre code,
.page--ffi-reference .code-block pre code {
  background: transparent !important;
  border: none !important;
  border-color: transparent !important;
  border-radius: 0 !important;
  padding: 0 !important;
  box-shadow: none !important;
  text-decoration: none !important;
}
pre code,
pre code *,
.doc-prose pre code,
.doc-prose pre code *,
.doc-content pre code,
.doc-content pre code * {
  background: transparent !important;
  border: none !important;
  border-color: transparent !important;
  border-radius: 0 !important;
  box-shadow: none !important;
  text-decoration: none !important;
}
pre code,
.doc-prose pre code,
.doc-content pre code {
  display: block !important;
  padding: 0 !important;
}
.card p code,
.card li code,
.doc-prose p code,
.doc-prose li code,
.doc-content p code,
.doc-content li code,
.section-lead code {
  font-family: "fira-mono", ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace !important;
  background: transparent !important;
  border: none !important;
  border-radius: 0 !important;
  padding: 0 !important;
  box-shadow: none !important;
  color: #1f2a37 !important;
}
.cta-button,
button,
input[type="button"],
input[type="submit"] {
  font-family: "fira-mono", ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace !important;
  text-transform: uppercase !important;
  letter-spacing: 0.06em !important;
  background: #fff !important;
  border: 1px solid var(--teal) !important;
  color: var(--teal) !important;
  box-shadow: 0 1px 3px rgba(20, 32, 52, 0.08) !important;
}
.cta-button:hover,
button:hover,
input[type="button"]:hover,
input[type="submit"]:hover {
  background: var(--teal) !important;
  color: #fff !important;
  box-shadow: 0 2px 5px rgba(20, 32, 52, 0.10) !important;
}
.footer {
  border: none !important;
  border-top: 1px solid var(--rule) !important;
  border-radius: 0 !important;
  color: var(--muted) !important;
}

/* Readability adjustments + docs-style margin note */
.hero-lead,
.hero-sublead,
.section-lead,
.card p,
.card-desc,
.doc-prose p,
.doc-prose li,
.install-step-card p {
  color: #2f3b4a !important;
  font-size: 1.14rem !important;
  line-height: 1.62 !important;
}
.section-title,
.hero-title {
  letter-spacing: 0 !important;
}
.section,
.card,
.doc-prose,
.hero-panel,
.install-step-card,
.toc-card,
.roadmap-item {
  font-size: 1.05rem !important;
}
.doc-prose > h2,
.doc-prose > h3,
.section h2,
.section h3 {
  color: #1f2a37 !important;
}
.hero-title { font-size: 3.1rem !important; }
.section-title { font-size: 2rem !important; }
.card {
  background: #fff !important;
}
.callout--note {
  background: #eaf4ff !important;
  border: 1px solid #b7d3ef !important;
  border-radius: 0 !important;
  box-shadow: none !important;
}
.callout--note .callout-title,
.callout--note .callout-label,
.callout--note p {
  color: #1f3f63 !important;
}
.page--status .callout--info {
  background: #f7fbff !important;
  border: 1px solid #c7dbea !important;
  border-left: 3px solid #84add0 !important;
  border-radius: 6px !important;
  box-shadow: none !important;
  padding: 12px 16px !important;
}
.page--status .callout--info .callout-title,
.page--status .callout--info .callout-label,
.page--status .callout--info p {
  color: #324252 !important;
}
.page--status .callout--info .callout-icon {
  color: #4f7fa6 !important;
  opacity: 1 !important;
}
.page--status .status-legend {
  background: #ffffff !important;
  border: 1px solid #c7dbea !important;
  color: #556173 !important;
}
.page--status .status-legend-title {
  color: #6a7584 !important;
}
.page--status .status-legend li {
  color: #556173 !important;
}
.page--status .status-legend-term {
  color: #2f3a49 !important;
}
.page--status .status-helper {
  color: #687487 !important;
}
.page--status .status-section {
  background: #ffffff !important;
  border-color: #c7dbea !important;
}
.page--status .status-title {
  color: #263241 !important;
}
.page--status .status-count,
.page--status .status-summary-action {
  color: #5c687a !important;
}
.page--status .status-summary:hover {
  background: #eef5fb !important;
}

/* Closer to racket-lang.org docs tone */
body {
  color: #4f5864 !important;
}
h1, h2, h3, .hero-title, .section-title {
  color: #262f3a !important;
  font-weight: 600 !important;
}
.hero-lead,
.hero-sublead,
.section-lead,
.card p,
.card-desc,
.doc-prose p,
.doc-prose li,
.install-step-card p {
  color: #555f6c !important;
}
.section-header {
  border-bottom: 1px solid #b9d3e6 !important;
  padding-bottom: 10px !important;
  margin-bottom: 14px !important;
}
.section-title {
  position: relative !important;
  padding-left: 14px !important;
}
.section-header .section-title {
  border-bottom: none !important;
}
.section-title::after {
  display: none !important;
  content: none !important;
}
.section-title::before {
  content: "" !important;
  position: absolute !important;
  left: 0 !important;
  top: 0.12em !important;
  width: 3px !important;
  height: 1.1em !important;
  border-radius: 2px !important;
  background: hsla(var(--accent-h, 252), 78%, 72%, 0.75) !important;
}
a {
  color: #0b79ad !important;
}
a:hover,
a:focus-visible {
  color: #075f88 !important;
}
#main-content:focus,
#main-content:focus-visible {
  outline: none !important;
}
.doc-cta,
.doc-cta:visited,
.doc-cta--primary,
.doc-cta--primary:visited {
  color: #ffffff !important;
}
.doc-cta--primary {
  background: #0b79ad !important;
  border-color: #0b79ad !important;
  box-shadow: 0 2px 6px rgba(20, 32, 52, 0.14) !important;
}
.doc-cta--primary:hover,
.doc-cta--primary:focus-visible {
  background: #075f88 !important;
  border-color: #075f88 !important;
  color: #ffffff !important;
}
.nav-home .nav-wordmark {
  display: inline-flex !important;
  align-items: baseline !important;
  gap: 0.01em !important;
}
.nav-home .nav-wordmark .nav-initial {
  font-size: 1.28em !important;
  line-height: 0.9 !important;
}
CSS
         purple blue red gold))))

  (js-append-child! head style)
  (for-each (λ (link-node)
              (js-append-child! head (sxml->dom link-node)))
            racket-css-links)

  (define page-structure
    (case (current-page)
      [(documentation)         (documentation-page)]
      [(doc-compiler-overview) (doc-compiler-overview-page)]
      [(doc-js-ffi)            (doc-js-ffi-page)]
      [(doc-ffi-standard)      (doc-ffi-standard-page)]
      [(doc-ffi-dom)           (doc-ffi-dom-page)]
      [(doc-ffi-js)            (doc-ffi-js-page)]
      [(doc-ffi-math)          (doc-ffi-math-page)]
      [(doc-ffi-jsxgraph)      (doc-ffi-jsxgraph-page)]
      [(doc-ffi-xtermjs)       (doc-ffi-xtermjs-page)]
      [(doc-extended-example-jsxgraph-board-points)
                               (doc-extended-example-jsxgraph-board-points-page)]
      [(doc-extended-example-jsxgraph-geometry-constructors)
                               (doc-extended-example-jsxgraph-geometry-constructors-page)]
      [else                    (documentation-page)]))

  (define safe-structure
    (if (eq? (current-page) 'doc-js-ffi)
        (sanitize-sxml page-structure)
        page-structure))

  (define page (sxml->dom safe-structure))

  (js-append-child! body page)

  (when (memq (current-page)
              '(doc-js-ffi
                doc-ffi-standard doc-ffi-dom doc-ffi-js
                doc-ffi-math doc-ffi-jsxgraph doc-ffi-xtermjs))
    (init-doc-js-ffi-page!))

  (when (eq? (current-page) 'doc-extended-example-jsxgraph-board-points)
    (init-extended-example-jsxgraph-board!))
  (when (eq? (current-page) 'doc-extended-example-jsxgraph-geometry-constructors)
    (init-extended-example-jsxgraph-geometry-board!)))

;;;
;;; Entry Point
;;;


(init-dom)
