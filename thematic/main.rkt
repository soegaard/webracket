#lang racket/base

;; thematic : command-line -> void
;;   Generate a starter web-easy external theme from Bootstrap custom properties.

(provide main)

(require lexers/css
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/runtime-path
         racket/string)

(define-runtime-path theme-template-path
  "../lib/web-easy/themes/theme-light.css")

(struct css-rule (selector declarations) #:transparent)
(struct css-decl (name value) #:transparent)

(struct rgb (r g b a) #:transparent)

(struct bootstrap-theme
  (fg
   bg
   border
   border-soft
   primary
   secondary
   success
   info
   warning
   danger
   light
   dark
   font-family-base
   font-family-heading
   font-size-sm
   font-size-md
   font-size-lg
   line-height-base
   font-weight-normal
   font-weight-semibold
   font-weight-bold
   radius-sm
   radius-md
   radius-lg
   radius-pill
   shadow-sm
   shadow-md
   shadow-lg
   props)
  #:transparent)

(define bootstrap-defaults
  (hash
   "--bs-body-color" "#212529"
   "--bs-body-bg" "#ffffff"
   "--bs-border-color" "#dee2e6"
   "--bs-border-color-translucent" "rgba(33, 37, 41, 0.175)"
   "--bs-primary" "#0d6efd"
   "--bs-secondary" "#6c757d"
   "--bs-success" "#198754"
   "--bs-info" "#0dcaf0"
   "--bs-warning" "#ffc107"
   "--bs-danger" "#dc3545"
   "--bs-light" "#f8f9fa"
   "--bs-dark" "#212529"
   "--bs-body-font-family"
   "system-ui, -apple-system, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, sans-serif"
   "--bs-body-font-size" "1rem"
   "--bs-body-line-height" "1.5"
   "--bs-border-radius-sm" "0.25rem"
   "--bs-border-radius" "0.375rem"
   "--bs-border-radius-lg" "0.5rem"
   "--bs-border-radius-pill" "50rem"
   "--bs-box-shadow-sm" "0 0.125rem 0.25rem rgba(0, 0, 0, 0.075)"
   "--bs-box-shadow" "0 0.5rem 1rem rgba(0, 0, 0, 0.15)"
   "--bs-box-shadow-lg" "0 1rem 3rem rgba(0, 0, 0, 0.175)"
   "--bs-focus-ring-color" "rgba(13, 110, 253, 0.25)"))

(define css-section-2-marker
  "/* -------------------------------------------------------------------------- */\n/* 2) Menu-specific tuning")

(define selector-regexp
  #px"^(:root|\\[data-bs-theme=(?:light|dark)\\])$")

(define (fail fmt . args)
  (raise-user-error 'thematic (apply format fmt args)))

;; string-empty-or-whitespace? : string? -> boolean?
;;   Determine whether s contains only whitespace.
(define (string-empty-or-whitespace? s)
  (string=? (string-trim s) ""))

;; trim-trailing-comment-space : string? -> string?
;;   Normalize surrounding whitespace while preserving internal value text.
(define (trim-trailing-comment-space s)
  (string-trim s))

;; strip-css-comments : string? -> string?
;;   Remove CSS comments from a selector or declaration fragment.
(define (strip-css-comments s)
  (regexp-replace* #px"(?s:/\\*.*?\\*/)" s " "))

;; normalize-selector : string? -> string?
;;   Trim selector text and discard comments around it.
(define (normalize-selector s)
  (string-trim (strip-css-comments s)))

;; split-selector-list : string? -> (listof string?)
;;   Split a selector list on top-level commas and normalize each entry.
(define (split-selector-list s)
  (define raw (normalize-selector s))
  (filter (lambda (part) (not (string-empty-or-whitespace? part)))
          (map normalize-selector (string-split raw ","))))

;; selector-list-matches? : string? string? -> boolean?
;;   Determine whether requested selector appears in a possibly-comma-separated selector list.
(define (selector-list-matches? selector-text requested-selector)
  (member requested-selector (split-selector-list selector-text)))

;; token-text : css-derived-token? -> string?
;;   Extract the original token text.
(define (token-text token)
  (css-derived-token-text token))

;; token-has-tag? : css-derived-token? symbol? -> boolean?
;;   Check whether token carries tag.
(define (token-has-tag? token tag)
  (css-derived-token-has-tag? token tag))

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

;; read-css-rules : string? -> (listof css-rule?)
;;   Parse top-level qualified rules and their declarations from source.
(define (read-css-rules source)
  (define tokens (css-string->derived-tokens source))
  (define rules '())
  (define state 'prelude)
  (define prelude-parts '())
  (define decl-name-parts '())
  (define decl-value-parts '())
  (define current-selector #f)
  (define current-declarations '())
  (define current-name #f)
  (define current-rule-at? #f)
  (define paren-depth 0)
  (define bracket-depth 0)
  (define brace-depth 0)

  (define (top-level-value?)
    (and (zero? paren-depth)
         (zero? bracket-depth)
         (= brace-depth 1)))

  (define (top-level-prelude?)
    (and (zero? paren-depth)
         (zero? bracket-depth)
         (zero? brace-depth)))

  (define (finish-declaration!)
    (define maybe-name (and current-name (string-trim current-name)))
    (define maybe-value (trim-trailing-comment-space (apply string-append (reverse decl-value-parts))))
    (when (and maybe-name
               (not (string=? maybe-name ""))
               (not (string=? maybe-value "")))
      (set! current-declarations
            (cons (css-decl maybe-name maybe-value) current-declarations)))
    (set! current-name #f)
    (set! decl-name-parts '())
    (set! decl-value-parts '()))

  (define (finish-rule!)
    (when current-selector
      (set! rules
            (cons (css-rule current-selector (reverse current-declarations))
                  rules)))
    (set! state 'prelude)
    (set! prelude-parts '())
    (set! decl-name-parts '())
    (set! decl-value-parts '())
    (set! current-selector #f)
    (set! current-declarations '())
    (set! current-name #f)
    (set! current-rule-at? #f))

  (for ([token (in-list tokens)])
    (define text (token-text token))
    (define previous-brace-depth brace-depth)
    (cond
      [(eq? state 'prelude)
       (cond
         [(and (string=? text "{")
               (top-level-prelude?))
          (set! current-selector
                (normalize-selector (apply string-append (reverse prelude-parts))))
          (set! prelude-parts '())
          (set! state (if current-rule-at? 'skip-block 'decl-name))]
         [(and (string=? text ";")
               (top-level-prelude?))
          (set! prelude-parts '())
          (set! current-rule-at? #f)]
         [else
          (when (token-has-tag? token 'at-rule-name)
            (set! current-rule-at? #t))
          (set! prelude-parts (cons text prelude-parts))])]
      [(eq? state 'skip-block)
       (when (and (string=? text "}")
                  (= previous-brace-depth 1)
                  (zero? paren-depth)
                  (zero? bracket-depth))
         (finish-rule!))]
      [(eq? state 'decl-name)
       (cond
         [(and (string=? text ":")
               (top-level-value?))
          (set! current-name
                (string-trim (apply string-append (reverse decl-name-parts))))
          (set! decl-name-parts '())
          (set! state 'decl-value)]
         [(and (string=? text ";")
               (top-level-value?))
          (set! decl-name-parts '())]
         [(and (string=? text "}")
               (= previous-brace-depth 1)
               (zero? paren-depth)
               (zero? bracket-depth))
          (finish-rule!)]
         [else
          (set! decl-name-parts (cons text decl-name-parts))])]
      [(eq? state 'decl-value)
       (cond
         [(and (string=? text ";")
               (top-level-value?))
          (finish-declaration!)
          (set! state 'decl-name)]
         [(and (string=? text "}")
               (= previous-brace-depth 1)
               (zero? paren-depth)
               (zero? bracket-depth))
          (finish-declaration!)
          (finish-rule!)]
         [else
          (set! decl-value-parts (cons text decl-value-parts))])])
    (let-values ([(new-paren new-bracket new-brace)
                  (update-delimiter-depths text paren-depth bracket-depth brace-depth)])
      (set! paren-depth new-paren)
      (set! bracket-depth new-bracket)
      (set! brace-depth new-brace)))

  (when (not (eq? state 'prelude))
    (fail "could not parse CSS rules cleanly; unterminated block near selector ~a"
          (or current-selector "<unknown>")))
  (reverse rules))

;; extract-top-level-imports : string? -> (listof string?)
;;   Preserve top-level @import rules for re-emission in generated stylesheets.
(define (extract-top-level-imports source)
  (for/list ([line (in-list (string-split (strip-css-comments source) "\n"))]
             #:when (regexp-match? #px"^\\s*@import\\b.*;\\s*$" line))
    (string-trim line)))

;; collect-custom-properties : (listof css-rule?) string? -> (hash/c string? string?)
;;   Collect custom properties for selector from matching rules, later declarations win.
(define (collect-custom-properties rules selector)
  (define props (make-hash))
  (for ([rule (in-list rules)]
        #:when (selector-list-matches? (css-rule-selector rule) selector))
    (for ([decl (in-list (css-rule-declarations rule))]
          #:when (string-prefix? (css-decl-name decl) "--"))
      (hash-set! props (css-decl-name decl) (css-decl-value decl))))
  props)

;; extract-bootstrap-theme : string? string? -> (hash/c string? string?)
;;   Parse source and return matching Bootstrap custom properties for selector.
(define (extract-bootstrap-theme source selector)
  (unless (regexp-match? selector-regexp selector)
    (fail "unsupported selector ~s; use :root, [data-bs-theme=light], or [data-bs-theme=dark]"
          selector))
  (define props
    (collect-custom-properties (read-css-rules source) selector))
  (when (zero? (hash-count props))
    (fail "no custom properties found for selector ~s" selector))
  props)

;; resolve-prop-value : (hash/c string? string?) string? [string?] [listof string?] -> string?
;;   Resolve one Bootstrap property, following simple custom-property var(...) indirections.
(define (resolve-prop-value props key [fallback-key #f] [seen '()])
  (when (member key seen)
    (fail "cyclic custom property reference while resolving ~a" key))
  (define raw
    (cond
      [(hash-has-key? props key)
       (hash-ref props key)]
      [(and fallback-key (hash-has-key? props fallback-key))
       (resolve-prop-value props fallback-key #f (cons key seen))]
      [else
       (hash-ref bootstrap-defaults key
                 (lambda ()
                   (if fallback-key
                       (resolve-prop-value props fallback-key #f (cons key seen))
                       (fail "missing required theme field for ~a" key))))]))
  (define var-match
    (regexp-match #px"^var\\(\\s*(--[-_a-zA-Z0-9]+)\\s*(?:,\\s*([^\\)]+))?\\s*\\)$"
                  (string-trim raw)))
  (cond
    [var-match
     (define ref-key (list-ref var-match 1))
     (define fallback-text (and (> (length var-match) 2) (list-ref var-match 2)))
     (cond
       [(or (hash-has-key? props ref-key)
            (hash-has-key? bootstrap-defaults ref-key))
        (resolve-prop-value props ref-key #f (cons key seen))]
       [fallback-text
        (string-trim fallback-text)]
       [else
        raw])]
    [else
     raw]))

;; prop-ref* : (hash/c string? string?) string? [string?] -> string?
;;   Read one Bootstrap property with optional fallback default.
(define (prop-ref* props key [fallback-key #f])
  (resolve-prop-value props key fallback-key))

;; bootstrap-props->theme-model : (hash/c string? string?) -> bootstrap-theme?
;;   Normalize Bootstrap custom properties into a small semantic theme model.
(define (bootstrap-props->theme-model props)
  (bootstrap-theme
   (prop-ref* props "--bs-body-color")
   (prop-ref* props "--bs-body-bg")
   (prop-ref* props "--bs-border-color")
   (prop-ref* props "--bs-border-color-translucent" "--bs-border-color")
   (prop-ref* props "--bs-primary")
   (prop-ref* props "--bs-secondary")
   (prop-ref* props "--bs-success")
   (prop-ref* props "--bs-info")
   (prop-ref* props "--bs-warning")
   (prop-ref* props "--bs-danger")
   (prop-ref* props "--bs-light")
   (prop-ref* props "--bs-dark")
   (prop-ref* props "--bs-body-font-family")
   (prop-ref* props "--bs-headings-font-family" "--bs-body-font-family")
   "0.875rem"
   (prop-ref* props "--bs-body-font-size")
   "1.15rem"
   (prop-ref* props "--bs-body-line-height")
   "400"
   "600"
   "700"
   (prop-ref* props "--bs-border-radius-sm")
   (prop-ref* props "--bs-border-radius")
   (prop-ref* props "--bs-border-radius-lg")
   (prop-ref* props "--bs-border-radius-pill")
   (prop-ref* props "--bs-box-shadow-sm")
   (prop-ref* props "--bs-box-shadow")
   (prop-ref* props "--bs-box-shadow-lg")
   props))

;; parse-byte-hex : string? -> integer?
;;   Parse one or two hex digits, duplicating a single nibble.
(define (parse-byte-hex s)
  (define text
    (if (= (string-length s) 1)
        (string-append s s)
        s))
  (or (string->number text 16)
      (fail "invalid hex color component ~a" s)))

;; parse-alpha-fragment : string? -> real?
;;   Parse alpha as raw decimal or percentage.
(define (parse-alpha-fragment s)
  (cond
    [(string-suffix? s "%")
     (/ (or (string->number (substring s 0 (sub1 (string-length s))))
            (fail "invalid alpha fragment ~a" s))
        100.0)]
    [else
     (or (string->number s)
         (fail "invalid alpha fragment ~a" s))]))

;; parse-channel-fragment : string? -> integer?
;;   Parse one rgb channel as integer or percentage.
(define (parse-channel-fragment s)
  (define value
    (cond
      [(string-suffix? s "%")
       (define pct
         (or (string->number (substring s 0 (sub1 (string-length s))))
             (fail "invalid percentage channel ~a" s)))
       (* 255.0 (/ pct 100.0))]
      [else
       (or (string->number s)
           (fail "invalid rgb channel ~a" s))]))
  (inexact->exact (round (min 255 (max 0 value)))))

;; parse-hex-color : string? -> (or/c rgb? #f)
;;   Parse a CSS hex color.
(define (parse-hex-color s)
  (define hex (substring s 1))
  (case (string-length hex)
    [(3)
     (rgb (parse-byte-hex (substring hex 0 1))
          (parse-byte-hex (substring hex 1 2))
          (parse-byte-hex (substring hex 2 3))
          1.0)]
    [(4)
     (rgb (parse-byte-hex (substring hex 0 1))
          (parse-byte-hex (substring hex 1 2))
          (parse-byte-hex (substring hex 2 3))
          (/ (parse-byte-hex (substring hex 3 4)) 255.0))]
    [(6)
     (rgb (parse-byte-hex (substring hex 0 2))
          (parse-byte-hex (substring hex 2 4))
          (parse-byte-hex (substring hex 4 6))
          1.0)]
    [(8)
     (rgb (parse-byte-hex (substring hex 0 2))
          (parse-byte-hex (substring hex 2 4))
          (parse-byte-hex (substring hex 4 6))
          (/ (parse-byte-hex (substring hex 6 8)) 255.0))]
    [else
     #f]))

;; parse-rgb-color : string? -> (or/c rgb? #f)
;;   Parse rgb()/rgba() in comma or modern space syntax.
(define (parse-rgb-color s)
  (define m
    (regexp-match #px"^(rgba?)\\((.*)\\)$" (string-downcase (string-trim s))))
  (and m
       (let* ([body (string-trim (list-ref m 2))]
              [pieces
               (if (regexp-match? #px"," body)
                   (map string-trim (string-split body ","))
                   (let* ([slash-parts (map string-trim (string-split body "/"))]
                          [channels
                           (filter (lambda (x) (not (string=? x "")))
                                   (regexp-split #px"[ \t\r\n]+" (car slash-parts)))]
                          [alpha-parts
                           (if (> (length slash-parts) 1)
                               (list (cadr slash-parts))
                               '())])
                     (append channels alpha-parts)))]
              [count (length pieces)])
         (and (or (= count 3) (= count 4))
              (rgb (parse-channel-fragment (list-ref pieces 0))
                   (parse-channel-fragment (list-ref pieces 1))
                   (parse-channel-fragment (list-ref pieces 2))
                   (if (= count 4)
                       (parse-alpha-fragment (list-ref pieces 3))
                       1.0))))))

;; parse-css-color : string? -> (or/c rgb? #f)
;;   Parse one CSS color string when supported.
(define (parse-css-color s)
  (cond
    [(not s)
     #f]
    [else
     (define text (string-trim s))
     (cond
       [(regexp-match? #px"^#[0-9a-fA-F]+$" text)
        (parse-hex-color text)]
       [(regexp-match? #px"^(?i:rgba?)\\(" text)
        (parse-rgb-color text)]
       [else
        #f])]))

;; clamp-byte : real? -> integer?
;;   Clamp a numeric color component into byte range.
(define (clamp-byte v)
  (inexact->exact (round (min 255 (max 0 v)))))

;; clamp-alpha : real? -> real?
;;   Clamp alpha into [0, 1].
(define (clamp-alpha v)
  (min 1.0 (max 0.0 v)))

;; mix-colors : rgb? rgb? real? -> rgb?
;;   Mix c1 toward c2 by ratio.
(define (mix-colors c1 c2 ratio)
  (define t (min 1.0 (max 0.0 ratio)))
  (define (mix-one a b)
    (+ (* a (- 1.0 t)) (* b t)))
  (rgb (clamp-byte (mix-one (rgb-r c1) (rgb-r c2)))
       (clamp-byte (mix-one (rgb-g c1) (rgb-g c2)))
       (clamp-byte (mix-one (rgb-b c1) (rgb-b c2)))
       (clamp-alpha (mix-one (rgb-a c1) (rgb-a c2)))))

;; rgb->css : rgb? -> string?
;;   Render c as #rrggbb or rgba(...).
(define (rgb->css c)
  (define (two-hex n)
    (~a (string-downcase (number->string n 16))
        #:min-width 2
        #:align 'right
        #:left-pad-string "0"))
  (if (>= (rgb-a c) 0.999)
      (string-append "#"
                     (two-hex (rgb-r c))
                     (two-hex (rgb-g c))
                     (two-hex (rgb-b c)))
      (format "rgba(~a, ~a, ~a, ~a)"
              (rgb-r c)
              (rgb-g c)
              (rgb-b c)
              (~r (rgb-a c) #:precision '(= 3)))))

;; srgb->linear : integer? -> real?
;;   Convert one sRGB channel to linear space.
(define (srgb->linear x)
  (define v (/ x 255.0))
  (if (<= v 0.04045)
      (/ v 12.92)
      (expt (/ (+ v 0.055) 1.055) 2.4)))

;; relative-luminance : rgb? -> real?
;;   Compute WCAG relative luminance.
(define (relative-luminance c)
  (+ (* 0.2126 (srgb->linear (rgb-r c)))
     (* 0.7152 (srgb->linear (rgb-g c)))
     (* 0.0722 (srgb->linear (rgb-b c)))))

;; contrast-ratio : rgb? rgb? -> real?
;;   Compute contrast ratio between foreground and background.
(define (contrast-ratio c1 c2)
  (define l1 (relative-luminance c1))
  (define l2 (relative-luminance c2))
  (define hi (max l1 l2))
  (define lo (min l1 l2))
  (/ (+ hi 0.05) (+ lo 0.05)))

;; infer-light-or-dark-theme? : bootstrap-theme? -> boolean?
;;   Infer whether the theme behaves like a dark theme.
(define (infer-light-or-dark-theme? theme)
  (define bg (or (parse-css-color (bootstrap-theme-bg theme))
                 (rgb 255 255 255 1.0)))
  (< (relative-luminance bg) 0.45))

;; derive-on-color : rgb? -> string?
;;   Pick a readable foreground for background c.
(define (derive-on-color c)
  (define white (rgb 255 255 255 1.0))
  (define black (rgb 17 24 39 1.0))
  (if (> (contrast-ratio white c) (contrast-ratio black c))
      (rgb->css white)
      (rgb->css black)))

;; maybe-color->css : (or/c rgb? #f) string? -> string?
;;   Render parsed color or preserve original fallback text.
(define (maybe-color->css c fallback)
  (if c (rgb->css c) fallback))

;; theme-prop-ref : bootstrap-theme? string? [string?] -> string?
;;   Read a Bootstrap property from the preserved theme property map with optional fallback key.
(define (theme-prop-ref theme key [fallback-key #f])
  (define props (bootstrap-theme-props theme))
  (cond
    [(hash-has-key? props key)
     (hash-ref props key)]
    [(and fallback-key (hash-has-key? props fallback-key))
     (hash-ref props fallback-key)]
    [else
     #f]))

;; derive-emphasis-color : rgb? boolean? -> rgb?
;;   Nudge semantic color toward higher contrast for text/hover use.
(define (derive-emphasis-color c dark?)
  (mix-colors c
              (if dark?
                  (rgb 255 255 255 1.0)
                  (rgb 0 0 0 1.0))
              (if dark? 0.26 0.18)))

;; derive-subtle-color : rgb? rgb? boolean? -> rgb?
;;   Derive a softened background tint for semantic families.
(define (derive-subtle-color c bg dark?)
  (define ratio
    (cond
      [dark?
       (if (neutral-color? c) 0.70 0.74)]
      [else
       (if (neutral-color? c) 0.72 0.78)]))
  (mix-colors c bg ratio))

;; derive-border-color : rgb? rgb? boolean? -> rgb?
;;   Derive a semantic border color between semantic and neutral border.
(define (derive-border-color c border dark?)
  (define ratio
    (cond
      [dark? 0.50]
      [(neutral-color? c) 0.44]
      [else 0.38]))
  (mix-colors c border ratio))

;; derive-muted-color : rgb? rgb? -> rgb?
;;   Soften foreground text toward the page background.
(define (derive-muted-color fg bg)
  (mix-colors fg bg 0.38))

;; color-distance : rgb? rgb? -> real?
;;   Measure a simple RGB channel distance between two colors.
(define (color-distance c1 c2)
  (/ (+ (abs (- (rgb-r c1) (rgb-r c2)))
        (abs (- (rgb-g c1) (rgb-g c2)))
        (abs (- (rgb-b c1) (rgb-b c2))))
     3.0))

;; color-channel-spread : rgb? -> integer?
;;   Measure rough chroma by channel spread.
(define (color-channel-spread c)
  (- (max (rgb-r c) (rgb-g c) (rgb-b c))
     (min (rgb-r c) (rgb-g c) (rgb-b c))))

;; neutral-color? : rgb? -> boolean?
;;   Determine whether c reads as a neutral surface color.
(define (neutral-color? c)
  (<= (color-channel-spread c) 42))

;; surface-compatible-color? : rgb? rgb? boolean? -> boolean?
;;   Determine whether candidate is a plausible neutral surface for the theme mode.
(define (surface-compatible-color? candidate bg dark?)
  (define candidate-lum (relative-luminance candidate))
  (define bg-lum (relative-luminance bg))
  (if dark?
      (and (> candidate-lum (+ bg-lum 0.03))
           (< candidate-lum 0.55))
      (and (< candidate-lum (- bg-lum 0.03))
           (> candidate-lum 0.18))))

;; choose-surface-candidate : rgb? boolean? (listof (cons/c rgb? boolean?)) rgb? -> rgb?
;;   Choose the first compatible surface candidate, preferring explicit Bootstrap neutrals.
(define (choose-surface-candidate bg dark? candidates fallback)
  (or (for/or ([entry (in-list candidates)])
        (define c (car entry))
        (define must-be-neutral? (cdr entry))
        (and c
             (or (not must-be-neutral?) (neutral-color? c))
             (surface-compatible-color? c bg dark?)
             c))
      fallback))

;; boost-subtle-color : rgb? rgb? rgb? boolean? -> rgb?
;;   Strengthen a subtle semantic background when boost mode is enabled.
(define (boost-subtle-color subtle base bg dark?)
  (define derived (derive-subtle-color base bg dark?))
  (cond
    [dark?
     subtle]
    [(or (neutral-color? subtle)
         (< (color-distance subtle bg) 16)
         (< (contrast-ratio subtle bg) 1.10))
     (mix-colors subtle derived 0.42)]
    [else
     subtle]))

;; boost-border-color : rgb? rgb? rgb? boolean? -> rgb?
;;   Strengthen a semantic border when boost mode is enabled.
(define (boost-border-color border* base neutral-border dark?)
  (define derived (derive-border-color base neutral-border dark?))
  (cond
    [dark?
     border*]
    [(or (neutral-color? border*)
         (< (color-distance border* neutral-border) 22))
     (mix-colors border* derived 0.46)]
    [else
     border*]))

;; derive-surface-colors : rgb? rgb? boolean? rgb? rgb? rgb? rgb? rgb? rgb? boolean? -> (hash/c symbol? string?)
;;   Derive neutral surface variants from background and foreground.
(define (derive-surface-colors bg fg dark? secondary-bg tertiary-bg neutral-secondary neutral-light neutral-dark accent boost?)
  (define raised-default (mix-colors bg fg (if dark? 0.10 0.03)))
  (define subtle-default (mix-colors bg fg (if dark? 0.06 0.05)))
  (define raised-base
    (choose-surface-candidate
     bg dark?
     (list (cons tertiary-bg #f)
           (cons secondary-bg #f)
           (cons neutral-secondary #t)
           (cons neutral-dark #t)
           (cons neutral-light #t))
     raised-default))
  (define accent-surface
    (mix-colors accent bg (if dark? 0.82 0.91)))
  (define raised-ratio (if boost? 0.24 0.14))
  (define raised
    (if dark?
        raised-base
        (mix-colors raised-base accent-surface raised-ratio)))
  (define subtle-seed
    (choose-surface-candidate
     bg dark?
     (list (cons secondary-bg #f)
           (cons neutral-secondary #t)
           (cons tertiary-bg #f)
           (cons neutral-dark #t)
           (cons neutral-light #t))
     subtle-default))
  (define subtle-base (mix-colors bg subtle-seed (if dark? 0.45 0.35)))
  (define subtle-ratio (if boost? 0.28 0.18))
  (define subtle
    (if dark?
        subtle-base
        (mix-colors subtle-base accent-surface subtle-ratio)))
  (define muted-base (mix-colors raised fg (if dark? 0.08 0.05)))
  (define muted-ratio (if boost? 0.34 0.24))
  (define muted
    (if dark?
        muted-base
        (mix-colors muted-base accent-surface muted-ratio)))
  (define hover  (mix-colors raised fg (if dark? 0.14 0.08)))
  (define active (mix-colors subtle fg (if dark? 0.22 0.14)))
  (define table-header-ratio (if boost? 0.46 0.32))
  (define table-header
    (if dark?
        muted
        (mix-colors muted accent-surface table-header-ratio)))
  (hash 'surface (rgb->css bg)
        'surface-raised (rgb->css raised)
        'surface-subtle (rgb->css subtle)
        'surface-muted (rgb->css muted)
        'surface-hover (rgb->css hover)
        'surface-active (rgb->css active)
        'table-header-bg (rgb->css table-header)))

;; build-token-lines : bootstrap-theme? [boolean?] -> (listof (cons/c string? string?))
;;   Derive the complete web-easy token set for emitted CSS.
(define (build-token-lines theme [boost? #f])
  (define dark? (infer-light-or-dark-theme? theme))
  (define bg (or (parse-css-color (bootstrap-theme-bg theme))
                 (parse-css-color (hash-ref bootstrap-defaults "--bs-body-bg"))))
  (define fg (or (parse-css-color (bootstrap-theme-fg theme))
                 (parse-css-color (hash-ref bootstrap-defaults "--bs-body-color"))))
  (define emphasis-fg
    (or (parse-css-color (theme-prop-ref theme "--bs-emphasis-color"))
        fg))
  (define secondary-fg
    (or (parse-css-color (theme-prop-ref theme "--bs-secondary-color"))
        (derive-muted-color fg bg)))
  (define tertiary-fg
    (or (parse-css-color (theme-prop-ref theme "--bs-tertiary-color"))
        (mix-colors fg bg 0.50)))
  (define secondary-bg
    (parse-css-color (theme-prop-ref theme "--bs-secondary-bg")))
  (define tertiary-bg
    (parse-css-color (theme-prop-ref theme "--bs-tertiary-bg")))
  (define neutral-secondary
    (parse-css-color (bootstrap-theme-secondary theme)))
  (define neutral-light
    (parse-css-color (bootstrap-theme-light theme)))
  (define neutral-dark
    (parse-css-color (bootstrap-theme-dark theme)))
  (define border (or (parse-css-color (bootstrap-theme-border theme))
                     (mix-colors fg bg 0.70)))
  (define border-soft
    (or (parse-css-color (bootstrap-theme-border-soft theme))
        (mix-colors border bg 0.25)))
  (define focus-base
    (or (parse-css-color (theme-prop-ref theme "--bs-focus-ring-color"))
        (parse-css-color (bootstrap-theme-primary theme))
        (parse-css-color (hash-ref bootstrap-defaults "--bs-primary"))))
  (define focus-color-source
    (or (parse-css-color (theme-prop-ref theme "--bs-link-hover-color"))
        (derive-emphasis-color focus-base dark?)))
  (define focus-color
    (rgb->css focus-color-source))
  (define focus-tint
    (or (theme-prop-ref theme "--bs-focus-ring-color")
        (rgb->css
         (struct-copy rgb focus-base [a (if dark? 0.24 0.20)]))))
  (define surface-map
    (derive-surface-colors bg fg dark? secondary-bg tertiary-bg
                           neutral-secondary neutral-light neutral-dark
                           focus-base boost?))
  (define default-primary-subtle
    (derive-subtle-color focus-base bg dark?))
  (define bg-selected
    (let ([explicit (parse-css-color (theme-prop-ref theme "--bs-primary-bg-subtle"))])
      (cond
        [(and explicit boost?)
         (rgb->css (boost-subtle-color explicit focus-base bg dark?))]
        [explicit
         (rgb->css explicit)]
        [else
         (rgb->css default-primary-subtle)])))
  (define bg-disabled
    (rgb->css (mix-colors bg fg (if dark? 0.12 0.05))))
  (define bg-hover
    (hash-ref surface-map 'surface-hover))
  (define fg-muted
    (rgb->css secondary-fg))
  (define border-muted
    (rgb->css (mix-colors border bg 0.18)))
  (define border-hover-rgb
    (mix-colors border fg 0.22))
  (define border-hover
    (rgb->css border-hover-rgb))
  (define border-strong
    (rgb->css (mix-colors border focus-base 0.55)))
  (define separator
    (rgb->css (mix-colors border bg 0.28)))
  (define separator-strong
    (rgb->css (mix-colors border fg 0.12)))
  (define control-border
    (rgb->css (mix-colors border border-soft 0.40)))
  (define control-border-hover
    (rgb->css (mix-colors border-hover-rgb fg 0.18)))
  (define control-border-focus
    (rgb->css (mix-colors focus-base fg (if dark? 0.12 0.05))))
  (define control-border-disabled
    (rgb->css (mix-colors border bg 0.35)))
  (define popup-bg
    (hash-ref surface-map 'surface-raised))
  (define popup-border
    (rgb->css border))
  (define placeholder
    (rgb->css tertiary-fg))
  (define overlay
    (if dark? "rgba(2, 6, 12, 0.72)" "rgba(0, 0, 0, 0.45)"))
  (define semantic-bases
    (list (cons "primary"   (bootstrap-theme-primary theme))
          (cons "secondary" (bootstrap-theme-secondary theme))
          (cons "success"   (bootstrap-theme-success theme))
          (cons "info"      (bootstrap-theme-info theme))
          (cons "warning"   (bootstrap-theme-warning theme))
          (cons "danger"    (bootstrap-theme-danger theme))
          (cons "light"     (bootstrap-theme-light theme))
          (cons "dark"      (bootstrap-theme-dark theme))))
  (define semantic-lines
    (append-map
     (lambda (entry)
       (define name (car entry))
       (define source-text (cdr entry))
       (define parsed
         (or (parse-css-color source-text)
             (parse-css-color (hash-ref bootstrap-defaults (string-append "--bs-" name)))))
       (define subtle
         (let ([explicit (parse-css-color (theme-prop-ref theme (format "--bs-~a-bg-subtle" name)))]
               [derived (derive-subtle-color parsed bg dark?)])
           (cond
             [(and explicit boost?)
              (rgb->css (boost-subtle-color explicit parsed bg dark?))]
             [explicit
              (rgb->css explicit)]
             [else
              (rgb->css derived)])))
       (define border*
         (let ([explicit (parse-css-color (theme-prop-ref theme (format "--bs-~a-border-subtle" name)))]
               [derived (derive-border-color parsed border dark?)])
           (cond
             [(and explicit boost?)
              (rgb->css (boost-border-color explicit parsed border dark?))]
             [explicit
              (rgb->css explicit)]
             [else
              (rgb->css derived)])))
       (define emphasis
         (or (theme-prop-ref theme (format "--bs-~a-text-emphasis" name))
             (rgb->css (derive-emphasis-color parsed dark?))))
       (list (cons (format "--we-~a" name) source-text)
             (cons (format "--we-~a-on" name) (derive-on-color parsed))
             (cons (format "--we-~a-emphasis" name) emphasis)
             (cons (format "--we-~a-subtle" name) subtle)
             (cons (format "--we-~a-border" name) border*)))
     semantic-bases))
  (append
   (list
    (cons "--we-focus" focus-color)
    (cons "--we-focus-tint" focus-tint)
    (cons "--we-fg" (bootstrap-theme-fg theme))
    (cons "--we-bg" (bootstrap-theme-bg theme))
    (cons "--we-bg-subtle" (hash-ref surface-map 'surface-subtle))
    (cons "--we-bg-selected" bg-selected)
    (cons "--we-bg-disabled" bg-disabled)
    (cons "--we-bg-hover" bg-hover)
    (cons "--we-border" (bootstrap-theme-border theme))
    (cons "--we-border-menu" popup-border)
    (cons "--we-border-muted" border-muted)
    (cons "--we-border-soft" (maybe-color->css border-soft (bootstrap-theme-border-soft theme)))
    (cons "--we-border-hover" border-hover)
    (cons "--we-border-strong" border-strong)
    (cons "--we-fg-muted" fg-muted)
    (cons "--we-overlay" overlay)
    (cons "--we-shadow-sm" (bootstrap-theme-shadow-sm theme))
    (cons "--we-shadow-md" (bootstrap-theme-shadow-md theme))
    (cons "--we-shadow-lg" (bootstrap-theme-shadow-lg theme))
    (cons "--we-shadow" "var(--we-shadow-lg)")
    (cons "--we-menu-item-hover-bg" bg-hover)
    (cons "--we-menu-item-hover-fg" (rgb->css emphasis-fg))
    (cons "--we-tab-active-border" bg-selected)
    (cons "--we-input-placeholder" placeholder)
    (cons "--we-surface" (hash-ref surface-map 'surface))
    (cons "--we-surface-raised" (hash-ref surface-map 'surface-raised))
    (cons "--we-surface-subtle" (hash-ref surface-map 'surface-subtle))
    (cons "--we-surface-muted" (hash-ref surface-map 'surface-muted))
    (cons "--we-surface-hover" (hash-ref surface-map 'surface-hover))
    (cons "--we-surface-active" (hash-ref surface-map 'surface-active))
    (cons "--we-separator" separator)
    (cons "--we-separator-strong" separator-strong)
    (cons "--we-table-header-bg" (hash-ref surface-map 'table-header-bg))
    (cons "--we-table-header-fg" (rgb->css emphasis-fg))
    (cons "--we-table-cell-border" separator)
    (cons "--we-control-border" control-border)
    (cons "--we-control-border-hover" control-border-hover)
    (cons "--we-control-border-focus" control-border-focus)
    (cons "--we-control-border-disabled" control-border-disabled)
    (cons "--we-popup-bg" popup-bg)
    (cons "--we-popup-border" popup-border)
    (cons "--we-popup-shadow" "var(--we-shadow-md)"))
   semantic-lines
   (list
    (cons "--we-space-xs" "2px")
    (cons "--we-space-sm" "4px")
    (cons "--we-space-md" "8px")
    (cons "--we-space-lg" "10px")
    (cons "--we-radius-sm" (bootstrap-theme-radius-sm theme))
    (cons "--we-radius-md" (bootstrap-theme-radius-md theme))
    (cons "--we-radius-lg" (bootstrap-theme-radius-lg theme))
    (cons "--we-radius-pill" (bootstrap-theme-radius-pill theme))
    (cons "--we-font-family-base" (bootstrap-theme-font-family-base theme))
    (cons "--we-font-family-heading" (bootstrap-theme-font-family-heading theme))
    (cons "--we-font-size-sm" (bootstrap-theme-font-size-sm theme))
    (cons "--we-font-size-md" (bootstrap-theme-font-size-md theme))
    (cons "--we-font-size-lg" (bootstrap-theme-font-size-lg theme))
    (cons "--we-line-height-base" (bootstrap-theme-line-height-base theme))
    (cons "--we-font-weight-normal" (bootstrap-theme-font-weight-normal theme))
    (cons "--we-font-weight-semibold" (bootstrap-theme-font-weight-semibold theme))
    (cons "--we-font-weight-bold" (bootstrap-theme-font-weight-bold theme))
    (cons "--we-menu-popup-offset" "2px")
    (cons "--we-menu-popup-min-width" "150px")
    (cons "--we-menu-popup-gap" "0px"))))

;; token-block->string : string? (listof (cons/c string? string?)) -> string?
;;   Emit one CSS variable block for theme-class.
(define (token-block->string theme-class token-lines)
  (define selector (format "html.~a" theme-class))
  (string-append
   "/* -------------------------------------------------------------------------- */\n"
   "/* 1) Design Tokens (generated from Bootstrap custom properties)              */\n"
   "/* -------------------------------------------------------------------------- */\n"
   selector
   " {\n"
   (apply string-append
          (for/list ([entry (in-list token-lines)])
            (format "  ~a: ~a;\n" (car entry) (cdr entry))))
   "  color: var(--we-fg);\n"
   "}\n\n"
   selector
   " {\n"
   "  background: var(--we-bg);\n"
   "  margin: 0;\n"
   "  padding: 0;\n"
   "}\n\n"
   selector
   " body {\n"
   "  background: var(--we-bg);\n"
   "  color: var(--we-fg);\n"
   "  font-family: var(--we-font-family-base);\n"
   "  font-size: var(--we-font-size-md);\n"
   "  line-height: var(--we-line-height-base);\n"
   "  margin: 0;\n"
   "  padding: 0;\n"
   "}\n\n"))

;; theme-template-suffix : string? -> string?
;;   Load the starter theme template suffix and rewrite the selector class.
(define (theme-template-suffix theme-class)
  (define template (file->string theme-template-path))
  (define marker-match
    (regexp-match-positions (regexp-quote css-section-2-marker) template))
  (unless marker-match
    (fail "could not find section 2 marker in theme template"))
  (define suffix (substring template (caar marker-match)))
  (string-replace suffix "html.we-theme-light" (format "html.~a" theme-class)))

;; emit-theme-stylesheet : bootstrap-theme? string? (listof string?) [boolean?] -> string?
;;   Emit the generated external theme stylesheet.
(define (emit-theme-stylesheet theme theme-class import-lines [boost? #f])
  (string-append
   (if (null? import-lines)
       ""
       (string-append
        (string-join import-lines "\n")
        "\n\n"))
   "/*\n"
   " * Generated by thematic / tools/thematic.\n"
   " *\n"
   " * Manual validation workflow:\n"
   " * 1) Run this tool against a Bootstrap stylesheet.\n"
   " * 2) Load the output beside the browser theme showcase.\n"
   " * 3) Compare the generated theme against the existing starter themes.\n"
   " */\n\n"
   (token-block->string theme-class (build-token-lines theme boost?))
   (theme-template-suffix theme-class)))

;; write-generated-theme! : path-string? path-string? string? string? boolean? -> void
;;   Generate and write one theme stylesheet from Bootstrap CSS.
(define (write-generated-theme! input-path output-path theme-class selector boost?)
  (define source (file->string input-path))
  (define import-lines (extract-top-level-imports source))
  (define theme
    (bootstrap-props->theme-model (extract-bootstrap-theme source selector)))
  (make-parent-directory* output-path)
  (call-with-output-file output-path
    (lambda (out)
      (display (emit-theme-stylesheet theme theme-class import-lines boost?) out))
    #:exists 'truncate/replace))

;; main : -> void
;;   Parse command-line arguments and run the generator.
(define (main)
  (define input-path #f)
  (define output-path #f)
  (define theme-class "we-theme-bootstrap")
  (define selector ":root")
  (define boost? #f)
  (command-line
   #:program "thematic"
   #:once-each
   [("--input") path "Path to the Bootstrap CSS file."
                (set! input-path path)]
   [("--output") path "Path to the generated theme CSS file."
                 (set! output-path path)]
   [("--theme-class") class-name "Theme class to emit on the <html> element."
                      (set! theme-class class-name)]
   [("--selector") selector-text "Bootstrap selector to read, such as :root."
                   (set! selector selector-text)]
   [("--accent-boost") "Strengthen low-contrast accent companions for web-easy."
                       (set! boost? #t)])
  (unless input-path
    (fail "missing required --input path"))
  (unless output-path
    (fail "missing required --output path"))
  (write-generated-theme! input-path output-path theme-class selector boost?))

(module+ main
  (main))

(module+ test
  (require rackunit)

  (define sample-bootstrap
    (string-append
     "/* comment */\n"
     ":root {\n"
     "  --bs-body-color: #212529;\n"
     "  --bs-body-bg: #ffffff;\n"
     "  --bs-border-color: #dee2e6;\n"
     "  --bs-primary: #0d6efd;\n"
     "  --bs-secondary: #6c757d;\n"
     "  --bs-success: #198754;\n"
     "  --bs-info: #0dcaf0;\n"
     "  --bs-warning: #ffc107;\n"
     "  --bs-danger: #dc3545;\n"
     "  --bs-light: #f8f9fa;\n"
     "  --bs-dark: #212529;\n"
     "  --bs-border-radius: 0.375rem;\n"
     "  --bs-border-radius-sm: 0.25rem;\n"
     "  --bs-border-radius-lg: 0.5rem;\n"
     "  --bs-border-radius-pill: 50rem;\n"
     "  --bs-box-shadow-sm: 0 0.125rem 0.25rem rgba(0, 0, 0, 0.075);\n"
     "  --bs-box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.15);\n"
     "  --bs-box-shadow-lg: 0 1rem 3rem rgba(0, 0, 0, 0.175);\n"
     "  --bs-body-font-family: system-ui, sans-serif;\n"
     "  --bs-body-font-size: 1rem;\n"
     "  --bs-body-line-height: 1.5;\n"
     "}\n"
     "[data-bs-theme=dark] {\n"
     "  --bs-body-color: #d9e2f3;\n"
     "  --bs-body-bg: #13161c;\n"
     "  --bs-border-color: rgba(217, 226, 243, 0.22);\n"
     "  --bs-primary: rgb(125 183 255 / 0.92);\n"
     "}\n"
     ".btn { color: red; }\n"))

  (define selector-list-bootstrap
    (string-append
     ":root,\n"
     "[data-bs-theme=light] {\n"
     "  --bs-body-color: #ebebeb;\n"
     "  --bs-body-bg: #0f2537;\n"
     "  --bs-border-color: #ced4da;\n"
     "  --bs-primary: #df6919;\n"
     "  --bs-secondary: #4e5d6c;\n"
     "  --bs-success: #5cb85c;\n"
     "  --bs-info: #5bc0de;\n"
     "  --bs-warning: #ffc107;\n"
     "  --bs-danger: #d9534f;\n"
     "  --bs-light: #abb6c2;\n"
     "  --bs-dark: #20374c;\n"
     "  --bs-primary-text-emphasis: #592a0a;\n"
     "  --bs-primary-bg-subtle: #f9e1d1;\n"
     "  --bs-primary-border-subtle: #f2c3a3;\n"
     "  --bs-secondary-color: rgba(235, 235, 235, 0.75);\n"
     "  --bs-tertiary-color: rgba(235, 235, 235, 0.5);\n"
     "  --bs-secondary-bg: #dee2e6;\n"
     "  --bs-tertiary-bg: #ebebeb;\n"
     "  --bs-focus-ring-color: rgba(223, 105, 25, 0.25);\n"
     "  --bs-body-font-family: Lato, sans-serif;\n"
     "  --bs-body-font-size: 1rem;\n"
     "  --bs-body-line-height: 1.5;\n"
     "}\n"))

  (define function-bootstrap
    ":root { --bs-focus-ring-color: rgba(13, 110, 253, 0.25); --bs-primary: rgb(13 110 253 / 92%); --bs-body-bg: #fff; --bs-body-color: #111; --bs-border-color: #ddd; }")

  (define import-bootstrap
    (string-append
     "@import url(\"https://fonts.googleapis.com/css2?family=Lato:wght@300;400;700&display=swap\");\n"
     "@import url(\"theme-extra.css\") layer(theme);\n"
     ":root { --bs-body-bg: #fff; --bs-body-color: #111; --bs-border-color: #ddd; --bs-primary: #0d6efd; }\n"))

  (define boost-bootstrap
    (string-append
     ":root {\n"
     "  --bs-body-color: #55595c;\n"
     "  --bs-body-bg: #ffffff;\n"
     "  --bs-border-color: #d8dde2;\n"
     "  --bs-primary: #325d88;\n"
     "  --bs-primary-bg-subtle: #f3f5f7;\n"
     "  --bs-primary-border-subtle: #d8dde2;\n"
     "}\n"))

  (test-case "read-css-rules parses top-level declarations"
    (define rules (read-css-rules sample-bootstrap))
    (check-equal? (length rules) 3)
    (check-equal? (css-rule-selector (first rules)) ":root")
    (check-equal? (css-decl-name (first (css-rule-declarations (first rules))))
                  "--bs-body-color"))

  (test-case "extract-bootstrap-theme matches requested selector only"
    (define props (extract-bootstrap-theme sample-bootstrap "[data-bs-theme=dark]"))
    (check-equal? (hash-ref props "--bs-body-bg") "#13161c")
    (check-false (hash-has-key? props "--bs-secondary")))

  (test-case "extract-bootstrap-theme matches selectors inside selector lists"
    (define props (extract-bootstrap-theme selector-list-bootstrap ":root"))
    (check-equal? (hash-ref props "--bs-body-bg") "#0f2537")
    (check-equal? (hash-ref props "--bs-primary-bg-subtle") "#f9e1d1"))

  (test-case "function values survive declaration parsing"
    (define props (extract-bootstrap-theme function-bootstrap ":root"))
    (check-equal? (hash-ref props "--bs-focus-ring-color")
                  "rgba(13, 110, 253, 0.25)")
    (check-equal? (hash-ref props "--bs-primary")
                  "rgb(13 110 253 / 92%)"))

  (test-case "bootstrap mapping falls back for headings font family"
    (define theme
      (bootstrap-props->theme-model
       (extract-bootstrap-theme sample-bootstrap ":root")))
    (check-equal? (bootstrap-theme-font-family-heading theme)
                  "system-ui, sans-serif"))

  (test-case "bootstrap mapping resolves font-family indirection"
    (define theme
      (bootstrap-props->theme-model
       (extract-bootstrap-theme
        "@import url(\"https://fonts.googleapis.com/css2?family=Nunito+Sans:wght@200;300;400;500;600;700&display=swap\");\n:root { --bs-font-sans-serif: \"Nunito Sans\", Arial, sans-serif; --bs-body-font-family: var(--bs-font-sans-serif); --bs-body-bg: #fff; --bs-body-color: #111; --bs-border-color: #ddd; --bs-primary: #0d6efd; }"
        ":root")))
    (check-equal? (bootstrap-theme-font-family-base theme)
                  "\"Nunito Sans\", Arial, sans-serif"))

  (test-case "derive-on-color chooses a readable foreground"
    (check-equal? (derive-on-color (rgb 10 20 30 1.0)) "#ffffff")
    (check-equal? (derive-on-color (rgb 250 245 235 1.0)) "#111827"))

  (test-case "infer-light-or-dark-theme? reacts to body background"
    (define light-theme
      (bootstrap-props->theme-model
       (extract-bootstrap-theme sample-bootstrap ":root")))
    (define dark-theme
      (bootstrap-props->theme-model
       (extract-bootstrap-theme sample-bootstrap "[data-bs-theme=dark]")))
    (check-false (infer-light-or-dark-theme? light-theme))
    (check-true (infer-light-or-dark-theme? dark-theme)))

  (test-case "emit-theme-stylesheet includes theme class and key tokens"
    (define css
      (emit-theme-stylesheet
       (bootstrap-props->theme-model
        (extract-bootstrap-theme sample-bootstrap ":root"))
       "we-theme-bootstrap"
       '()))
    (check-true (regexp-match? #px"html\\.we-theme-bootstrap \\{" css))
    (check-true (regexp-match? #px"--we-primary:" css))
    (check-true (regexp-match? #px"--we-surface-raised:" css))
    (check-true (regexp-match? #px"html\\.we-theme-bootstrap \\.we-button-primary" css)))

  (test-case "explicit bootstrap companion tokens are preferred when present"
    (define css
      (emit-theme-stylesheet
       (bootstrap-props->theme-model
        (extract-bootstrap-theme selector-list-bootstrap ":root"))
       "we-theme-superhero"
       '()))
    (check-true (regexp-match? #px"--we-primary-emphasis: #592a0a;" css))
    (check-true (regexp-match? #px"--we-primary-subtle: #f9e1d1;" css))
    (check-true (regexp-match? #px"--we-primary-border: #f2c3a3;" css))
    (check-true (regexp-match? #px"--we-focus-tint: rgba\\(223, 105, 25, 0\\.25\\);" css)))

  (test-case "dark selector-list themes avoid overly bright neutral surfaces"
    (define css
      (emit-theme-stylesheet
       (bootstrap-props->theme-model
        (extract-bootstrap-theme selector-list-bootstrap ":root"))
       "we-theme-superhero"
       '()))
    (check-true (regexp-match? #px"--we-surface-raised: #4e5d6c;" css))
    (check-true (regexp-match? #px"--we-popup-bg: #4e5d6c;" css))
    (check-false (regexp-match? #px"--we-surface-raised: #ebebeb;" css)))

  (test-case "derived light theme section chrome stays distinct from plain muted neutrals"
    (define token-lines
      (build-token-lines
       (bootstrap-props->theme-model
        (extract-bootstrap-theme sample-bootstrap ":root"))))
    (define token-map
      (for/hash ([entry (in-list token-lines)])
        (values (car entry) (cdr entry))))
    (check-not-equal? (hash-ref token-map "--we-table-header-bg")
                      (hash-ref token-map "--we-surface-muted")))

  (test-case "accent boost strengthens weak authored primary companion tokens"
    (define theme
      (bootstrap-props->theme-model
       (extract-bootstrap-theme boost-bootstrap ":root")))
    (define plain-map
      (for/hash ([entry (in-list (build-token-lines theme))])
        (values (car entry) (cdr entry))))
    (define boosted-map
      (for/hash ([entry (in-list (build-token-lines theme #t))])
        (values (car entry) (cdr entry))))
    (check-not-equal? (hash-ref plain-map "--we-primary-subtle")
                      (hash-ref boosted-map "--we-primary-subtle"))
    (check-not-equal? (hash-ref plain-map "--we-primary-border")
                      (hash-ref boosted-map "--we-primary-border")))

  (test-case "extract-top-level-imports preserves font imports"
    (check-equal? (extract-top-level-imports import-bootstrap)
                  '("@import url(\"https://fonts.googleapis.com/css2?family=Lato:wght@300;400;700&display=swap\");"
                    "@import url(\"theme-extra.css\") layer(theme);")))

  (test-case "generated stylesheets do not leak light-template border literals"
    (define css
      (emit-theme-stylesheet
       (bootstrap-props->theme-model
        (extract-bootstrap-theme sample-bootstrap ":root"))
       "we-theme-bootstrap"
       '()))
    (check-false (regexp-match? #px"#c0cee2" css))
    (check-false (regexp-match? #px"#9aa9c3" css)))

  (test-case "generated stylesheets emit imports before theme body"
    (define css
      (emit-theme-stylesheet
       (bootstrap-props->theme-model
        (extract-bootstrap-theme import-bootstrap ":root"))
       "we-theme-imports"
       (extract-top-level-imports import-bootstrap)))
    (check-true (regexp-match? #px"^@import url\\(\"https://fonts.googleapis.com/css2\\?family=Lato" css))
    (check-true (regexp-match? #px"@import url\\(\"theme-extra\\.css\"\\) layer\\(theme\\);" css))
    (check-true (regexp-match? #px"html\\.we-theme-imports \\{" css))))
