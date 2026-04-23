#lang racket/base

;; thematic : command-line -> void
;;   Generate a starter web-easy external theme from Bootstrap custom properties.

(provide main)

(require parsers/css
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
   props
   component-styles)
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
   "--bs-form-valid-color" "#198754"
   "--bs-form-valid-border-color" "#198754"
   "--bs-form-invalid-color" "#dc3545"
   "--bs-form-invalid-border-color" "#dc3545"
   "--bs-light" "#f8f9fa"
   "--bs-dark" "#212529"
   "--bs-body-font-family"
   "system-ui, -apple-system, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, sans-serif"
   "--bs-body-font-size" "1rem"
   "--bs-body-font-weight" "400"
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

(define css-var-regexp
  #px"var\\(\\s*(--[-_a-zA-Z0-9]+)\\s*(?:,\\s*([^\\)]+))?\\s*\\)")

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

;; read-css-rules : string? -> css-stylesheet?
;;   Parse source into a CSS stylesheet AST.
(define (read-css-rules source)
  (parse-css source))

;; extract-top-level-imports/rules : css-stylesheet? -> (listof string?)
;;   Preserve top-level @import rules from a parsed stylesheet for re-emission.
(define (extract-top-level-imports/rules stylesheet)
  (for/list ([rule (in-list (css-stylesheet-rules stylesheet))]
             #:when (and (css-at-rule? rule)
                         (string-ci=? (css-at-rule-name rule) "@import")
                         (not (css-at-rule-block rule))))
    (string-append (css-at-rule-name rule)
                   " "
                   (css-at-rule-prelude rule)
                   ";")))

;; extract-top-level-imports : string? -> (listof string?)
;;   Parse source and preserve top-level @import rules for re-emission.
(define (extract-top-level-imports source)
  (extract-top-level-imports/rules (read-css-rules source)))

;; collect-custom-properties : css-stylesheet? string? -> (hash/c string? string?)
;;   Collect custom properties for selector from matching rules, later declarations win.
(define (collect-custom-properties rules selector)
  (css-compute-custom-properties-for-selector-group
   rules
   selector
   #:resolve-vars? #t
   #:defaults bootstrap-defaults))

;; validate-bootstrap-selector! : string? -> void?
;;   Check that selector is one of the supported Bootstrap theme roots.
(define (validate-bootstrap-selector! selector)
  (unless (regexp-match? selector-regexp selector)
    (fail "unsupported selector ~s; use :root, [data-bs-theme=light], or [data-bs-theme=dark]"
          selector)))

;; extract-bootstrap-theme : string? string? -> (hash/c string? string?)
;;   Parse source and return matching Bootstrap custom properties for selector.
(define (extract-bootstrap-theme source selector)
  (extract-bootstrap-theme/rules (read-css-rules source) selector))

;; extract-bootstrap-theme/rules : css-stylesheet? string? -> (hash/c string? string?)
;;   Reuse a pre-parsed stylesheet and return matching Bootstrap custom properties for selector.
(define (extract-bootstrap-theme/rules rules selector)
  (validate-bootstrap-selector! selector)
  (define props
    (collect-custom-properties rules selector))
  (when (zero? (hash-count props))
    (fail "no custom properties found for selector ~s" selector))
  props)

;; resolve-css-value : string? (string? -> (or/c string? #f)) [listof string?] -> string?
;;   Resolve var(...) fragments within text using lookup.
(define (resolve-css-value text lookup [seen '()])
  (define m (regexp-match-positions css-var-regexp text))
  (if (not m)
      (string-trim text)
      (let* ([whole-pos (list-ref m 0)]
             [key-pos (list-ref m 1)]
             [fallback-pos (and (> (length m) 2) (list-ref m 2))]
             [before (substring text 0 (car whole-pos))]
             [after (substring text (cdr whole-pos))]
             [key (substring text (car key-pos) (cdr key-pos))]
             [fallback-text (and fallback-pos
                                 (substring text (car fallback-pos) (cdr fallback-pos)))]
             [match-text (substring text (car whole-pos) (cdr whole-pos))]
             [replacement
              (cond
                [(member key seen)
                 (or fallback-text match-text)]
                [else
                 (define resolved (lookup key))
                 (cond
                   [resolved
                    (resolve-css-value resolved lookup (cons key seen))]
                   [fallback-text
                    (resolve-css-value fallback-text lookup seen)]
                   [else
                    match-text])])])
        (define new-text (string-append before replacement after))
        (if (string=? new-text text)
            (string-trim text)
            (resolve-css-value new-text lookup seen)))))

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
  (resolve-css-value
   raw
   (lambda (lookup-key)
     (cond
       [(hash-has-key? props lookup-key)
        (resolve-prop-value props lookup-key #f (cons key seen))]
       [(hash-has-key? bootstrap-defaults lookup-key)
        (hash-ref bootstrap-defaults lookup-key)]
       [else
        #f]))
   (cons key seen)))

;; prop-ref* : (hash/c string? string?) string? [string?] -> string?
;;   Read one Bootstrap property with optional fallback default.
(define (prop-ref* props key [fallback-key #f])
  (or (hash-ref props key #f)
      (and fallback-key (hash-ref props fallback-key #f))))

;; selector-ref* : procedure? (listof string?) string? [string?] -> (or/c string? #f)
;;   Try ref against selectors in order and return the first concrete value.
(define (selector-ref* ref selectors name [fallback #f])
  (for/or ([selector (in-list selectors)])
    (ref selector name fallback)))

;; make-computed-selector-refs : (hash/c string? string?) (or/c css-stylesheet? #f) -> (values procedure? procedure?)
;;   Build cached exact-selector computed-style/custom-property lookup helpers.
(define (make-computed-selector-refs props rules)
  (cond
    [(not (css-stylesheet? rules))
     (values (lambda (_selector _name [fallback #f]) fallback)
             (lambda (_selector _name [fallback #f]) fallback))]
    [else
     (define defaults
       (for/fold ([env (hash-copy bootstrap-defaults)])
                 ([(key value) (in-hash props)])
         (hash-set! env key value)
         env))
     (define style-cache (make-hash))
     (define custom-cache (make-hash))
     (define (style-map selector)
       (hash-ref! style-cache selector
                  (lambda ()
                    (css-compute-style-for-selector-group
                     rules
                     selector
                     #:resolve-vars? #t
                     #:defaults defaults))))
     (define (custom-map selector)
       (hash-ref! custom-cache selector
                  (lambda ()
                    (css-compute-custom-properties-for-selector-group
                     rules
                     selector
                     #:resolve-vars? #t
                     #:defaults defaults))))
     (define (computed-style-ref selector name [fallback #f])
       (hash-ref (style-map selector) (string-downcase name) fallback))
     (define (computed-custom-ref selector name [fallback #f])
       (hash-ref (custom-map selector) name fallback))
     (values computed-style-ref computed-custom-ref)]))

;; meaningful-css-value? : (or/c string? #f) -> boolean?
;;   Check whether a CSS value carries concrete styling information.
(define (meaningful-css-value? value)
  (and value
       (not (member (string-downcase (string-trim value))
                    '("initial" "inherit" "unset" "revert" "revert-layer" "currentcolor")))))

;; extract-component-styles : (hash/c string? string?) css-stylesheet? -> (hash/c symbol? string?)
;;   Extract component-level Bootstrap styles that need more than root token mapping.
(define (extract-component-styles props rules)
  (define-values (nav-style-ref nav-custom-ref)
    (make-computed-selector-refs props rules))
  (define nav-link-color
    (selector-ref* nav-custom-ref
                   '(".navbar-nav" ".nav" ".nav-link")
                   "--bs-nav-link-color"))
  (define nav-link-hover
    (selector-ref* nav-custom-ref
                   '(".navbar-nav" ".nav" ".nav-link:hover")
                   "--bs-nav-link-hover-color"))
  (define nav-link-disabled
    (selector-ref* nav-custom-ref
                   '(".navbar-nav" ".nav" ".nav-link.disabled")
                   "--bs-nav-link-disabled-color"))
  (define nav-font-size
    (selector-ref* nav-style-ref
                   '(".navbar .nav-link"
                     ".nav-tabs .nav-link"
                     ".nav-pills .nav-link"
                     ".navbar"
                     ".nav-link")
                   "font-size"))
  (define nav-font-weight
    (or (selector-ref* nav-style-ref
                       '(".navbar .nav-link"
                         ".nav-tabs .nav-link"
                         ".nav-pills .nav-link"
                         ".navbar")
                       "font-weight")
        (nav-custom-ref ".nav" "--bs-nav-link-font-weight")
        (nav-custom-ref ".navbar-nav" "--bs-nav-link-font-weight")))
  (define nav-text-transform
    (selector-ref* nav-style-ref
                   '(".navbar .nav-link"
                     ".nav-tabs .nav-link"
                     ".nav-pills .nav-link"
                     ".navbar")
                   "text-transform"))
  (define nav-link-padding-y
    (or (selector-ref* nav-custom-ref
                       '(".navbar-nav" ".nav")
                       "--bs-nav-link-padding-y")
        (selector-ref* nav-style-ref
                       '(".navbar-nav .nav-link" ".nav-link")
                       "padding-top")))
  (define nav-link-padding-x
    (or (selector-ref* nav-custom-ref
                       '(".navbar" ".nav")
                       "--bs-navbar-nav-link-padding-x")
        (selector-ref* nav-custom-ref
                       '(".nav" ".navbar-nav")
                       "--bs-nav-link-padding-x")
        (selector-ref* nav-style-ref
                       '(".navbar-nav .nav-link" ".nav-link")
                       "padding-left")))
  (define navbar-brand-size
    (or (nav-custom-ref ".navbar" "--bs-navbar-brand-font-size")
        (nav-style-ref ".navbar-brand" "font-size")))
  (define navbar-brand-gap
    (or (nav-custom-ref ".navbar" "--bs-navbar-brand-margin-end")
        (nav-style-ref ".navbar-brand" "margin-right")))
  (define navbar-brand-color
    (or (nav-custom-ref ".navbar" "--bs-navbar-brand-color")
        (nav-style-ref ".navbar-brand" "color")))
  (define navbar-brand-hover
    (or (nav-custom-ref ".navbar" "--bs-navbar-brand-hover-color")
        (nav-style-ref ".navbar-brand:hover" "color")))
  (define navbar-link-color
    (or (nav-custom-ref ".navbar" "--bs-navbar-color")
        nav-link-color))
  (define navbar-link-hover
    (or (nav-custom-ref ".navbar" "--bs-navbar-hover-color")
        nav-link-hover))
  (define navbar-link-active
    (or (nav-custom-ref ".navbar" "--bs-navbar-active-color")
        (nav-style-ref ".navbar-nav .nav-link.active" "color")
        nav-link-hover))
  (define navbar-link-disabled
    (or (nav-custom-ref ".navbar" "--bs-navbar-disabled-color")
        nav-link-disabled))
  (define tab-border-color
    (or (nav-custom-ref ".nav-tabs" "--bs-nav-tabs-border-color")
        (nav-style-ref ".nav-tabs .nav-link" "border-color")))
  (define tab-border-radius
    (or (nav-custom-ref ".nav-tabs" "--bs-nav-tabs-border-radius")
        (nav-style-ref ".nav-tabs .nav-link" "border-radius")))
  (define tab-hover-border
    (or (nav-custom-ref ".nav-tabs" "--bs-nav-tabs-link-hover-border-color")
        (nav-style-ref ".nav-tabs .nav-link:hover" "border-color")))
  (define tab-bg
    (nav-style-ref ".nav-tabs .nav-link" "background-color"))
  (define tab-color
    (or (nav-style-ref ".nav-tabs .nav-link" "color")
        nav-link-color))
  (define tab-active-color
    (or (nav-custom-ref ".nav-tabs" "--bs-nav-tabs-link-active-color")
        (nav-style-ref ".nav-tabs .nav-link.active" "color")))
  (define tab-active-bg
    (or (nav-custom-ref ".nav-tabs" "--bs-nav-tabs-link-active-bg")
        (nav-style-ref ".nav-tabs .nav-link.active" "background-color")))
  (define tab-active-border
    (or (nav-custom-ref ".nav-tabs" "--bs-nav-tabs-link-active-border-color")
        (nav-style-ref ".nav-tabs .nav-link.active" "border-color")))
  (define pill-color
    (or (nav-style-ref ".nav-pills .nav-link" "color")
        nav-link-color))
  (define pill-border
    (nav-style-ref ".nav-pills .nav-link" "border-color"))
  (define pill-active-color
    (or (nav-custom-ref ".nav-pills" "--bs-nav-pills-link-active-color")
        (nav-style-ref ".nav-pills .nav-link.active" "color")))
  (define pill-active-bg
    (or (nav-custom-ref ".nav-pills" "--bs-nav-pills-link-active-bg")
        (nav-style-ref ".nav-pills .nav-link.active" "background-color")))
  (define pill-radius
    (or (nav-custom-ref ".nav-pills" "--bs-nav-pills-border-radius")
        (nav-style-ref ".nav-pills .nav-link" "border-radius")))
  (define-values (computed-style-ref computed-custom-ref)
    (make-computed-selector-refs props rules))
  (define dropdown-bg
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-bg")
        (computed-style-ref ".dropdown-menu" "background-color")))
  (define dropdown-color
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-color")
        (computed-style-ref ".dropdown-item" "color")))
  (define dropdown-link-color
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-link-color")
        (selector-ref* computed-style-ref
                       '(".dropdown-menu .dropdown-item" ".dropdown-item")
                       "color")
        dropdown-color))
  (define dropdown-border-color
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-border-color")
        (computed-style-ref ".dropdown-menu" "border-color")))
  (define dropdown-border-width
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-border-width")
        (computed-style-ref ".dropdown-menu" "border-width")))
  (define dropdown-radius
    (or (computed-style-ref ".dropdown-menu" "border-radius")
        (computed-custom-ref ".dropdown-menu" "--bs-dropdown-border-radius")))
  (define dropdown-shadow
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-box-shadow")
        (computed-style-ref ".dropdown-menu" "box-shadow")))
  (define dropdown-padding-y
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-padding-y")
        (computed-style-ref ".dropdown-menu" "padding-top")))
  (define dropdown-header-color
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-header-color")
        (computed-style-ref ".dropdown-header" "color")))
  (define dropdown-header-padding-x
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-header-padding-x")
        (computed-style-ref ".dropdown-header" "padding-left")))
  (define dropdown-header-padding-y
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-header-padding-y")
        (computed-style-ref ".dropdown-header" "padding-top")))
  (define dropdown-divider-bg
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-divider-bg")
        (computed-style-ref ".dropdown-divider" "border-top-color")))
  (define dropdown-item-padding-x
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-item-padding-x")
        (computed-style-ref ".dropdown-item" "padding-left")))
  (define dropdown-item-padding-y
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-item-padding-y")
        (computed-style-ref ".dropdown-item" "padding-top")))
  (define dropdown-item-font-size
    (selector-ref* computed-style-ref
                   '(".dropdown-menu .dropdown-item" ".dropdown-item")
                   "font-size"))
  (define dropdown-item-font-weight
    (selector-ref* computed-style-ref
                   '(".dropdown-menu .dropdown-item" ".dropdown-item")
                   "font-weight"))
  (define dropdown-item-text-transform
    (selector-ref* computed-style-ref
                   '(".dropdown-menu .dropdown-item" ".dropdown-item")
                   "text-transform"))
  (define dropdown-item-radius
    (computed-style-ref ".dropdown-item" "border-radius"))
  (define dropdown-item-hover-bg
    (selector-ref* computed-style-ref
                   '(".dropdown-item:hover" ".dropdown-item:focus")
                   "background-color"))
  (define dropdown-item-hover-color
    (selector-ref* computed-style-ref
                   '(".dropdown-item:hover" ".dropdown-item:focus")
                   "color"))
  (define dropdown-item-active-bg
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-link-active-bg")
        (selector-ref* computed-style-ref
                       '(".dropdown-item.active" ".dropdown-item:active")
                       "background-color")))
  (define dropdown-item-active-color
    (or (computed-custom-ref ".dropdown-menu" "--bs-dropdown-link-active-color")
        (selector-ref* computed-style-ref
                       '(".dropdown-item.active" ".dropdown-item:active")
                       "color")))
  (define tooltip-bg
    (or (computed-custom-ref ".tooltip" "--bs-tooltip-bg")
        (computed-style-ref ".tooltip-inner" "background-color")))
  (define tooltip-color
    (or (computed-custom-ref ".tooltip" "--bs-tooltip-color")
        (computed-style-ref ".tooltip-inner" "color")))
  (define tooltip-font-size
    (or (computed-custom-ref ".tooltip" "--bs-tooltip-font-size")
        (computed-style-ref ".tooltip" "font-size")
        (computed-style-ref ".tooltip-inner" "font-size")))
  (define tooltip-radius
    (or (computed-style-ref ".tooltip-inner" "border-radius")
        (computed-custom-ref ".tooltip" "--bs-tooltip-border-radius")))
  (define tooltip-opacity
    (or (computed-custom-ref ".tooltip" "--bs-tooltip-opacity")
        (computed-style-ref ".tooltip" "opacity")))
  (define tooltip-padding-x
    (or (computed-custom-ref ".tooltip" "--bs-tooltip-padding-x")
        (computed-style-ref ".tooltip-inner" "padding-left")))
  (define tooltip-padding-y
    (or (computed-custom-ref ".tooltip" "--bs-tooltip-padding-y")
        (computed-style-ref ".tooltip-inner" "padding-top")))
  (define popover-bg
    (or (computed-custom-ref ".popover" "--bs-popover-bg")
        (computed-style-ref ".popover" "background-color")))
  (define popover-max-width
    (or (computed-custom-ref ".popover" "--bs-popover-max-width")
        (computed-style-ref ".popover" "max-width")))
  (define popover-font-size
    (or (computed-custom-ref ".popover" "--bs-popover-font-size")
        (computed-style-ref ".popover" "font-size")))
  (define popover-color
    (or (computed-custom-ref ".popover" "--bs-popover-body-color")
        (computed-custom-ref ".popover" "--bs-popover-color")
        (computed-style-ref ".popover-body" "color")
        (computed-style-ref ".popover" "color")))
  (define popover-border-color
    (or (computed-custom-ref ".popover" "--bs-popover-border-color")
        (computed-style-ref ".popover" "border-color")))
  (define popover-border-width
    (or (computed-custom-ref ".popover" "--bs-popover-border-width")
        (computed-style-ref ".popover" "border-width")))
  (define popover-radius
    (or (computed-style-ref ".popover" "border-radius")
        (computed-custom-ref ".popover" "--bs-popover-border-radius")))
  (define popover-shadow
    (or (computed-custom-ref ".popover" "--bs-popover-box-shadow")
        (computed-style-ref ".popover" "box-shadow")))
  (define popover-arrow-width
    (or (computed-custom-ref ".popover" "--bs-popover-arrow-width")
        "1rem"))
  (define popover-arrow-height
    (or (computed-custom-ref ".popover" "--bs-popover-arrow-height")
        "0.5rem"))
  (define popover-header-bg
    (or (computed-custom-ref ".popover" "--bs-popover-header-bg")
        (computed-style-ref ".popover-header" "background-color")))
  (define popover-header-color
    (or (computed-custom-ref ".popover" "--bs-popover-header-color")
        (computed-style-ref ".popover-header" "color")))
  (define popover-header-padding-x
    (or (computed-custom-ref ".popover" "--bs-popover-header-padding-x")
        (computed-style-ref ".popover-header" "padding-left")))
  (define popover-header-padding-y
    (or (computed-custom-ref ".popover" "--bs-popover-header-padding-y")
        (computed-style-ref ".popover-header" "padding-top")))
  (define popover-body-padding-x
    (or (computed-custom-ref ".popover" "--bs-popover-body-padding-x")
        (computed-style-ref ".popover-body" "padding-left")))
  (define popover-body-padding-y
    (or (computed-custom-ref ".popover" "--bs-popover-body-padding-y")
        (computed-style-ref ".popover-body" "padding-top")))
  (define popover-title-font-size
    (or (computed-style-ref ".popover-header" "font-size")
        (computed-custom-ref ".popover-header" "--bs-popover-header-font-size")))
  (define popover-title-font-weight
    (or (computed-style-ref ".popover-header" "font-weight")
        (computed-style-ref "h3.popover-header" "font-weight")))
  (define-values (button-style-ref button-custom-ref)
    (make-computed-selector-refs props rules))
  (define btn-padding-x
    (or (button-custom-ref ".btn" "--bs-btn-padding-x")
        (button-style-ref ".btn" "padding-left")))
  (define btn-padding-y
    (or (button-custom-ref ".btn" "--bs-btn-padding-y")
        (button-style-ref ".btn" "padding-top")))
  (define btn-font-size
    (or (button-style-ref ".btn" "font-size")
        (button-custom-ref ".btn" "--bs-btn-font-size")))
  (define btn-font-weight
    (or (button-style-ref ".btn" "font-weight")
        (button-custom-ref ".btn" "--bs-btn-font-weight")))
  (define btn-line-height
    (or (button-style-ref ".btn" "line-height")
        (button-custom-ref ".btn" "--bs-btn-line-height")))
  (define btn-radius
    (or (button-style-ref ".btn" "border-radius")
        (button-style-ref "button" "border-radius")))
  (define btn-text-transform
    (or (button-style-ref ".btn" "text-transform")
        "none"))
  (define btn-secondary-bg
    (or (button-custom-ref ".btn-secondary" "--bs-btn-bg")
        (button-style-ref ".btn-secondary" "background-color")))
  (define btn-secondary-border
    (or (button-custom-ref ".btn-secondary" "--bs-btn-border-color")
        (button-style-ref ".btn-secondary" "border-color")))
  (define btn-secondary-color
    (or (button-custom-ref ".btn-secondary" "--bs-btn-color")
        (button-style-ref ".btn-secondary" "color")))
  (define btn-secondary-hover-bg
    (or (button-custom-ref ".btn-secondary" "--bs-btn-hover-bg")
        (button-style-ref ".btn-secondary:hover" "background-color")))
  (define btn-secondary-hover-border
    (or (button-custom-ref ".btn-secondary" "--bs-btn-hover-border-color")
        (button-style-ref ".btn-secondary:hover" "border-color")))
  (define btn-light-bg
    (or (button-custom-ref ".btn-light" "--bs-btn-bg")
        (button-style-ref ".btn-light" "background-color")))
  (define btn-light-border
    (or (button-custom-ref ".btn-light" "--bs-btn-border-color")
        (button-style-ref ".btn-light" "border-color")))
  (define btn-light-color
    (or (button-custom-ref ".btn-light" "--bs-btn-color")
        (button-style-ref ".btn-light" "color")))
  (define btn-light-hover-bg
    (or (button-custom-ref ".btn-light" "--bs-btn-hover-bg")
        (button-style-ref ".btn-light:hover" "background-color")))
  (define btn-light-hover-border
    (or (button-custom-ref ".btn-light" "--bs-btn-hover-border-color")
        (button-style-ref ".btn-light:hover" "border-color")))
  (define btn-outline-secondary-color
    (or (button-custom-ref ".btn-outline-secondary" "--bs-btn-color")
        (button-style-ref ".btn-outline-secondary" "color")))
  (define btn-outline-secondary-border
    (or (button-custom-ref ".btn-outline-secondary" "--bs-btn-border-color")
        (button-style-ref ".btn-outline-secondary" "border-color")))
  (define btn-outline-secondary-hover-bg
    (or (button-custom-ref ".btn-outline-secondary" "--bs-btn-hover-bg")
        (button-style-ref ".btn-outline-secondary:hover" "background-color")))
  (define btn-outline-secondary-hover-border
    (or (button-custom-ref ".btn-outline-secondary" "--bs-btn-hover-border-color")
        (button-style-ref ".btn-outline-secondary:hover" "border-color")))
  (define btn-outline-secondary-hover-color
    (or (button-custom-ref ".btn-outline-secondary" "--bs-btn-hover-color")
        (button-style-ref ".btn-outline-secondary:hover" "color")))
  (define btn-success-color
    (or (button-custom-ref ".btn-success" "--bs-btn-color")
        (button-style-ref ".btn-success" "color")))
  (define btn-info-color
    (or (button-custom-ref ".btn-info" "--bs-btn-color")
        (button-style-ref ".btn-info" "color")))
  (define btn-warning-color
    (or (button-custom-ref ".btn-warning" "--bs-btn-color")
        (button-style-ref ".btn-warning" "color")))
  (define btn-danger-color
    (or (button-custom-ref ".btn-danger" "--bs-btn-color")
        (button-style-ref ".btn-danger" "color")))
  (define-values (surface-style-ref surface-custom-ref)
    (make-computed-selector-refs props rules))
  (define card-bg
    (or (surface-custom-ref ".card" "--bs-card-bg")
        (surface-style-ref ".card" "background-color")))
  (define card-radius
    (surface-style-ref ".card" "border-radius"))
  (define card-shadow
    (or (surface-custom-ref ".card" "--bs-card-box-shadow")
        (surface-style-ref ".card" "box-shadow")))
  (define card-border-color
    (or (surface-custom-ref ".card" "--bs-card-border-color")
        (surface-style-ref ".card" "border-color")))
  (define card-cap-bg
    (or (surface-custom-ref ".card" "--bs-card-cap-bg")
        (surface-style-ref ".card-header" "background-color")))
  (define card-cap-color
    (or (surface-custom-ref ".card" "--bs-card-cap-color")
        (surface-style-ref ".card-header" "color")))
  (define card-cap-padding-x
    (or (surface-custom-ref ".card" "--bs-card-cap-padding-x")
        (surface-style-ref ".card-header" "padding-left")))
  (define card-cap-padding-y
    (or (surface-custom-ref ".card" "--bs-card-cap-padding-y")
        (surface-style-ref ".card-header" "padding-top")))
  (define card-body-padding-x
    (or (surface-custom-ref ".card" "--bs-card-spacer-x")
        (surface-style-ref ".card-body" "padding-left")))
  (define card-body-padding-y
    (or (surface-custom-ref ".card" "--bs-card-spacer-y")
        (surface-style-ref ".card-body" "padding-top")))
  (define badge-padding-x
    (or (surface-custom-ref ".badge" "--bs-badge-padding-x")
        (surface-style-ref ".badge" "padding-left")))
  (define badge-padding-y
    (or (surface-custom-ref ".badge" "--bs-badge-padding-y")
        (surface-style-ref ".badge" "padding-top")))
  (define badge-font-size
    (or (surface-custom-ref ".badge" "--bs-badge-font-size")
        (surface-style-ref ".badge" "font-size")))
  (define badge-font-weight
    (or (surface-custom-ref ".badge" "--bs-badge-font-weight")
        (surface-style-ref ".badge" "font-weight")))
  (define badge-radius
    (surface-style-ref ".badge" "border-radius"))
  (define badge-color
    (or (surface-custom-ref ".badge" "--bs-badge-color")
        (surface-style-ref ".badge" "color")))
  (define badge-secondary-color
    (or (surface-style-ref ".badge.bg-secondary, .badge.bg-light" "color")
        (surface-style-ref ".badge.bg-secondary" "color")))
  (define badge-light-color
    (or (surface-style-ref ".badge.bg-secondary, .badge.bg-light" "color")
        (surface-style-ref ".badge.bg-light" "color")))
  (define-values (navmetric-style-ref navmetric-custom-ref)
    (make-computed-selector-refs props rules))
  (define accordion-bg
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-bg")
        (navmetric-style-ref ".accordion-item" "background-color")))
  (define accordion-border-color
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-border-color")
        (navmetric-style-ref ".accordion-item" "border-color")))
  (define accordion-border-width
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-border-width")
        (navmetric-style-ref ".accordion-item" "border-width")))
  (define accordion-radius
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-border-radius")
        (navmetric-style-ref ".accordion-item" "border-radius")))
  (define accordion-trigger-radius-top
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-inner-border-radius")
        (navmetric-style-ref ".accordion-item:first-of-type > .accordion-header .accordion-button"
                             "border-top-left-radius")))
  (define accordion-trigger-radius-bottom
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-inner-border-radius")
        (navmetric-style-ref ".accordion-item:last-of-type > .accordion-header .accordion-button.collapsed"
                             "border-bottom-left-radius")))
  (define accordion-button-padding-x
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-btn-padding-x")
        (navmetric-style-ref ".accordion-button" "padding-left")))
  (define accordion-button-padding-y
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-btn-padding-y")
        (navmetric-style-ref ".accordion-button" "padding-top")))
  (define accordion-button-color
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-btn-color")
        (navmetric-style-ref ".accordion-button" "color")))
  (define accordion-button-bg
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-btn-bg")
        (navmetric-style-ref ".accordion-button" "background-color")))
  (define accordion-icon
    (navmetric-custom-ref ".accordion" "--bs-accordion-btn-icon"))
  (define accordion-active-icon
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-btn-active-icon")
        accordion-icon))
  (define accordion-icon-width
    (navmetric-custom-ref ".accordion" "--bs-accordion-btn-icon-width"))
  (define accordion-icon-transform
    (navmetric-custom-ref ".accordion" "--bs-accordion-btn-icon-transform"))
  (define accordion-active-color
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-active-color")
        (navmetric-style-ref ".accordion-button:not(.collapsed)" "color")))
  (define accordion-active-bg
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-active-bg")
        (navmetric-style-ref ".accordion-button:not(.collapsed)" "background-color")))
  (define accordion-body-padding-x
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-body-padding-x")
        (navmetric-style-ref ".accordion-body" "padding-left")))
  (define accordion-body-padding-y
    (or (navmetric-custom-ref ".accordion" "--bs-accordion-body-padding-y")
        (navmetric-style-ref ".accordion-body" "padding-top")))
  (define pagination-padding-x
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-padding-x")
        (navmetric-style-ref ".page-link" "padding-left")))
  (define pagination-padding-y
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-padding-y")
        (navmetric-style-ref ".page-link" "padding-top")))
  (define pagination-font-size
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-font-size")
        (navmetric-style-ref ".page-link" "font-size")))
  (define pagination-color
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-color")
        (navmetric-style-ref ".page-link" "color")))
  (define pagination-bg
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-bg")
        (navmetric-style-ref ".page-link" "background-color")))
  (define pagination-border-width
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-border-width")
        (navmetric-style-ref ".page-link" "border-width")))
  (define pagination-border-color
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-border-color")
        (navmetric-style-ref ".page-link" "border-color")))
  (define pagination-radius-direct
    (or (navmetric-style-ref ".pagination .page-link" "border-radius")
        (navmetric-style-ref ".page-link" "border-radius")))
  (define pagination-radius
    (and (meaningful-css-value? pagination-radius-direct)
         (not (regexp-match? css-var-regexp pagination-radius-direct))
         pagination-radius-direct))
  (define pagination-edge-radius
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-border-radius")
        (navmetric-style-ref ".page-item:first-child .page-link"
                             "border-top-left-radius")))
  (define pagination-hover-color
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-hover-color")
        (navmetric-style-ref ".page-link:hover" "color")))
  (define pagination-hover-bg
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-hover-bg")
        (navmetric-style-ref ".page-link:hover" "background-color")))
  (define pagination-hover-border-color
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-hover-border-color")
        (navmetric-style-ref ".page-link:hover" "border-color")))
  (define pagination-active-color
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-active-color")
        (navmetric-style-ref ".page-link.active" "color")
        (navmetric-style-ref ".active > .page-link" "color")))
  (define pagination-active-bg
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-active-bg")
        (navmetric-style-ref ".page-link.active" "background-color")
        (navmetric-style-ref ".active > .page-link" "background-color")))
  (define pagination-active-border-color
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-active-border-color")
        (navmetric-style-ref ".page-link.active" "border-color")
        (navmetric-style-ref ".active > .page-link" "border-color")))
  (define pagination-disabled-color
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-disabled-color")
        (navmetric-style-ref ".page-link.disabled" "color")
        (navmetric-style-ref ".disabled > .page-link" "color")))
  (define pagination-disabled-bg
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-disabled-bg")
        (navmetric-style-ref ".page-link.disabled" "background-color")
        (navmetric-style-ref ".disabled > .page-link" "background-color")))
  (define pagination-disabled-border-color
    (or (navmetric-custom-ref ".pagination" "--bs-pagination-disabled-border-color")
        (navmetric-style-ref ".page-link.disabled" "border-color")
        (navmetric-style-ref ".disabled > .page-link" "border-color")))
  (define progress-height
    (or (navmetric-custom-ref ".progress" "--bs-progress-height")
        (navmetric-custom-ref ".progress-stacked" "--bs-progress-height")
        (navmetric-style-ref ".progress" "height")
        (navmetric-style-ref "progress" "height")))
  (define progress-font-size
    (or (navmetric-custom-ref ".progress" "--bs-progress-font-size")
        (navmetric-custom-ref ".progress-stacked" "--bs-progress-font-size")
        (navmetric-style-ref ".progress" "font-size")))
  (define progress-bg
    (or (navmetric-custom-ref ".progress" "--bs-progress-bg")
        (navmetric-custom-ref ".progress-stacked" "--bs-progress-bg")
        (navmetric-style-ref ".progress" "background-color")
        (navmetric-style-ref "progress" "background-color")))
  (define progress-radius-direct
    (navmetric-style-ref ".progress" "border-radius"))
  (define progress-radius
    (and (meaningful-css-value? progress-radius-direct)
         (not (regexp-match? css-var-regexp progress-radius-direct))
         progress-radius-direct))
  (define progress-box-shadow-direct
    (navmetric-style-ref ".progress" "box-shadow"))
  (define progress-box-shadow
    (and (meaningful-css-value? progress-box-shadow-direct)
         (not (regexp-match? css-var-regexp progress-box-shadow-direct))
         progress-box-shadow-direct))
  (define progress-bar-color
    (or (navmetric-custom-ref ".progress" "--bs-progress-bar-color")
        (navmetric-custom-ref ".progress-stacked" "--bs-progress-bar-color")
        (navmetric-style-ref ".progress-bar" "color")))
  (define progress-bar-bg
    (or (navmetric-custom-ref ".progress" "--bs-progress-bar-bg")
        (navmetric-custom-ref ".progress-stacked" "--bs-progress-bar-bg")
        (navmetric-style-ref ".progress-bar" "background-color")))
  (define breadcrumb-divider-color
    (or (navmetric-custom-ref ".breadcrumb" "--bs-breadcrumb-divider-color")
        (navmetric-style-ref ".breadcrumb-item + .breadcrumb-item::before" "color")))
  (define breadcrumb-item-padding-x
    (or (navmetric-custom-ref ".breadcrumb" "--bs-breadcrumb-item-padding-x")
        (navmetric-style-ref ".breadcrumb-item + .breadcrumb-item" "padding-left")))
  (define breadcrumb-active-color
    (or (navmetric-custom-ref ".breadcrumb" "--bs-breadcrumb-item-active-color")
        (navmetric-style-ref ".breadcrumb-item.active" "color")))
  (define breadcrumb-link-color
    (or (navmetric-style-ref ".breadcrumb-item a" "color")
        (hash-ref props "--bs-link-color" #f)))
  (define-values (toast-style-ref toast-custom-ref)
    (make-computed-selector-refs props rules))
  (define toast-padding-x
    (or (toast-custom-ref ".toast" "--bs-toast-padding-x")
        (toast-style-ref ".toast-header" "padding-left")
        (toast-style-ref ".toast-body" "padding-left")))
  (define toast-padding-y
    (or (toast-custom-ref ".toast" "--bs-toast-padding-y")
        (toast-style-ref ".toast-header" "padding-top")))
  (define toast-font-size
    (or (toast-custom-ref ".toast" "--bs-toast-font-size")
        (toast-style-ref ".toast" "font-size")))
  (define toast-color
    (or (toast-custom-ref ".toast" "--bs-toast-color")
        (toast-style-ref ".toast" "color")
        (hash-ref props "--bs-body-color" #f)))
  (define toast-bg
    (or (toast-custom-ref ".toast" "--bs-toast-bg")
        (toast-style-ref ".toast" "background-color")))
  (define toast-border-width
    (or (toast-custom-ref ".toast" "--bs-toast-border-width")
        (toast-style-ref ".toast" "border-width")))
  (define toast-border-color
    (or (toast-custom-ref ".toast" "--bs-toast-border-color")
        (toast-style-ref ".toast" "border-color")))
  (define toast-radius-direct
    (toast-style-ref ".toast" "border-radius"))
  (define toast-radius
    (and (meaningful-css-value? toast-radius-direct)
         (not (regexp-match? css-var-regexp toast-radius-direct))
         toast-radius-direct))
  (define toast-shadow
    (or (toast-custom-ref ".toast" "--bs-toast-box-shadow")
        (toast-style-ref ".toast" "box-shadow")))
  (define toast-header-color
    (or (toast-custom-ref ".toast" "--bs-toast-header-color")
        (toast-style-ref ".toast-header" "color")))
  (define toast-header-bg
    (or (toast-custom-ref ".toast" "--bs-toast-header-bg")
        (toast-style-ref ".toast-header" "background-color")))
  (define toast-header-border-color
    (or (toast-custom-ref ".toast" "--bs-toast-header-border-color")
        (toast-style-ref ".toast-header" "border-bottom-color")))
  (define toast-body-padding
    (or (toast-style-ref ".toast-body" "padding")
        toast-padding-x))
  (define close-button-color
    (or (toast-custom-ref ".btn-close" "--bs-btn-close-color")
        (toast-style-ref ".btn-close" "color")))
  (define close-button-bg
    (or (toast-custom-ref ".btn-close" "--bs-btn-close-bg")
        (toast-style-ref ".btn-close" "background-image")))
  (define close-button-opacity
    (or (toast-custom-ref ".btn-close" "--bs-btn-close-opacity")
        (toast-style-ref ".btn-close" "opacity")))
  (define close-button-hover-opacity
    (or (toast-custom-ref ".btn-close" "--bs-btn-close-hover-opacity")
        (toast-style-ref ".btn-close:hover" "opacity")))
  (define close-button-radius
    (toast-style-ref ".btn-close" "border-radius"))
  (define modal-close-bg
    (or (toast-style-ref ".modal .btn-close" "background-image")
        (toast-style-ref ".modal-header .btn-close" "background-image")
        close-button-bg))
  (define-values (alert-style-ref alert-custom-ref)
    (make-computed-selector-refs props rules))
  (define alert-base-color-direct
    (alert-style-ref ".alert" "color"))
  (define alert-base-color-var
    (alert-custom-ref ".alert" "--bs-alert-color"))
  (define alert-base-color
    (or (and (meaningful-css-value? alert-base-color-direct)
             alert-base-color-direct)
        alert-base-color-var))
  (define (alert-tone-bg tone)
    (or (alert-style-ref (format ".alert-~a" tone) "background-color")
        (alert-custom-ref (format ".alert-~a" tone) "--bs-alert-bg")))
  (define (alert-tone-border tone)
    (or (alert-style-ref (format ".alert-~a" tone) "border-color")
        (alert-custom-ref (format ".alert-~a" tone) "--bs-alert-border-color")))
  (define (alert-tone-color tone)
    (define direct
      (alert-style-ref (format ".alert-~a" tone) "color"))
    (define via-var
      (alert-custom-ref (format ".alert-~a" tone) "--bs-alert-color"))
    (or (and (meaningful-css-value? direct) direct)
        alert-base-color
        via-var))
  (define alert-primary-bg (alert-tone-bg "primary"))
  (define alert-primary-border (alert-tone-border "primary"))
  (define alert-primary-color (alert-tone-color "primary"))
  (define alert-secondary-bg (alert-tone-bg "secondary"))
  (define alert-secondary-border (alert-tone-border "secondary"))
  (define alert-secondary-color (alert-tone-color "secondary"))
  (define alert-success-bg (alert-tone-bg "success"))
  (define alert-success-border (alert-tone-border "success"))
  (define alert-success-color (alert-tone-color "success"))
  (define alert-info-bg (alert-tone-bg "info"))
  (define alert-info-border (alert-tone-border "info"))
  (define alert-info-color (alert-tone-color "info"))
  (define alert-warning-bg (alert-tone-bg "warning"))
  (define alert-warning-border (alert-tone-border "warning"))
  (define alert-warning-color (alert-tone-color "warning"))
  (define alert-danger-bg (alert-tone-bg "danger"))
  (define alert-danger-border (alert-tone-border "danger"))
  (define alert-danger-color (alert-tone-color "danger"))
  (define alert-light-bg (alert-tone-bg "light"))
  (define alert-light-border (alert-tone-border "light"))
  (define alert-light-color (alert-tone-color "light"))
  (define alert-dark-bg (alert-tone-bg "dark"))
  (define alert-dark-border (alert-tone-border "dark"))
  (define alert-dark-color (alert-tone-color "dark"))
  (define-values (list-table-style-ref list-table-custom-ref)
    (make-computed-selector-refs props rules))
  (define list-group-bg
    (or (list-table-custom-ref ".list-group" "--bs-list-group-bg")
        (list-table-style-ref ".list-group-item" "background-color")))
  (define list-group-border-color
    (or (list-table-custom-ref ".list-group" "--bs-list-group-border-color")
        (list-table-style-ref ".list-group-item" "border-color")))
  (define list-group-border-width
    (list-table-style-ref ".list-group" "border-width"))
  (define list-group-radius
    (list-table-style-ref ".list-group" "border-radius"))
  (define list-group-item-radius
    (list-table-style-ref ".list-group-item" "border-radius"))
  (define list-group-padding-x
    (or (list-table-custom-ref ".list-group" "--bs-list-group-item-padding-x")
        (list-table-style-ref ".list-group-item" "padding-left")))
  (define list-group-padding-y
    (or (list-table-custom-ref ".list-group" "--bs-list-group-item-padding-y")
        (list-table-style-ref ".list-group-item" "padding-top")))
  (define list-group-color
    (or (list-table-custom-ref ".list-group" "--bs-list-group-color")
        (list-table-style-ref ".list-group-item" "color")))
  (define list-group-active-bg
    (or (list-table-custom-ref ".list-group" "--bs-list-group-active-bg")
        (list-table-style-ref ".list-group-item.active" "background-color")))
  (define list-group-active-color
    (or (list-table-custom-ref ".list-group" "--bs-list-group-active-color")
        (list-table-style-ref ".list-group-item.active" "color")))
  (define list-group-active-border
    (or (list-table-custom-ref ".list-group" "--bs-list-group-active-border-color")
        (list-table-style-ref ".list-group-item.active" "border-color")))
  (define table-bg
    (or (list-table-custom-ref ".table" "--bs-table-bg")
        (list-table-style-ref ".table" "background-color")))
  (define table-border-color
    (or (list-table-custom-ref ".table" "--bs-table-border-color")
        (list-table-style-ref ".table" "border-color")))
  (define table-color
    (or (list-table-custom-ref ".table" "--bs-table-color")
        (list-table-style-ref ".table" "color")))
  (define table-striped-bg
    (or (list-table-custom-ref ".table" "--bs-table-striped-bg")
        (list-table-style-ref ".table-striped > tbody > tr:nth-of-type(odd) > *" "background-color")))
  (define table-hover-bg
    (or (list-table-custom-ref ".table" "--bs-table-hover-bg")
        (list-table-style-ref ".table-hover > tbody > tr:hover > *" "background-color")))
  (define table-active-bg
    (or (list-table-custom-ref ".table" "--bs-table-active-bg")
        (list-table-style-ref ".table-active" "background-color")))
  (define table-cell-padding-x
    (or (list-table-custom-ref ".table" "--bs-table-cell-padding-x")
        (list-table-style-ref ".table th" "padding-left")
        (list-table-style-ref ".table td" "padding-left")))
  (define table-cell-padding-y
    (or (list-table-custom-ref ".table" "--bs-table-cell-padding-y")
        (list-table-style-ref ".table th" "padding-top")
        (list-table-style-ref ".table td" "padding-top")))
  (define table-border-width
    (or (list-table-style-ref ".table > :not(caption) > * > *" "border-bottom-width")
        (list-table-style-ref ".table td" "border-bottom-width")
        (list-table-style-ref ".table th" "border-bottom-width")))
  (define table-header-color
    (let ([header-color (list-table-style-ref ".table th" "color")])
      (if (meaningful-css-value? header-color)
          header-color
          table-color)))
  (define table-header-font-size
    (or (list-table-style-ref "th" "font-size")
        (list-table-style-ref ".table th" "font-size")))
  (define table-header-font-weight
    (or (list-table-style-ref ".table th" "font-weight")
        (list-table-style-ref "th" "font-weight")))
  (define table-header-text-transform
    (or (list-table-style-ref "th" "text-transform")
        (list-table-style-ref ".table th" "text-transform")))
  (define table-body-font-weight
    (or (list-table-style-ref ".table td" "font-weight")
        (hash-ref props "--bs-body-font-weight" #f)
        (list-table-style-ref "body" "font-weight")))
  (define-values (field-style-ref field-custom-ref)
    (make-computed-selector-refs props rules))
  (define field-bg
    (selector-ref* field-style-ref
                   '(".form-control" ".form-select")
                   "background-color"))
  (define field-color
    (selector-ref* field-style-ref
                   '(".form-control" ".form-select")
                   "color"))
  (define field-border-color
    (selector-ref* field-style-ref
                   '(".form-control" ".form-select")
                   "border-color"))
  (define field-border-width
    (selector-ref* field-style-ref
                   '(".form-control" ".form-select")
                   "border-width"))
  (define field-radius
    (selector-ref* field-style-ref
                   '(".form-control" ".form-select")
                   "border-radius"))
  (define field-padding-x
    (selector-ref* field-style-ref
                   '(".form-control" ".form-select")
                   "padding-left"))
  (define field-padding-y
    (selector-ref* field-style-ref
                   '(".form-control" ".form-select")
                   "padding-top"))
  (define field-font-size
    (selector-ref* field-style-ref
                   '(".form-control" ".form-select")
                   "font-size"))
  (define field-font-weight
    (or (field-style-ref ".form-control" "font-weight")
        (field-style-ref ".form-select" "font-weight")
        (hash-ref props "--bs-body-font-weight" #f)))
  (define field-select-icon
    (or (field-custom-ref ".form-select" "--bs-form-select-bg-img")
        (field-style-ref ".form-select" "background-image")))
  (define field-select-padding-right
    (field-style-ref ".form-select" "padding-right"))
  (define field-focus-bg
    (or (field-style-ref ".form-control:focus" "background-color")
        (field-style-ref ".form-select:focus" "background-color")))
  (define field-focus-border-color
    (or (field-style-ref ".form-control:focus" "border-color")
        (field-style-ref ".form-select:focus" "border-color")))
  (define input-group-addon-bg
    (field-style-ref ".input-group-text" "background-color"))
  (define input-group-addon-color
    (field-style-ref ".input-group-text" "color"))
  (define input-group-addon-border-color
    (field-style-ref ".input-group-text" "border-color"))
  (define input-group-addon-padding-x
    (field-style-ref ".input-group-text" "padding-left"))
  (define input-group-addon-padding-y
    (field-style-ref ".input-group-text" "padding-top"))
  (define input-group-addon-font-size
    (field-style-ref ".input-group-text" "font-size"))
  (define input-group-addon-font-weight
    (field-style-ref ".input-group-text" "font-weight"))
  (define field-valid-color
    (or (hash-ref props "--bs-form-valid-color" #f)
        (field-style-ref ".valid-feedback" "color")))
  (define field-valid-border-color
    (or (hash-ref props "--bs-form-valid-border-color" #f)
        (field-style-ref ".form-control.is-valid" "border-color")))
  (define field-valid-icon
    (field-style-ref ".form-control.is-valid" "background-image"))
  (define field-invalid-color
    (or (hash-ref props "--bs-form-invalid-color" #f)
        (field-style-ref ".invalid-feedback" "color")))
  (define field-invalid-border-color
    (or (hash-ref props "--bs-form-invalid-border-color" #f)
        (field-style-ref ".form-control.is-invalid" "border-color")))
  (define field-invalid-icon
    (field-style-ref ".form-control.is-invalid" "background-image"))
  (define-values (check-style-ref check-custom-ref)
    (make-computed-selector-refs props rules))
  (define check-bg
    (or (check-custom-ref ".form-check-input" "--bs-form-check-bg")
        (check-style-ref ".form-check-input" "background-color")))
  (define check-border-color
    (check-style-ref ".form-check-input" "border-color"))
  (define check-border-width
    (check-style-ref ".form-check-input" "border-width"))
  (define check-size
    (check-style-ref ".form-check-input" "width"))
  (define check-height
    (check-style-ref ".form-check-input" "height"))
  (define check-radius
    (or (check-style-ref ".form-check-input[type=checkbox]" "border-radius")
        (check-style-ref ".form-check-input" "border-radius")))
  (define check-focus-border-color
    (check-style-ref ".form-check-input:focus" "border-color"))
  (define check-focus-shadow
    (check-style-ref ".form-check-input:focus" "box-shadow"))
  (define check-checked-bg
    (check-style-ref ".form-check-input:checked" "background-color"))
  (define check-checked-border-color
    (check-style-ref ".form-check-input:checked" "border-color"))
  (define check-checked-icon
    (or (check-custom-ref ".form-check-input:checked[type=checkbox]" "--bs-form-check-bg-image")
        (check-style-ref ".form-check-input:checked[type=checkbox]" "background-image")))
  (define check-disabled-opacity
    (check-style-ref ".form-check-input:disabled" "opacity"))
  (define switch-width
    (check-style-ref ".form-switch .form-check-input" "width"))
  (define switch-radius
    (check-style-ref ".form-switch .form-check-input" "border-radius"))
  (define switch-bg-icon
    (or (check-custom-ref ".form-switch .form-check-input" "--bs-form-switch-bg")
        (check-style-ref ".form-switch .form-check-input" "background-image")))
  (define switch-focus-icon
    (or (check-custom-ref ".form-switch .form-check-input:focus" "--bs-form-switch-bg")
        (check-style-ref ".form-switch .form-check-input:focus" "background-image")))
  (define switch-checked-icon
    (or (check-custom-ref ".form-switch .form-check-input:checked" "--bs-form-switch-bg")
        (check-style-ref ".form-switch .form-check-input:checked" "background-image")))
  (define-values (dialog-style-ref dialog-custom-ref)
    (make-computed-selector-refs props rules))
  (define modal-bg
    (or (selector-ref* dialog-style-ref
                       '(".modal-content" ".modal")
                       "background-color")
        (dialog-custom-ref ".modal-content" "--bs-modal-bg")
        (dialog-custom-ref ".modal" "--bs-modal-bg")))
  (define modal-color
    (or (selector-ref* dialog-style-ref
                       '(".modal-content" ".modal")
                       "color")
        (dialog-custom-ref ".modal-content" "--bs-modal-color")
        (dialog-custom-ref ".modal" "--bs-modal-color")))
  (define modal-border-color
    (or (selector-ref* dialog-style-ref
                       '(".modal-content" ".modal")
                       "border-color")
        (dialog-custom-ref ".modal-content" "--bs-modal-border-color")
        (dialog-custom-ref ".modal" "--bs-modal-border-color")))
  (define modal-border-width
    (or (selector-ref* dialog-style-ref
                       '(".modal-content" ".modal")
                       "border-width")
        (dialog-custom-ref ".modal-content" "--bs-modal-border-width")
        (dialog-custom-ref ".modal" "--bs-modal-border-width")))
  (define modal-radius
    (selector-ref* dialog-style-ref
                   '(".modal-content" ".modal")
                   "border-radius"))
  (define modal-shadow
    (selector-ref* dialog-style-ref
                   '(".modal-content" ".modal")
                   "box-shadow"))
  (define modal-header-padding-x
    (or (dialog-style-ref ".modal-header" "padding-left")
        (dialog-style-ref ".modal" "padding-left")
        (dialog-custom-ref ".modal-header" "--bs-modal-header-padding-x")
        (dialog-custom-ref ".modal" "--bs-modal-header-padding-x")))
  (define modal-header-padding-y
    (or (dialog-style-ref ".modal-header" "padding-top")
        (dialog-style-ref ".modal" "padding-top")
        (dialog-custom-ref ".modal-header" "--bs-modal-header-padding-y")
        (dialog-custom-ref ".modal" "--bs-modal-header-padding-y")))
  (define modal-header-border-color
    (or (dialog-style-ref ".modal-header" "border-bottom-color")
        (dialog-style-ref ".modal" "border-bottom-color")
        (dialog-custom-ref ".modal-header" "--bs-modal-header-border-color")
        (dialog-custom-ref ".modal" "--bs-modal-header-border-color")))
  (define modal-header-border-width
    (or (dialog-style-ref ".modal-header" "border-bottom-width")
        (dialog-style-ref ".modal" "border-bottom-width")
        (dialog-custom-ref ".modal-header" "--bs-modal-header-border-width")
        (dialog-custom-ref ".modal" "--bs-modal-header-border-width")))
  (define modal-body-padding-x
    (or (dialog-style-ref ".modal-body" "padding-left")
        (dialog-style-ref ".modal" "padding-left")))
  (define modal-body-padding-y
    (or (dialog-style-ref ".modal-body" "padding-top")
        (dialog-style-ref ".modal" "padding-top")))
  (define modal-footer-padding-x
    (or (dialog-style-ref ".modal-footer" "padding-left")
        (dialog-style-ref ".modal" "padding-left")))
  (define modal-footer-padding-y
    (or (dialog-style-ref ".modal-footer" "padding-top")
        (dialog-style-ref ".modal" "padding-top")))
  (define modal-footer-gap
    (or (dialog-custom-ref ".modal-footer" "--bs-modal-footer-gap")
        (dialog-custom-ref ".modal" "--bs-modal-footer-gap")
        (dialog-style-ref ".modal-footer > *" "margin")))
  (define modal-footer-border-color
    (or (dialog-style-ref ".modal-footer" "border-top-color")
        (dialog-style-ref ".modal" "border-top-color")
        (dialog-custom-ref ".modal-footer" "--bs-modal-footer-border-color")
        (dialog-custom-ref ".modal" "--bs-modal-footer-border-color")))
  (define modal-footer-border-width
    (or (dialog-style-ref ".modal-footer" "border-top-width")
        (dialog-style-ref ".modal" "border-top-width")
        (dialog-custom-ref ".modal-footer" "--bs-modal-footer-border-width")
        (dialog-custom-ref ".modal" "--bs-modal-footer-border-width")))
  (define modal-title-color
    (let ([title-color (dialog-style-ref ".modal-title" "color")])
      (and (meaningful-css-value? title-color) title-color)))
  (define modal-title-font-size
    (or (dialog-style-ref ".modal-title" "font-size")
        (dialog-style-ref "h5" "font-size")))
  (define modal-title-font-weight
    (or (dialog-style-ref ".modal-title" "font-weight")
        (dialog-style-ref "h5" "font-weight")))
  (define modal-title-text-transform
    (or (dialog-style-ref ".modal-title" "text-transform")
        (dialog-style-ref "h5" "text-transform")))
  (hash 'nav-link-color nav-link-color
        'nav-link-hover nav-link-hover
        'nav-link-disabled nav-link-disabled
        'nav-font-size nav-font-size
        'nav-font-weight nav-font-weight
        'nav-text-transform nav-text-transform
        'nav-link-padding-y nav-link-padding-y
        'nav-link-padding-x nav-link-padding-x
        'navbar-brand-size navbar-brand-size
        'navbar-brand-gap navbar-brand-gap
        'navbar-brand-color navbar-brand-color
        'navbar-brand-hover navbar-brand-hover
        'navbar-link-color navbar-link-color
        'navbar-link-hover navbar-link-hover
        'navbar-link-active navbar-link-active
        'navbar-link-disabled navbar-link-disabled
        'tab-border-color tab-border-color
        'tab-border-radius tab-border-radius
        'tab-hover-border tab-hover-border
        'tab-bg tab-bg
        'tab-color tab-color
        'tab-active-color tab-active-color
        'tab-active-bg tab-active-bg
        'tab-active-border tab-active-border
        'pill-color pill-color
        'pill-border pill-border
        'pill-active-color pill-active-color
        'pill-active-bg pill-active-bg
        'pill-radius pill-radius
        'dropdown-bg dropdown-bg
        'dropdown-color dropdown-color
        'dropdown-border-color dropdown-border-color
        'dropdown-border-width dropdown-border-width
        'dropdown-radius dropdown-radius
        'dropdown-shadow dropdown-shadow
        'dropdown-padding-y dropdown-padding-y
        'dropdown-header-color dropdown-header-color
        'dropdown-header-padding-x dropdown-header-padding-x
        'dropdown-header-padding-y dropdown-header-padding-y
        'dropdown-divider-bg dropdown-divider-bg
        'dropdown-link-color dropdown-link-color
        'dropdown-item-padding-x dropdown-item-padding-x
        'dropdown-item-padding-y dropdown-item-padding-y
        'dropdown-item-font-size dropdown-item-font-size
        'dropdown-item-font-weight dropdown-item-font-weight
        'dropdown-item-text-transform dropdown-item-text-transform
        'dropdown-item-radius dropdown-item-radius
        'dropdown-item-hover-bg dropdown-item-hover-bg
        'dropdown-item-hover-color dropdown-item-hover-color
        'dropdown-item-active-bg dropdown-item-active-bg
        'dropdown-item-active-color dropdown-item-active-color
        'tooltip-bg tooltip-bg
        'tooltip-color tooltip-color
        'tooltip-font-size tooltip-font-size
        'tooltip-radius tooltip-radius
        'tooltip-opacity tooltip-opacity
        'tooltip-padding-x tooltip-padding-x
        'tooltip-padding-y tooltip-padding-y
        'popover-bg popover-bg
        'popover-max-width popover-max-width
        'popover-font-size popover-font-size
        'popover-color popover-color
        'popover-border-color popover-border-color
        'popover-border-width popover-border-width
        'popover-radius popover-radius
        'popover-shadow popover-shadow
        'popover-arrow-width popover-arrow-width
        'popover-arrow-height popover-arrow-height
        'popover-header-bg popover-header-bg
        'popover-header-color popover-header-color
        'popover-header-padding-x popover-header-padding-x
        'popover-header-padding-y popover-header-padding-y
        'popover-body-padding-x popover-body-padding-x
        'popover-body-padding-y popover-body-padding-y
        'popover-title-font-size popover-title-font-size
        'popover-title-font-weight popover-title-font-weight
        'btn-padding-x btn-padding-x
        'btn-padding-y btn-padding-y
        'btn-font-size btn-font-size
        'btn-font-weight btn-font-weight
        'btn-line-height btn-line-height
        'btn-radius btn-radius
        'btn-text-transform btn-text-transform
        'btn-secondary-bg btn-secondary-bg
        'btn-secondary-border btn-secondary-border
        'btn-secondary-color btn-secondary-color
        'btn-secondary-hover-bg btn-secondary-hover-bg
        'btn-secondary-hover-border btn-secondary-hover-border
        'btn-light-bg btn-light-bg
        'btn-light-border btn-light-border
        'btn-light-color btn-light-color
        'btn-light-hover-bg btn-light-hover-bg
        'btn-light-hover-border btn-light-hover-border
        'btn-outline-secondary-color btn-outline-secondary-color
        'btn-outline-secondary-border btn-outline-secondary-border
        'btn-outline-secondary-hover-bg btn-outline-secondary-hover-bg
        'btn-outline-secondary-hover-border btn-outline-secondary-hover-border
        'btn-outline-secondary-hover-color btn-outline-secondary-hover-color
        'btn-success-color btn-success-color
        'btn-info-color btn-info-color
        'btn-warning-color btn-warning-color
        'btn-danger-color btn-danger-color
        'card-bg card-bg
        'card-radius card-radius
        'card-shadow card-shadow
        'card-border-color card-border-color
        'card-cap-bg card-cap-bg
        'card-cap-color card-cap-color
        'card-cap-padding-x card-cap-padding-x
        'card-cap-padding-y card-cap-padding-y
        'card-body-padding-x card-body-padding-x
        'card-body-padding-y card-body-padding-y
        'badge-padding-x badge-padding-x
        'badge-padding-y badge-padding-y
        'badge-font-size badge-font-size
        'badge-font-weight badge-font-weight
        'badge-radius badge-radius
        'badge-color badge-color
        'badge-secondary-color badge-secondary-color
        'badge-light-color badge-light-color
        'accordion-bg accordion-bg
        'accordion-border-color accordion-border-color
        'accordion-border-width accordion-border-width
        'accordion-radius accordion-radius
        'accordion-trigger-radius-top accordion-trigger-radius-top
        'accordion-trigger-radius-bottom accordion-trigger-radius-bottom
        'accordion-button-padding-x accordion-button-padding-x
        'accordion-button-padding-y accordion-button-padding-y
        'accordion-button-color accordion-button-color
        'accordion-button-bg accordion-button-bg
        'accordion-icon accordion-icon
        'accordion-active-icon accordion-active-icon
        'accordion-icon-width accordion-icon-width
        'accordion-icon-transform accordion-icon-transform
        'accordion-active-color accordion-active-color
        'accordion-active-bg accordion-active-bg
        'accordion-body-padding-x accordion-body-padding-x
        'accordion-body-padding-y accordion-body-padding-y
        'pagination-padding-x pagination-padding-x
        'pagination-padding-y pagination-padding-y
        'pagination-font-size pagination-font-size
        'pagination-color pagination-color
        'pagination-bg pagination-bg
        'pagination-border-width pagination-border-width
        'pagination-border-color pagination-border-color
        'pagination-radius pagination-radius
        'pagination-edge-radius pagination-edge-radius
        'pagination-hover-color pagination-hover-color
        'pagination-hover-bg pagination-hover-bg
        'pagination-hover-border-color pagination-hover-border-color
        'pagination-active-color pagination-active-color
        'pagination-active-bg pagination-active-bg
        'pagination-active-border-color pagination-active-border-color
        'pagination-disabled-color pagination-disabled-color
        'pagination-disabled-bg pagination-disabled-bg
        'pagination-disabled-border-color pagination-disabled-border-color
        'progress-height progress-height
        'progress-font-size progress-font-size
        'progress-bg progress-bg
        'progress-radius progress-radius
        'progress-box-shadow progress-box-shadow
        'progress-bar-color progress-bar-color
        'progress-bar-bg progress-bar-bg
        'breadcrumb-divider-color breadcrumb-divider-color
        'breadcrumb-item-padding-x breadcrumb-item-padding-x
        'breadcrumb-active-color breadcrumb-active-color
        'breadcrumb-link-color breadcrumb-link-color
        'toast-padding-x toast-padding-x
        'toast-padding-y toast-padding-y
        'toast-font-size toast-font-size
        'toast-color toast-color
        'toast-bg toast-bg
        'toast-border-width toast-border-width
        'toast-border-color toast-border-color
        'toast-radius toast-radius
        'toast-shadow toast-shadow
        'toast-header-color toast-header-color
        'toast-header-bg toast-header-bg
        'toast-header-border-color toast-header-border-color
        'toast-body-padding toast-body-padding
        'close-button-color close-button-color
        'close-button-bg close-button-bg
        'close-button-opacity close-button-opacity
        'close-button-hover-opacity close-button-hover-opacity
        'close-button-radius close-button-radius
        'modal-close-bg modal-close-bg
        'alert-primary-bg alert-primary-bg
        'alert-primary-border alert-primary-border
        'alert-primary-color alert-primary-color
        'alert-secondary-bg alert-secondary-bg
        'alert-secondary-border alert-secondary-border
        'alert-secondary-color alert-secondary-color
        'alert-success-bg alert-success-bg
        'alert-success-border alert-success-border
        'alert-success-color alert-success-color
        'alert-info-bg alert-info-bg
        'alert-info-border alert-info-border
        'alert-info-color alert-info-color
        'alert-warning-bg alert-warning-bg
        'alert-warning-border alert-warning-border
        'alert-warning-color alert-warning-color
        'alert-danger-bg alert-danger-bg
        'alert-danger-border alert-danger-border
        'alert-danger-color alert-danger-color
        'alert-light-bg alert-light-bg
        'alert-light-border alert-light-border
        'alert-light-color alert-light-color
        'alert-dark-bg alert-dark-bg
        'alert-dark-border alert-dark-border
        'alert-dark-color alert-dark-color
        'list-group-bg list-group-bg
        'list-group-border-color list-group-border-color
        'list-group-border-width list-group-border-width
        'list-group-radius list-group-radius
        'list-group-item-radius list-group-item-radius
        'list-group-padding-x list-group-padding-x
        'list-group-padding-y list-group-padding-y
        'list-group-color list-group-color
        'list-group-active-bg list-group-active-bg
        'list-group-active-color list-group-active-color
        'list-group-active-border list-group-active-border
        'table-bg table-bg
        'table-border-color table-border-color
        'table-color table-color
        'table-striped-bg table-striped-bg
        'table-hover-bg table-hover-bg
        'table-active-bg table-active-bg
        'table-cell-padding-x table-cell-padding-x
        'table-cell-padding-y table-cell-padding-y
        'table-border-width table-border-width
        'table-header-color table-header-color
        'table-header-font-size table-header-font-size
        'table-header-font-weight table-header-font-weight
        'table-header-text-transform table-header-text-transform
        'table-body-font-weight table-body-font-weight
        'field-bg field-bg
        'field-color field-color
        'field-border-color field-border-color
        'field-border-width field-border-width
        'field-radius field-radius
        'field-padding-x field-padding-x
        'field-padding-y field-padding-y
        'field-font-size field-font-size
        'field-font-weight field-font-weight
        'field-select-icon field-select-icon
        'field-select-padding-right field-select-padding-right
        'field-focus-bg field-focus-bg
        'field-focus-border-color field-focus-border-color
        'input-group-addon-bg input-group-addon-bg
        'input-group-addon-color input-group-addon-color
        'input-group-addon-border-color input-group-addon-border-color
        'input-group-addon-padding-x input-group-addon-padding-x
        'input-group-addon-padding-y input-group-addon-padding-y
        'input-group-addon-font-size input-group-addon-font-size
        'input-group-addon-font-weight input-group-addon-font-weight
        'field-valid-color field-valid-color
        'field-valid-border-color field-valid-border-color
        'field-valid-icon field-valid-icon
        'field-invalid-color field-invalid-color
        'field-invalid-border-color field-invalid-border-color
        'field-invalid-icon field-invalid-icon
        'check-bg check-bg
        'check-border-color check-border-color
        'check-border-width check-border-width
        'check-size check-size
        'check-height check-height
        'check-radius check-radius
        'check-focus-border-color check-focus-border-color
        'check-focus-shadow check-focus-shadow
        'check-checked-bg check-checked-bg
        'check-checked-border-color check-checked-border-color
        'check-checked-icon check-checked-icon
        'check-disabled-opacity check-disabled-opacity
        'switch-width switch-width
        'switch-radius switch-radius
        'switch-bg-icon switch-bg-icon
        'switch-focus-icon switch-focus-icon
        'switch-checked-icon switch-checked-icon
        'modal-bg modal-bg
        'modal-color modal-color
        'modal-border-color modal-border-color
        'modal-border-width modal-border-width
        'modal-radius modal-radius
        'modal-shadow modal-shadow
        'modal-header-padding-x modal-header-padding-x
        'modal-header-padding-y modal-header-padding-y
        'modal-header-border-color modal-header-border-color
        'modal-header-border-width modal-header-border-width
        'modal-body-padding-x modal-body-padding-x
        'modal-body-padding-y modal-body-padding-y
        'modal-footer-padding-x modal-footer-padding-x
        'modal-footer-padding-y modal-footer-padding-y
        'modal-footer-gap modal-footer-gap
        'modal-footer-border-color modal-footer-border-color
        'modal-footer-border-width modal-footer-border-width
        'modal-title-color modal-title-color
        'modal-title-font-size modal-title-font-size
        'modal-title-font-weight modal-title-font-weight
        'modal-title-text-transform modal-title-text-transform))

;; bootstrap-props->theme-model : (hash/c string? string?) [(or/c css-stylesheet? #f)] -> bootstrap-theme?
;;   Normalize Bootstrap custom properties into a small semantic theme model.
(define (bootstrap-props->theme-model props [rules #f])
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
   (or (hash-ref props "--bs-body-font-weight" #f)
       (and rules
            (hash-ref (css-compute-style-for-selector-group rules
                                                            "body"
                                                            #:resolve-vars? #t
                                                            #:defaults props)
                      "font-weight"
                      #f))
       "400")
   "600"
   "700"
   (prop-ref* props "--bs-border-radius-sm")
   (prop-ref* props "--bs-border-radius")
   (prop-ref* props "--bs-border-radius-lg")
   (prop-ref* props "--bs-border-radius-pill")
   (prop-ref* props "--bs-box-shadow-sm")
   (prop-ref* props "--bs-box-shadow")
   (prop-ref* props "--bs-box-shadow-lg")
   props
   (extract-component-styles props rules)))

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

;; bootstrap-rules->theme-model : css-stylesheet? [string?] -> bootstrap-theme?
;;   Build a normalized theme model from a parsed Bootstrap stylesheet.
(define (bootstrap-rules->theme-model rules [selector ":root"])
  (bootstrap-props->theme-model
   (extract-bootstrap-theme/rules rules selector)
   rules))

;; theme->token-map : bootstrap-theme? [boolean?] -> (hash/c string? string?)
;;   Convert generated token lines into a lookup hash for tests and probes.
(define (theme->token-map theme [boost? #f])
  (for/hash ([entry (in-list (build-token-lines theme boost?))])
    (values (car entry) (cdr entry))))

;; bootstrap-rules->token-map : css-stylesheet? [string?] [boolean?] -> (hash/c string? string?)
;;   Build a generated token lookup hash directly from a parsed stylesheet.
(define (bootstrap-rules->token-map rules [selector ":root"] [boost? #f])
  (theme->token-map (bootstrap-rules->theme-model rules selector) boost?))

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

;; theme-component-ref : bootstrap-theme? symbol? [string?] -> (or/c string? #f)
;;   Read one extracted component style with optional fallback.
(define (theme-component-ref theme key [fallback #f])
  (define value
    (hash-ref (bootstrap-theme-component-styles theme) key (lambda () fallback)))
  (if (and value
           (regexp-match? #px"var\\(\\s*--bs-" value))
      fallback
      value))

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
  (define navbar-bg
    (or (theme-prop-ref theme "--bs-tertiary-bg")
        (hash-ref surface-map 'surface-subtle)))
  (define navbar-border
    (or (theme-prop-ref theme "--bs-border-color")
        control-border))
  (define navbar-link-color
    (or (theme-component-ref theme 'navbar-link-color)
        (theme-component-ref theme 'nav-link-color)
        fg-muted))
  (define navbar-link-hover
    (or (theme-component-ref theme 'navbar-link-hover)
        (theme-component-ref theme 'nav-link-hover)
        (rgb->css emphasis-fg)))
  (define navbar-link-active
    (or (theme-component-ref theme 'navbar-link-active)
        (rgb->css emphasis-fg)))
  (define navbar-link-disabled
    (or (theme-component-ref theme 'navbar-link-disabled)
        (theme-component-ref theme 'nav-link-disabled)
        fg-muted))
  (define navbar-brand-color
    (or (theme-component-ref theme 'navbar-brand-color)
        navbar-link-active))
  (define navbar-brand-hover
    (or (theme-component-ref theme 'navbar-brand-hover)
        navbar-brand-color))
  (define navbar-brand-size
    (or (theme-component-ref theme 'navbar-brand-size)
        (bootstrap-theme-font-size-lg theme)))
  (define navbar-brand-gap
    (or (theme-component-ref theme 'navbar-brand-gap)
        "1rem"))
  (define nav-font-size
    (or (theme-component-ref theme 'nav-font-size)
        (bootstrap-theme-font-size-md theme)))
  (define nav-font-weight
    (or (theme-component-ref theme 'nav-font-weight)
        (bootstrap-theme-font-weight-semibold theme)))
  (define nav-text-transform
    (or (theme-component-ref theme 'nav-text-transform)
        "none"))
  (define nav-link-padding-x
    (or (theme-component-ref theme 'nav-link-padding-x)
        "0.75rem"))
  (define nav-link-padding-y
    (or (theme-component-ref theme 'nav-link-padding-y)
        "0.5rem"))
  (define tab-bg
    (or (theme-component-ref theme 'tab-bg)
        (hash-ref surface-map 'surface-muted)))
  (define tab-color
    (or (theme-component-ref theme 'tab-color)
        navbar-link-color))
  (define tab-border-color
    (or (theme-component-ref theme 'tab-border-color)
        control-border))
  (define tab-border-radius
    (or (theme-component-ref theme 'tab-border-radius)
        (bootstrap-theme-radius-lg theme)))
  (define tab-hover-border
    (or (theme-component-ref theme 'tab-hover-border)
        tab-border-color))
  (define tab-active-bg
    (or (theme-component-ref theme 'tab-active-bg)
        bg-selected))
  (define tab-active-color
    (or (theme-component-ref theme 'tab-active-color)
        (rgb->css emphasis-fg)))
  (define tab-active-border
    (or (theme-component-ref theme 'tab-active-border)
        bg-selected))
  (define pill-color
    (or (theme-component-ref theme 'pill-color)
        navbar-link-color))
  (define pill-border
    (or (theme-component-ref theme 'pill-border)
        "transparent"))
  (define pill-active-color
    (or (theme-component-ref theme 'pill-active-color)
        (derive-on-color focus-base)))
  (define pill-active-bg
    (or (theme-component-ref theme 'pill-active-bg)
        (bootstrap-theme-primary theme)))
  (define pill-radius
    (or (theme-component-ref theme 'pill-radius)
        (bootstrap-theme-radius-pill theme)))
  (define menu-popup-bg
    (or (theme-component-ref theme 'dropdown-bg)
        popup-bg))
  (define menu-popup-border-color
    (or (theme-component-ref theme 'dropdown-border-color)
        popup-border))
  (define menu-popup-border-width
    (or (theme-component-ref theme 'dropdown-border-width)
        "1px"))
  (define menu-popup-radius
    (or (theme-component-ref theme 'dropdown-radius)
        (bootstrap-theme-radius-md theme)))
  (define menu-popup-shadow
    (or (theme-component-ref theme 'dropdown-shadow)
        "var(--we-shadow-md)"))
  (define menu-popup-padding-y
    (or (theme-component-ref theme 'dropdown-padding-y)
        "0.5rem"))
  (define menu-header-color
    (or (theme-component-ref theme 'dropdown-header-color)
        fg-muted))
  (define menu-header-padding-x
    (or (theme-component-ref theme 'dropdown-header-padding-x)
        "1rem"))
  (define menu-header-padding-y
    (or (theme-component-ref theme 'dropdown-header-padding-y)
        "0.5rem"))
  (define menu-divider-color
    (or (theme-component-ref theme 'dropdown-divider-bg)
        separator))
  (define menu-item-color
    (or (theme-component-ref theme 'dropdown-link-color)
        (theme-component-ref theme 'dropdown-color)
        (bootstrap-theme-fg theme)))
  (define menu-item-padding-x
    (or (theme-component-ref theme 'dropdown-item-padding-x)
        "1rem"))
  (define menu-item-padding-y
    (or (theme-component-ref theme 'dropdown-item-padding-y)
        "0.35rem"))
  (define menu-item-font-size
    (or (theme-component-ref theme 'dropdown-item-font-size)
        (bootstrap-theme-font-size-md theme)))
  (define menu-item-font-weight
    (or (theme-component-ref theme 'dropdown-item-font-weight)
        (bootstrap-theme-font-weight-normal theme)))
  (define menu-item-text-transform
    (or (theme-component-ref theme 'dropdown-item-text-transform)
        "none"))
  (define menu-item-radius
    (or (theme-component-ref theme 'dropdown-item-radius)
        "0"))
  (define menu-item-hover-bg
    (or (theme-component-ref theme 'dropdown-item-hover-bg)
        bg-hover))
  (define menu-item-hover-fg
    (or (theme-component-ref theme 'dropdown-item-hover-color)
        menu-item-color))
  (define menu-item-active-bg
    (or (theme-component-ref theme 'dropdown-item-active-bg)
        bg-selected))
  (define menu-item-active-fg
    (or (theme-component-ref theme 'dropdown-item-active-color)
        (rgb->css emphasis-fg)))
  (define tooltip-bg
    (or (theme-component-ref theme 'tooltip-bg)
        popup-bg))
  (define tooltip-color
    (or (theme-component-ref theme 'tooltip-color)
        (derive-on-color (or (parse-css-color tooltip-bg)
                             (rgb 26 31 43 1.0)))))
  (define tooltip-font-size
    (or (theme-component-ref theme 'tooltip-font-size)
        (bootstrap-theme-font-size-sm theme)))
  (define tooltip-radius
    (or (theme-component-ref theme 'tooltip-radius)
        (bootstrap-theme-radius-sm theme)))
  (define tooltip-opacity
    (or (theme-component-ref theme 'tooltip-opacity)
        "0.9"))
  (define tooltip-padding-x
    (or (theme-component-ref theme 'tooltip-padding-x)
        "0.5rem"))
  (define tooltip-padding-y
    (or (theme-component-ref theme 'tooltip-padding-y)
        "0.25rem"))
  (define tooltip-border-color
    (or (theme-component-ref theme 'tooltip-border-color)
        tooltip-bg))
  (define tooltip-shadow
    (or (theme-component-ref theme 'tooltip-shadow)
        "none"))
  (define tooltip-arrow-size "0.5rem")
  (define popover-bg
    (or (theme-component-ref theme 'popover-bg)
        popup-bg))
  (define popover-max-width
    (or (theme-component-ref theme 'popover-max-width)
        "18rem"))
  (define popover-font-size
    (or (theme-component-ref theme 'popover-font-size)
        (bootstrap-theme-font-size-sm theme)))
  (define popover-color
    (or (theme-component-ref theme 'popover-color)
        (bootstrap-theme-fg theme)))
  (define popover-border-color
    (or (theme-component-ref theme 'popover-border-color)
        popup-border))
  (define popover-border-width
    (or (theme-component-ref theme 'popover-border-width)
        "1px"))
  (define popover-radius
    (or (theme-component-ref theme 'popover-radius)
        (bootstrap-theme-radius-lg theme)))
  (define popover-shadow
    (or (theme-component-ref theme 'popover-shadow)
        "var(--we-shadow-md)"))
  (define popover-arrow-width
    (or (theme-component-ref theme 'popover-arrow-width)
        "1rem"))
  (define popover-arrow-height
    (or (theme-component-ref theme 'popover-arrow-height)
        "0.5rem"))
  (define popover-header-bg
    (or (theme-component-ref theme 'popover-header-bg)
        (hash-ref surface-map 'surface-muted)))
  (define popover-header-color
    (or (theme-component-ref theme 'popover-header-color)
        (rgb->css emphasis-fg)))
  (define popover-header-padding-x
    (or (theme-component-ref theme 'popover-header-padding-x)
        "0.75rem"))
  (define popover-header-padding-y
    (or (theme-component-ref theme 'popover-header-padding-y)
        "0.5rem"))
  (define popover-body-padding-x
    (or (theme-component-ref theme 'popover-body-padding-x)
        "0.75rem"))
  (define popover-body-padding-y
    (or (theme-component-ref theme 'popover-body-padding-y)
        "0.5rem"))
  (define popover-title-font-size
    (or (theme-component-ref theme 'popover-title-font-size)
        (bootstrap-theme-font-size-lg theme)))
  (define popover-title-font-weight
    (or (theme-component-ref theme 'popover-title-font-weight)
        (bootstrap-theme-font-weight-semibold theme)))
  (define button-padding-x
    (or (theme-component-ref theme 'btn-padding-x)
        "0.75rem"))
  (define button-padding-y
    (or (theme-component-ref theme 'btn-padding-y)
        "0.375rem"))
  (define button-font-size
    (or (theme-component-ref theme 'btn-font-size)
        (bootstrap-theme-font-size-md theme)))
  (define button-font-weight
    (or (theme-component-ref theme 'btn-font-weight)
        (bootstrap-theme-font-weight-normal theme)))
  (define button-line-height
    (or (theme-component-ref theme 'btn-line-height)
        "1.25"))
  (define button-radius
    (or (theme-component-ref theme 'btn-radius)
        "0"))
  (define button-text-transform
    (or (theme-component-ref theme 'btn-text-transform)
        "none"))
  (define button-secondary-bg
    (or (theme-component-ref theme 'btn-secondary-bg)
        (rgb->css (or secondary-bg (mix-colors bg fg 0.08)))))
  (define button-secondary-border
    (or (theme-component-ref theme 'btn-secondary-border)
        (theme-component-ref theme 'btn-outline-secondary-border)
        separator-strong))
  (define button-secondary-color
    (or (theme-component-ref theme 'btn-secondary-color)
        (theme-component-ref theme 'btn-outline-secondary-color)
        (rgb->css emphasis-fg)))
  (define button-secondary-hover-bg
    (or (theme-component-ref theme 'btn-secondary-hover-bg)
        bg-hover))
  (define button-secondary-hover-border
    (or (theme-component-ref theme 'btn-secondary-hover-border)
        button-secondary-border))
  (define button-secondary-hover-color
    (or (theme-component-ref theme 'btn-outline-secondary-hover-color)
        button-secondary-color))
  (define button-light-bg
    (or (theme-component-ref theme 'btn-light-bg)
        (bootstrap-theme-light theme)))
  (define button-light-border
    (or (theme-component-ref theme 'btn-light-border)
        (bootstrap-theme-light theme)))
  (define button-light-color
    (or (theme-component-ref theme 'btn-light-color)
        (theme-prop-ref theme "--bs-light-text-emphasis")
        (rgb->css emphasis-fg)))
  (define button-light-hover-bg
    (or (theme-component-ref theme 'btn-light-hover-bg)
        button-light-bg))
  (define button-light-hover-border
    (or (theme-component-ref theme 'btn-light-hover-border)
        button-light-border))
  (define outline-secondary-color
    (or (theme-component-ref theme 'btn-outline-secondary-color)
        button-secondary-color))
  (define outline-secondary-border
    (or (theme-component-ref theme 'btn-outline-secondary-border)
        button-secondary-border))
  (define outline-secondary-hover-bg
    (or (theme-component-ref theme 'btn-outline-secondary-hover-bg)
        button-secondary-bg))
  (define outline-secondary-hover-border
    (or (theme-component-ref theme 'btn-outline-secondary-hover-border)
        button-secondary-border))
  (define button-success-color
    (or (theme-component-ref theme 'btn-success-color)
        (bootstrap-theme-success theme)))
  (define button-info-color
    (or (theme-component-ref theme 'btn-info-color)
        (bootstrap-theme-info theme)))
  (define button-warning-color
    (or (theme-component-ref theme 'btn-warning-color)
        (bootstrap-theme-warning theme)))
  (define button-danger-color
    (or (theme-component-ref theme 'btn-danger-color)
        (bootstrap-theme-danger theme)))
  (define card-bg
    (or (theme-component-ref theme 'card-bg)
        (hash-ref surface-map 'surface)))
  (define card-radius
    (or (theme-component-ref theme 'card-radius)
        "0"))
  (define card-shadow
    (or (theme-component-ref theme 'card-shadow)
        "none"))
  (define card-border-color
    (or (theme-component-ref theme 'card-border-color)
        separator))
  (define card-cap-bg
    (or (theme-component-ref theme 'card-cap-bg)
        (hash-ref surface-map 'surface-subtle)))
  (define card-cap-color
    (or (theme-component-ref theme 'card-cap-color)
        fg-muted))
  (define card-cap-padding-x
    (or (theme-component-ref theme 'card-cap-padding-x)
        "0.85rem"))
  (define card-cap-padding-y
    (or (theme-component-ref theme 'card-cap-padding-y)
        "0.6rem"))
  (define card-body-padding-x
    (or (theme-component-ref theme 'card-body-padding-x)
        "0.85rem"))
  (define card-body-padding-y
    (or (theme-component-ref theme 'card-body-padding-y)
        "0.85rem"))
  (define badge-padding-x
    (or (theme-component-ref theme 'badge-padding-x)
        "0.65em"))
  (define badge-padding-y
    (or (theme-component-ref theme 'badge-padding-y)
        "0.35em"))
  (define badge-font-size
    (or (theme-component-ref theme 'badge-font-size)
        "0.75em"))
  (define badge-font-weight
    (or (theme-component-ref theme 'badge-font-weight)
        (bootstrap-theme-font-weight-semibold theme)))
  (define badge-radius
    (or (theme-component-ref theme 'badge-radius)
        "0"))
  (define badge-color
    (or (theme-component-ref theme 'badge-color)
        (rgb->css emphasis-fg)))
  (define badge-secondary-color
    (or (theme-component-ref theme 'badge-secondary-color)
        (rgb->css emphasis-fg)))
  (define badge-light-color
    (or (theme-component-ref theme 'badge-light-color)
        (rgb->css emphasis-fg)))
  (define accordion-bg
    (or (theme-component-ref theme 'accordion-bg)
        (hash-ref surface-map 'surface)))
  (define accordion-border-color
    (or (theme-component-ref theme 'accordion-border-color)
        separator))
  (define accordion-border-width
    (or (theme-component-ref theme 'accordion-border-width)
        "1px"))
  (define accordion-radius
    (or (theme-component-ref theme 'accordion-radius)
        "0"))
  (define accordion-trigger-radius-top
    (or (theme-component-ref theme 'accordion-trigger-radius-top)
        accordion-radius))
  (define accordion-trigger-radius-bottom
    (or (theme-component-ref theme 'accordion-trigger-radius-bottom)
        accordion-radius))
  (define accordion-button-padding-x
    (or (theme-component-ref theme 'accordion-button-padding-x)
        "1rem"))
  (define accordion-button-padding-y
    (or (theme-component-ref theme 'accordion-button-padding-y)
        "0.75rem"))
  (define accordion-button-color
    (or (theme-component-ref theme 'accordion-button-color)
        (bootstrap-theme-fg theme)))
  (define accordion-button-bg
    (or (theme-component-ref theme 'accordion-button-bg)
        (hash-ref surface-map 'surface-subtle)))
  (define accordion-icon
    (or (theme-component-ref theme 'accordion-icon)
        "none"))
  (define accordion-active-icon
    (or (theme-component-ref theme 'accordion-active-icon)
        accordion-icon))
  (define accordion-icon-width
    (or (theme-component-ref theme 'accordion-icon-width)
        "1rem"))
  (define accordion-icon-transform
    (or (theme-component-ref theme 'accordion-icon-transform)
        "rotate(90deg)"))
  (define accordion-active-color
    (or (theme-component-ref theme 'accordion-active-color)
        accordion-button-color))
  (define accordion-active-bg
    (or (theme-component-ref theme 'accordion-active-bg)
        accordion-button-bg))
  (define accordion-body-padding-x
    (or (theme-component-ref theme 'accordion-body-padding-x)
        "1rem"))
  (define accordion-body-padding-y
    (or (theme-component-ref theme 'accordion-body-padding-y)
        "0.75rem"))
  (define pagination-padding-x
    (or (theme-component-ref theme 'pagination-padding-x)
        "0.75rem"))
  (define pagination-padding-y
    (or (theme-component-ref theme 'pagination-padding-y)
        "0.375rem"))
  (define pagination-font-size
    (or (theme-component-ref theme 'pagination-font-size)
        (bootstrap-theme-font-size-md theme)))
  (define pagination-color
    (or (theme-component-ref theme 'pagination-color)
        fg-muted))
  (define pagination-bg
    (or (theme-component-ref theme 'pagination-bg)
        (hash-ref surface-map 'surface)))
  (define pagination-border-width
    (or (theme-component-ref theme 'pagination-border-width)
        "0"))
  (define pagination-border-color
    (or (theme-component-ref theme 'pagination-border-color)
        "transparent"))
  (define pagination-radius
    (or (theme-component-ref theme 'pagination-radius)
        "0"))
  (define pagination-edge-radius
    (or (theme-component-ref theme 'pagination-edge-radius)
        pagination-radius))
  (define pagination-hover-color
    (or (theme-component-ref theme 'pagination-hover-color)
        pagination-color))
  (define pagination-hover-bg
    (or (theme-component-ref theme 'pagination-hover-bg)
        pagination-bg))
  (define pagination-hover-border-color
    (or (theme-component-ref theme 'pagination-hover-border-color)
        pagination-border-color))
  (define pagination-active-color
    (or (theme-component-ref theme 'pagination-active-color)
        (derive-on-color focus-base)))
  (define pagination-active-bg
    (or (theme-component-ref theme 'pagination-active-bg)
        (bootstrap-theme-primary theme)))
  (define pagination-active-border-color
    (or (theme-component-ref theme 'pagination-active-border-color)
        pagination-active-bg))
  (define pagination-disabled-color
    (or (theme-component-ref theme 'pagination-disabled-color)
        fg-muted))
  (define pagination-disabled-bg
    (or (theme-component-ref theme 'pagination-disabled-bg)
        bg-disabled))
  (define pagination-disabled-border-color
    (or (theme-component-ref theme 'pagination-disabled-border-color)
        pagination-border-color))
  (define progress-height
    (or (theme-component-ref theme 'progress-height)
        "1rem"))
  (define progress-font-size
    (or (theme-component-ref theme 'progress-font-size)
        "0.75rem"))
  (define progress-bg
    (or (theme-component-ref theme 'progress-bg)
        (hash-ref surface-map 'surface-muted)))
  (define progress-radius
    (or (theme-component-ref theme 'progress-radius)
        "0"))
  (define progress-box-shadow
    (or (theme-component-ref theme 'progress-box-shadow)
        "none"))
  (define progress-bar-color
    (or (theme-component-ref theme 'progress-bar-color)
        "#fff"))
  (define progress-bar-bg
    (or (theme-component-ref theme 'progress-bar-bg)
        (bootstrap-theme-primary theme)))
  (define breadcrumb-divider-color
    (or (theme-component-ref theme 'breadcrumb-divider-color)
        fg-muted))
  (define breadcrumb-item-padding-x
    (or (theme-component-ref theme 'breadcrumb-item-padding-x)
        "0.5rem"))
  (define breadcrumb-active-color
    (or (theme-component-ref theme 'breadcrumb-active-color)
        fg-muted))
  (define breadcrumb-link-color
    (or (theme-component-ref theme 'breadcrumb-link-color)
        (theme-prop-ref theme "--bs-link-color")
        (rgb->css emphasis-fg)))
  (define toast-padding-x
    (or (theme-component-ref theme 'toast-padding-x)
        "0.75rem"))
  (define toast-padding-y
    (or (theme-component-ref theme 'toast-padding-y)
        "0.5rem"))
  (define toast-font-size
    (or (theme-component-ref theme 'toast-font-size)
        "0.875rem"))
  (define toast-color
    (or (theme-component-ref theme 'toast-color)
        (bootstrap-theme-fg theme)))
  (define toast-bg
    (or (theme-component-ref theme 'toast-bg)
        popup-bg))
  (define toast-border-width
    (or (theme-component-ref theme 'toast-border-width)
        "1px"))
  (define toast-border-color
    (or (theme-component-ref theme 'toast-border-color)
        popup-border))
  (define toast-radius
    (or (theme-component-ref theme 'toast-radius)
        "0"))
  (define toast-shadow
    (or (theme-component-ref theme 'toast-shadow)
        "var(--we-shadow-md)"))
  (define toast-header-color
    (or (theme-component-ref theme 'toast-header-color)
        fg-muted))
  (define toast-header-bg
    (or (theme-component-ref theme 'toast-header-bg)
        "transparent"))
  (define toast-header-border-color
    (or (theme-component-ref theme 'toast-header-border-color)
        separator))
  (define toast-body-padding
    (or (theme-component-ref theme 'toast-body-padding)
        toast-padding-x))
  (define close-button-color
    (or (theme-component-ref theme 'close-button-color)
        "#000"))
  (define close-button-opacity
    (or (theme-component-ref theme 'close-button-opacity)
        "0.5"))
  (define close-button-hover-opacity
    (or (theme-component-ref theme 'close-button-hover-opacity)
        "0.75"))
  (define close-button-radius
    (or (theme-component-ref theme 'close-button-radius)
        "0"))
  (define (alert-token tone role fallback)
    (or (theme-component-ref theme (string->symbol (format "alert-~a-~a" tone role)))
        fallback))
  (define alert-primary-bg
    (alert-token "primary" "bg"
                 (or (theme-prop-ref theme "--bs-primary-bg-subtle")
                     (bootstrap-theme-primary theme))))
  (define alert-primary-border
    (alert-token "primary" "border"
                 (or (theme-prop-ref theme "--bs-primary-border-subtle")
                     (bootstrap-theme-primary theme))))
  (define alert-primary-color
    (alert-token "primary" "color"
                 (or (theme-prop-ref theme "--bs-primary-text-emphasis")
                     (bootstrap-theme-primary theme))))
  (define alert-secondary-bg
    (alert-token "secondary" "bg"
                 (or (theme-prop-ref theme "--bs-secondary-bg-subtle")
                     (bootstrap-theme-secondary theme))))
  (define alert-secondary-border
    (alert-token "secondary" "border"
                 (or (theme-prop-ref theme "--bs-secondary-border-subtle")
                     (bootstrap-theme-secondary theme))))
  (define alert-secondary-color
    (alert-token "secondary" "color"
                 (or (theme-prop-ref theme "--bs-secondary-text-emphasis")
                     (bootstrap-theme-secondary theme))))
  (define alert-success-bg
    (alert-token "success" "bg"
                 (or (theme-prop-ref theme "--bs-success-bg-subtle")
                     (bootstrap-theme-success theme))))
  (define alert-success-border
    (alert-token "success" "border"
                 (or (theme-prop-ref theme "--bs-success-border-subtle")
                     (bootstrap-theme-success theme))))
  (define alert-success-color
    (alert-token "success" "color"
                 (or (theme-prop-ref theme "--bs-success-text-emphasis")
                     (bootstrap-theme-success theme))))
  (define alert-info-bg
    (alert-token "info" "bg"
                 (or (theme-prop-ref theme "--bs-info-bg-subtle")
                     (bootstrap-theme-info theme))))
  (define alert-info-border
    (alert-token "info" "border"
                 (or (theme-prop-ref theme "--bs-info-border-subtle")
                     (bootstrap-theme-info theme))))
  (define alert-info-color
    (alert-token "info" "color"
                 (or (theme-prop-ref theme "--bs-info-text-emphasis")
                     (bootstrap-theme-info theme))))
  (define alert-warning-bg
    (alert-token "warning" "bg"
                 (or (theme-prop-ref theme "--bs-warning-bg-subtle")
                     (bootstrap-theme-warning theme))))
  (define alert-warning-border
    (alert-token "warning" "border"
                 (or (theme-prop-ref theme "--bs-warning-border-subtle")
                     (bootstrap-theme-warning theme))))
  (define alert-warning-color
    (alert-token "warning" "color"
                 (or (theme-prop-ref theme "--bs-warning-text-emphasis")
                     (bootstrap-theme-warning theme))))
  (define alert-danger-bg
    (alert-token "danger" "bg"
                 (or (theme-prop-ref theme "--bs-danger-bg-subtle")
                     (bootstrap-theme-danger theme))))
  (define alert-danger-border
    (alert-token "danger" "border"
                 (or (theme-prop-ref theme "--bs-danger-border-subtle")
                     (bootstrap-theme-danger theme))))
  (define alert-danger-color
    (alert-token "danger" "color"
                 (or (theme-prop-ref theme "--bs-danger-text-emphasis")
                     (bootstrap-theme-danger theme))))
  (define alert-light-bg
    (alert-token "light" "bg"
                 (or (theme-prop-ref theme "--bs-light-bg-subtle")
                     (bootstrap-theme-light theme))))
  (define alert-light-border
    (alert-token "light" "border"
                 (or (theme-prop-ref theme "--bs-light-border-subtle")
                     (bootstrap-theme-light theme))))
  (define alert-light-color
    (alert-token "light" "color"
                 (or (theme-prop-ref theme "--bs-light-text-emphasis")
                     (bootstrap-theme-light theme))))
  (define alert-dark-bg
    (alert-token "dark" "bg"
                 (or (theme-prop-ref theme "--bs-dark-bg-subtle")
                     (bootstrap-theme-dark theme))))
  (define alert-dark-border
    (alert-token "dark" "border"
                 (or (theme-prop-ref theme "--bs-dark-border-subtle")
                     (bootstrap-theme-dark theme))))
  (define alert-dark-color
    (alert-token "dark" "color"
                 (or (theme-prop-ref theme "--bs-dark-text-emphasis")
                     (bootstrap-theme-dark theme))))
  (define list-group-bg
    (or (theme-component-ref theme 'list-group-bg)
        (hash-ref surface-map 'surface)))
  (define list-group-border-color
    (or (theme-component-ref theme 'list-group-border-color)
        separator))
  (define list-group-border-width
    (or (theme-component-ref theme 'list-group-border-width)
        "1px"))
  (define list-group-radius
    (or (theme-component-ref theme 'list-group-radius)
        "0"))
  (define list-group-item-radius
    (or (theme-component-ref theme 'list-group-item-radius)
        "0"))
  (define list-group-padding-x
    (or (theme-component-ref theme 'list-group-padding-x)
        "1rem"))
  (define list-group-padding-y
    (or (theme-component-ref theme 'list-group-padding-y)
        "0.5rem"))
  (define list-group-color
    (or (theme-component-ref theme 'list-group-color)
        (bootstrap-theme-fg theme)))
  (define list-group-active-bg
    (or (theme-component-ref theme 'list-group-active-bg)
        bg-selected))
  (define list-group-active-color
    (or (theme-component-ref theme 'list-group-active-color)
        (rgb->css emphasis-fg)))
  (define list-group-active-border
    (or (theme-component-ref theme 'list-group-active-border)
        (theme-prop-ref theme "--bs-primary-border-subtle")
        separator-strong))
  (define table-bg
    (or (theme-component-ref theme 'table-bg)
        (hash-ref surface-map 'surface)))
  (define table-border-color
    (or (theme-component-ref theme 'table-border-color)
        separator))
  (define table-color
    (or (theme-component-ref theme 'table-color)
        (bootstrap-theme-fg theme)))
  (define table-striped-bg
    (or (theme-component-ref theme 'table-striped-bg)
        (hash-ref surface-map 'surface-muted)))
  (define table-hover-bg
    (or (theme-component-ref theme 'table-hover-bg)
        bg-hover))
  (define table-active-bg
    (or (theme-component-ref theme 'table-active-bg)
        (hash-ref surface-map 'surface-active)))
  (define table-cell-padding-x
    (or (theme-component-ref theme 'table-cell-padding-x)
        "0.75rem"))
  (define table-cell-padding-y
    (or (theme-component-ref theme 'table-cell-padding-y)
        "0.5rem"))
  (define table-border-width
    (or (theme-component-ref theme 'table-border-width)
        "1px"))
  (define table-header-color
    (or (theme-component-ref theme 'table-header-color)
        (rgb->css emphasis-fg)))
  (define table-header-font-size
    (or (theme-component-ref theme 'table-header-font-size)
        (bootstrap-theme-font-size-md theme)))
  (define table-header-font-weight
    (or (theme-component-ref theme 'table-header-font-weight)
        (bootstrap-theme-font-weight-bold theme)))
  (define table-header-text-transform
    (or (theme-component-ref theme 'table-header-text-transform)
        "none"))
  (define table-body-font-weight
    (or (theme-component-ref theme 'table-body-font-weight)
        (bootstrap-theme-font-weight-normal theme)))
  (define field-bg
    (or (theme-component-ref theme 'field-bg)
        (hash-ref surface-map 'surface)))
  (define field-color
    (or (theme-component-ref theme 'field-color)
        (bootstrap-theme-fg theme)))
  (define field-border-color
    (or (theme-component-ref theme 'field-border-color)
        control-border))
  (define field-border-width
    (or (theme-component-ref theme 'field-border-width)
        "1px"))
  (define field-radius
    (or (theme-component-ref theme 'field-radius)
        (bootstrap-theme-radius-md theme)))
  (define field-padding-x
    (or (theme-component-ref theme 'field-padding-x)
        "0.75rem"))
  (define field-padding-y
    (or (theme-component-ref theme 'field-padding-y)
        "0.5rem"))
  (define field-font-size
    (or (theme-component-ref theme 'field-font-size)
        (bootstrap-theme-font-size-md theme)))
  (define field-font-weight
    (or (theme-component-ref theme 'field-font-weight)
        (bootstrap-theme-font-weight-normal theme)))
  (define field-focus-bg
    (or (theme-component-ref theme 'field-focus-bg)
        field-bg))
  (define field-focus-border-color
    (or (theme-component-ref theme 'field-focus-border-color)
        control-border-focus))
  (define input-group-addon-bg
    (or (theme-component-ref theme 'input-group-addon-bg)
        (hash-ref surface-map 'surface-muted)))
  (define input-group-addon-color
    (or (theme-component-ref theme 'input-group-addon-color)
        field-color))
  (define input-group-addon-border-color
    (or (theme-component-ref theme 'input-group-addon-border-color)
        field-border-color))
  (define input-group-addon-padding-x
    (or (theme-component-ref theme 'input-group-addon-padding-x)
        field-padding-x))
  (define input-group-addon-padding-y
    (or (theme-component-ref theme 'input-group-addon-padding-y)
        field-padding-y))
  (define input-group-addon-font-size
    (or (theme-component-ref theme 'input-group-addon-font-size)
        field-font-size))
  (define input-group-addon-font-weight
    (or (theme-component-ref theme 'input-group-addon-font-weight)
        field-font-weight))
  (define field-valid-color
    (or (theme-component-ref theme 'field-valid-color)
        (bootstrap-theme-success theme)))
  (define field-valid-border-color
    (or (theme-component-ref theme 'field-valid-border-color)
        field-valid-color))
  (define field-valid-icon
    (or (theme-component-ref theme 'field-valid-icon)
        "none"))
  (define field-invalid-color
    (or (theme-component-ref theme 'field-invalid-color)
        (bootstrap-theme-danger theme)))
  (define field-invalid-border-color
    (or (theme-component-ref theme 'field-invalid-border-color)
        field-invalid-color))
  (define field-invalid-icon
    (or (theme-component-ref theme 'field-invalid-icon)
        "none"))
  (define check-bg
    (or (theme-component-ref theme 'check-bg)
        field-bg))
  (define check-border-color
    (or (theme-component-ref theme 'check-border-color)
        field-border-color))
  (define check-border-width
    (or (theme-component-ref theme 'check-border-width)
        field-border-width))
  (define check-size
    (or (theme-component-ref theme 'check-size)
        "1em"))
  (define check-height
    (or (theme-component-ref theme 'check-height)
        check-size))
  (define check-radius
    (or (theme-component-ref theme 'check-radius)
        "0.25em"))
  (define check-focus-border-color
    (or (theme-component-ref theme 'check-focus-border-color)
        field-focus-border-color))
  (define check-focus-shadow
    (or (theme-component-ref theme 'check-focus-shadow)
        "none"))
  (define check-checked-bg
    (or (theme-component-ref theme 'check-checked-bg)
        (bootstrap-theme-primary theme)))
  (define check-checked-border-color
    (or (theme-component-ref theme 'check-checked-border-color)
        check-checked-bg))
  (define check-checked-icon
    (or (theme-component-ref theme 'check-checked-icon)
        "none"))
  (define check-disabled-opacity
    (or (theme-component-ref theme 'check-disabled-opacity)
        "0.5"))
  (define switch-width
    (or (theme-component-ref theme 'switch-width)
        "2em"))
  (define switch-radius
    (or (theme-component-ref theme 'switch-radius)
        check-radius))
  (define switch-bg-icon
    (or (theme-component-ref theme 'switch-bg-icon)
        "none"))
  (define switch-focus-icon
    (or (theme-component-ref theme 'switch-focus-icon)
        switch-bg-icon))
  (define switch-checked-icon
    (or (theme-component-ref theme 'switch-checked-icon)
        switch-bg-icon))
  (define dialog-bg
    (or (theme-component-ref theme 'modal-bg)
        popup-bg))
  (define dialog-color
    (or (theme-component-ref theme 'modal-color)
        (bootstrap-theme-fg theme)))
  (define dialog-border-color
    (or (theme-component-ref theme 'modal-border-color)
        control-border))
  (define dialog-border-width
    (or (theme-component-ref theme 'modal-border-width)
        "1px"))
  (define dialog-radius
    (or (theme-component-ref theme 'modal-radius)
        "0"))
  (define dialog-shadow
    (or (theme-component-ref theme 'modal-shadow)
        "none"))
  (define dialog-header-padding-x
    (or (theme-component-ref theme 'modal-header-padding-x)
        "0.9rem"))
  (define dialog-header-padding-y
    (or (theme-component-ref theme 'modal-header-padding-y)
        "0.7rem"))
  (define dialog-header-border-color
    (or (theme-component-ref theme 'modal-header-border-color)
        separator))
  (define dialog-header-border-width
    (or (theme-component-ref theme 'modal-header-border-width)
        "1px"))
  (define dialog-body-padding-x
    (or (theme-component-ref theme 'modal-body-padding-x)
        "0.9rem"))
  (define dialog-body-padding-y
    (or (theme-component-ref theme 'modal-body-padding-y)
        "0.85rem"))
  (define dialog-footer-padding-x
    (or (theme-component-ref theme 'modal-footer-padding-x)
        "0.9rem"))
  (define dialog-footer-padding-y
    (or (theme-component-ref theme 'modal-footer-padding-y)
        "0.65rem"))
  (define dialog-footer-gap
    (or (theme-component-ref theme 'modal-footer-gap)
        "0.5rem"))
  (define dialog-footer-border-color
    (or (theme-component-ref theme 'modal-footer-border-color)
        separator))
  (define dialog-footer-border-width
    (or (theme-component-ref theme 'modal-footer-border-width)
        "1px"))
  (define dialog-title-color
    (or (theme-component-ref theme 'modal-title-color)
        (rgb->css emphasis-fg)))
  (define dialog-title-font-size
    (or (theme-component-ref theme 'modal-title-font-size)
        (bootstrap-theme-font-size-lg theme)))
  (define dialog-title-font-weight
    (or (theme-component-ref theme 'modal-title-font-weight)
        (bootstrap-theme-font-weight-semibold theme)))
  (define dialog-title-text-transform
    (or (theme-component-ref theme 'modal-title-text-transform)
        "none"))
  (define dialog-close-icon
    (or (theme-component-ref theme 'modal-close-bg)
        (theme-component-ref theme 'close-button-bg)
        "none"))
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
    (cons "--we-menu-popup-bg" menu-popup-bg)
    (cons "--we-menu-popup-border-color" menu-popup-border-color)
    (cons "--we-menu-popup-border-width" menu-popup-border-width)
    (cons "--we-menu-popup-radius" menu-popup-radius)
    (cons "--we-menu-popup-shadow" menu-popup-shadow)
    (cons "--we-menu-popup-padding-y" menu-popup-padding-y)
    (cons "--we-menu-header-color" menu-header-color)
    (cons "--we-menu-header-padding-x" menu-header-padding-x)
    (cons "--we-menu-header-padding-y" menu-header-padding-y)
    (cons "--we-menu-divider-color" menu-divider-color)
    (cons "--we-menu-item-color" menu-item-color)
    (cons "--we-menu-item-padding-x" menu-item-padding-x)
    (cons "--we-menu-item-padding-y" menu-item-padding-y)
    (cons "--we-menu-item-font-size" menu-item-font-size)
    (cons "--we-menu-item-font-weight" menu-item-font-weight)
    (cons "--we-menu-item-text-transform" menu-item-text-transform)
    (cons "--we-menu-item-radius" menu-item-radius)
    (cons "--we-menu-item-hover-bg" menu-item-hover-bg)
    (cons "--we-menu-item-hover-fg" menu-item-hover-fg)
    (cons "--we-menu-item-active-bg" menu-item-active-bg)
    (cons "--we-menu-item-active-fg" menu-item-active-fg)
    (cons "--we-tab-active-border" bg-selected)
    (cons "--we-navbar-bg" navbar-bg)
    (cons "--we-navbar-border" navbar-border)
    (cons "--we-navbar-link-color" navbar-link-color)
    (cons "--we-navbar-link-hover-color" navbar-link-hover)
    (cons "--we-navbar-link-active-color" navbar-link-active)
    (cons "--we-navbar-link-disabled-color" navbar-link-disabled)
    (cons "--we-navbar-brand-color" navbar-brand-color)
    (cons "--we-navbar-brand-hover-color" navbar-brand-hover)
    (cons "--we-navbar-brand-font-size" navbar-brand-size)
    (cons "--we-navbar-brand-gap" navbar-brand-gap)
    (cons "--we-nav-font-size" nav-font-size)
    (cons "--we-nav-font-weight" nav-font-weight)
    (cons "--we-nav-text-transform" nav-text-transform)
    (cons "--we-nav-link-padding-x" nav-link-padding-x)
    (cons "--we-nav-link-padding-y" nav-link-padding-y)
    (cons "--we-tab-bg" tab-bg)
    (cons "--we-tab-color" tab-color)
    (cons "--we-tab-border-color" tab-border-color)
    (cons "--we-tab-border-radius" tab-border-radius)
    (cons "--we-tab-hover-border-color" tab-hover-border)
    (cons "--we-tab-active-bg" tab-active-bg)
    (cons "--we-tab-active-color" tab-active-color)
    (cons "--we-tab-active-border-color" tab-active-border)
    (cons "--we-pill-color" pill-color)
    (cons "--we-pill-border-color" pill-border)
    (cons "--we-pill-active-color" pill-active-color)
    (cons "--we-pill-active-bg" pill-active-bg)
    (cons "--we-pill-radius" pill-radius)
    (cons "--we-button-padding-x" button-padding-x)
    (cons "--we-button-padding-y" button-padding-y)
    (cons "--we-button-font-size" button-font-size)
    (cons "--we-button-font-weight" button-font-weight)
    (cons "--we-button-line-height" button-line-height)
    (cons "--we-button-radius" button-radius)
    (cons "--we-button-text-transform" button-text-transform)
    (cons "--we-button-secondary-bg" button-secondary-bg)
    (cons "--we-button-secondary-border" button-secondary-border)
    (cons "--we-button-secondary-color" button-secondary-color)
    (cons "--we-button-secondary-hover-bg" button-secondary-hover-bg)
    (cons "--we-button-secondary-hover-border" button-secondary-hover-border)
    (cons "--we-button-secondary-hover-color" button-secondary-hover-color)
    (cons "--we-button-light-bg" button-light-bg)
    (cons "--we-button-light-border" button-light-border)
    (cons "--we-button-light-color" button-light-color)
    (cons "--we-button-light-hover-bg" button-light-hover-bg)
    (cons "--we-button-light-hover-border" button-light-hover-border)
    (cons "--we-button-outline-secondary-color" outline-secondary-color)
    (cons "--we-button-outline-secondary-border" outline-secondary-border)
    (cons "--we-button-outline-secondary-hover-bg" outline-secondary-hover-bg)
    (cons "--we-button-outline-secondary-hover-border" outline-secondary-hover-border)
    (cons "--we-button-success-color" button-success-color)
    (cons "--we-button-info-color" button-info-color)
    (cons "--we-button-warning-color" button-warning-color)
    (cons "--we-button-danger-color" button-danger-color)
    (cons "--we-card-bg" card-bg)
    (cons "--we-card-radius" card-radius)
    (cons "--we-card-shadow" card-shadow)
    (cons "--we-card-border-color" card-border-color)
    (cons "--we-card-cap-bg" card-cap-bg)
    (cons "--we-card-cap-color" card-cap-color)
    (cons "--we-card-cap-padding-x" card-cap-padding-x)
    (cons "--we-card-cap-padding-y" card-cap-padding-y)
    (cons "--we-card-body-padding-x" card-body-padding-x)
    (cons "--we-card-body-padding-y" card-body-padding-y)
    (cons "--we-badge-padding-x" badge-padding-x)
    (cons "--we-badge-padding-y" badge-padding-y)
    (cons "--we-badge-font-size" badge-font-size)
    (cons "--we-badge-font-weight" badge-font-weight)
    (cons "--we-badge-radius" badge-radius)
    (cons "--we-badge-color" badge-color)
    (cons "--we-badge-secondary-color" badge-secondary-color)
    (cons "--we-badge-light-color" badge-light-color)
    (cons "--we-accordion-bg" accordion-bg)
    (cons "--we-accordion-border-color" accordion-border-color)
    (cons "--we-accordion-border-width" accordion-border-width)
    (cons "--we-accordion-radius" accordion-radius)
    (cons "--we-accordion-trigger-radius-top" accordion-trigger-radius-top)
    (cons "--we-accordion-trigger-radius-bottom" accordion-trigger-radius-bottom)
    (cons "--we-accordion-button-padding-x" accordion-button-padding-x)
    (cons "--we-accordion-button-padding-y" accordion-button-padding-y)
    (cons "--we-accordion-button-color" accordion-button-color)
    (cons "--we-accordion-button-bg" accordion-button-bg)
    (cons "--we-accordion-icon" accordion-icon)
    (cons "--we-accordion-active-icon" accordion-active-icon)
    (cons "--we-accordion-icon-width" accordion-icon-width)
    (cons "--we-accordion-icon-transform" accordion-icon-transform)
    (cons "--we-accordion-active-color" accordion-active-color)
    (cons "--we-accordion-active-bg" accordion-active-bg)
    (cons "--we-accordion-body-padding-x" accordion-body-padding-x)
    (cons "--we-accordion-body-padding-y" accordion-body-padding-y)
    (cons "--we-pagination-padding-x" pagination-padding-x)
    (cons "--we-pagination-padding-y" pagination-padding-y)
    (cons "--we-pagination-font-size" pagination-font-size)
    (cons "--we-pagination-color" pagination-color)
    (cons "--we-pagination-bg" pagination-bg)
    (cons "--we-pagination-border-width" pagination-border-width)
    (cons "--we-pagination-border-color" pagination-border-color)
    (cons "--we-pagination-radius" pagination-radius)
    (cons "--we-pagination-edge-radius" pagination-edge-radius)
    (cons "--we-pagination-hover-color" pagination-hover-color)
    (cons "--we-pagination-hover-bg" pagination-hover-bg)
    (cons "--we-pagination-hover-border-color" pagination-hover-border-color)
    (cons "--we-pagination-active-color" pagination-active-color)
    (cons "--we-pagination-active-bg" pagination-active-bg)
    (cons "--we-pagination-active-border-color" pagination-active-border-color)
    (cons "--we-pagination-disabled-color" pagination-disabled-color)
    (cons "--we-pagination-disabled-bg" pagination-disabled-bg)
    (cons "--we-pagination-disabled-border-color" pagination-disabled-border-color)
    (cons "--we-progress-height" progress-height)
    (cons "--we-progress-font-size" progress-font-size)
    (cons "--we-progress-bg" progress-bg)
    (cons "--we-progress-radius" progress-radius)
    (cons "--we-progress-box-shadow" progress-box-shadow)
    (cons "--we-progress-bar-color" progress-bar-color)
    (cons "--we-progress-bar-bg" progress-bar-bg)
    (cons "--we-breadcrumb-divider-color" breadcrumb-divider-color)
    (cons "--we-breadcrumb-item-padding-x" breadcrumb-item-padding-x)
    (cons "--we-breadcrumb-active-color" breadcrumb-active-color)
    (cons "--we-breadcrumb-link-color" breadcrumb-link-color)
    (cons "--we-toast-padding-x" toast-padding-x)
    (cons "--we-toast-padding-y" toast-padding-y)
    (cons "--we-toast-font-size" toast-font-size)
    (cons "--we-toast-color" toast-color)
    (cons "--we-toast-bg" toast-bg)
    (cons "--we-toast-border-width" toast-border-width)
    (cons "--we-toast-border-color" toast-border-color)
    (cons "--we-toast-radius" toast-radius)
    (cons "--we-toast-shadow" toast-shadow)
    (cons "--we-toast-header-color" toast-header-color)
    (cons "--we-toast-header-bg" toast-header-bg)
    (cons "--we-toast-header-border-color" toast-header-border-color)
    (cons "--we-toast-body-padding" toast-body-padding)
    (cons "--we-tooltip-bg" tooltip-bg)
    (cons "--we-tooltip-color" tooltip-color)
    (cons "--we-tooltip-font-size" tooltip-font-size)
    (cons "--we-tooltip-radius" tooltip-radius)
    (cons "--we-tooltip-opacity" tooltip-opacity)
    (cons "--we-tooltip-padding-x" tooltip-padding-x)
    (cons "--we-tooltip-padding-y" tooltip-padding-y)
    (cons "--we-tooltip-border-color" tooltip-border-color)
    (cons "--we-tooltip-shadow" tooltip-shadow)
    (cons "--we-tooltip-arrow-size" tooltip-arrow-size)
    (cons "--we-popover-bg" popover-bg)
    (cons "--we-popover-max-width" popover-max-width)
    (cons "--we-popover-font-size" popover-font-size)
    (cons "--we-popover-color" popover-color)
    (cons "--we-popover-border-color" popover-border-color)
    (cons "--we-popover-border-width" popover-border-width)
    (cons "--we-popover-radius" popover-radius)
    (cons "--we-popover-shadow" popover-shadow)
    (cons "--we-popover-arrow-width" popover-arrow-width)
    (cons "--we-popover-arrow-height" popover-arrow-height)
    (cons "--we-popover-header-bg" popover-header-bg)
    (cons "--we-popover-header-color" popover-header-color)
    (cons "--we-popover-header-padding-x" popover-header-padding-x)
    (cons "--we-popover-header-padding-y" popover-header-padding-y)
    (cons "--we-popover-body-padding-x" popover-body-padding-x)
    (cons "--we-popover-body-padding-y" popover-body-padding-y)
    (cons "--we-popover-title-font-size" popover-title-font-size)
    (cons "--we-popover-title-font-weight" popover-title-font-weight)
    (cons "--we-close-button-color" close-button-color)
    (cons "--we-close-button-opacity" close-button-opacity)
    (cons "--we-close-button-hover-opacity" close-button-hover-opacity)
    (cons "--we-close-button-radius" close-button-radius)
    (cons "--we-alert-primary-bg" alert-primary-bg)
    (cons "--we-alert-primary-border" alert-primary-border)
    (cons "--we-alert-primary-color" alert-primary-color)
    (cons "--we-alert-secondary-bg" alert-secondary-bg)
    (cons "--we-alert-secondary-border" alert-secondary-border)
    (cons "--we-alert-secondary-color" alert-secondary-color)
    (cons "--we-alert-success-bg" alert-success-bg)
    (cons "--we-alert-success-border" alert-success-border)
    (cons "--we-alert-success-color" alert-success-color)
    (cons "--we-alert-info-bg" alert-info-bg)
    (cons "--we-alert-info-border" alert-info-border)
    (cons "--we-alert-info-color" alert-info-color)
    (cons "--we-alert-warning-bg" alert-warning-bg)
    (cons "--we-alert-warning-border" alert-warning-border)
    (cons "--we-alert-warning-color" alert-warning-color)
    (cons "--we-alert-danger-bg" alert-danger-bg)
    (cons "--we-alert-danger-border" alert-danger-border)
    (cons "--we-alert-danger-color" alert-danger-color)
    (cons "--we-alert-light-bg" alert-light-bg)
    (cons "--we-alert-light-border" alert-light-border)
    (cons "--we-alert-light-color" alert-light-color)
    (cons "--we-alert-dark-bg" alert-dark-bg)
    (cons "--we-alert-dark-border" alert-dark-border)
    (cons "--we-alert-dark-color" alert-dark-color)
    (cons "--we-list-group-bg" list-group-bg)
    (cons "--we-list-group-border-color" list-group-border-color)
    (cons "--we-list-group-border-width" list-group-border-width)
    (cons "--we-list-group-radius" list-group-radius)
    (cons "--we-list-group-item-radius" list-group-item-radius)
    (cons "--we-list-group-padding-x" list-group-padding-x)
    (cons "--we-list-group-padding-y" list-group-padding-y)
    (cons "--we-list-group-color" list-group-color)
    (cons "--we-list-group-active-bg" list-group-active-bg)
    (cons "--we-list-group-active-color" list-group-active-color)
    (cons "--we-list-group-active-border-color" list-group-active-border)
    (cons "--we-table-bg" table-bg)
    (cons "--we-table-border-color" table-border-color)
    (cons "--we-table-color" table-color)
    (cons "--we-table-striped-bg" table-striped-bg)
    (cons "--we-table-hover-bg" table-hover-bg)
    (cons "--we-table-active-bg" table-active-bg)
    (cons "--we-table-cell-padding-x" table-cell-padding-x)
    (cons "--we-table-cell-padding-y" table-cell-padding-y)
    (cons "--we-table-border-width" table-border-width)
    (cons "--we-table-header-color" table-header-color)
    (cons "--we-table-header-font-size" table-header-font-size)
    (cons "--we-table-header-font-weight" table-header-font-weight)
    (cons "--we-table-header-text-transform" table-header-text-transform)
    (cons "--we-table-body-font-weight" table-body-font-weight)
    (cons "--we-field-bg" field-bg)
    (cons "--we-field-color" field-color)
    (cons "--we-field-border-color" field-border-color)
    (cons "--we-field-border-width" field-border-width)
    (cons "--we-field-radius" field-radius)
    (cons "--we-field-padding-x" field-padding-x)
    (cons "--we-field-padding-y" field-padding-y)
    (cons "--we-field-font-size" field-font-size)
    (cons "--we-field-font-weight" field-font-weight)
    (cons "--we-field-select-icon" (or (theme-component-ref theme 'field-select-icon) "none"))
    (cons "--we-field-select-padding-right"
          (or (theme-component-ref theme 'field-select-padding-right)
              "calc(var(--we-field-padding-x) * 3)"))
    (cons "--we-field-focus-bg" field-focus-bg)
    (cons "--we-field-focus-border-color" field-focus-border-color)
    (cons "--we-input-group-addon-bg" input-group-addon-bg)
    (cons "--we-input-group-addon-color" input-group-addon-color)
    (cons "--we-input-group-addon-border-color" input-group-addon-border-color)
    (cons "--we-input-group-addon-padding-x" input-group-addon-padding-x)
    (cons "--we-input-group-addon-padding-y" input-group-addon-padding-y)
    (cons "--we-input-group-addon-font-size" input-group-addon-font-size)
    (cons "--we-input-group-addon-font-weight" input-group-addon-font-weight)
    (cons "--we-field-valid-color" field-valid-color)
    (cons "--we-field-valid-border-color" field-valid-border-color)
    (cons "--we-field-valid-icon" field-valid-icon)
    (cons "--we-field-invalid-color" field-invalid-color)
    (cons "--we-field-invalid-border-color" field-invalid-border-color)
    (cons "--we-field-invalid-icon" field-invalid-icon)
    (cons "--we-check-bg" check-bg)
    (cons "--we-check-border-color" check-border-color)
    (cons "--we-check-border-width" check-border-width)
    (cons "--we-check-size" check-size)
    (cons "--we-check-height" check-height)
    (cons "--we-check-radius" check-radius)
    (cons "--we-check-focus-border-color" check-focus-border-color)
    (cons "--we-check-focus-shadow" check-focus-shadow)
    (cons "--we-check-checked-bg" check-checked-bg)
    (cons "--we-check-checked-border-color" check-checked-border-color)
    (cons "--we-check-checked-icon" check-checked-icon)
    (cons "--we-check-disabled-opacity" check-disabled-opacity)
    (cons "--we-switch-width" switch-width)
    (cons "--we-switch-radius" switch-radius)
    (cons "--we-switch-bg-icon" switch-bg-icon)
    (cons "--we-switch-focus-icon" switch-focus-icon)
    (cons "--we-switch-checked-icon" switch-checked-icon)
    (cons "--we-dialog-bg" dialog-bg)
    (cons "--we-dialog-color" dialog-color)
    (cons "--we-dialog-border-color" dialog-border-color)
    (cons "--we-dialog-border-width" dialog-border-width)
    (cons "--we-dialog-radius" dialog-radius)
    (cons "--we-dialog-shadow" dialog-shadow)
    (cons "--we-dialog-header-padding-x" dialog-header-padding-x)
    (cons "--we-dialog-header-padding-y" dialog-header-padding-y)
    (cons "--we-dialog-header-border-color" dialog-header-border-color)
    (cons "--we-dialog-header-border-width" dialog-header-border-width)
    (cons "--we-dialog-body-padding-x" dialog-body-padding-x)
    (cons "--we-dialog-body-padding-y" dialog-body-padding-y)
    (cons "--we-dialog-footer-padding-x" dialog-footer-padding-x)
    (cons "--we-dialog-footer-padding-y" dialog-footer-padding-y)
    (cons "--we-dialog-footer-gap" dialog-footer-gap)
    (cons "--we-dialog-footer-border-color" dialog-footer-border-color)
    (cons "--we-dialog-footer-border-width" dialog-footer-border-width)
    (cons "--we-dialog-title-color" dialog-title-color)
    (cons "--we-dialog-title-font-size" dialog-title-font-size)
    (cons "--we-dialog-title-font-weight" dialog-title-font-weight)
    (cons "--we-dialog-title-text-transform" dialog-title-text-transform)
    (cons "--we-dialog-close-icon" dialog-close-icon)
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
   "  font-weight: var(--we-font-weight-normal);\n"
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
  (define rules (read-css-rules source))
  (define import-lines (extract-top-level-imports/rules rules))
  (define theme (bootstrap-rules->theme-model rules selector))
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

  (define nav-bootstrap
    (string-append
     ":root {\n"
     "  --bs-body-color: #55595c;\n"
     "  --bs-body-bg: #ffffff;\n"
     "  --bs-border-color: #e0e1e2;\n"
     "  --bs-primary: #1a1a1a;\n"
     "  --bs-secondary-bg: #f8f9fa;\n"
     "  --bs-tertiary-bg: #f8f9fa;\n"
     "}\n"
     ".nav {\n"
     "  --bs-nav-link-padding-x: 1rem;\n"
     "  --bs-nav-link-padding-y: 0.5rem;\n"
     "  --bs-nav-link-color: #55595c;\n"
     "  --bs-nav-link-hover-color: #1a1a1a;\n"
     "}\n"
     ".nav-tabs {\n"
     "  --bs-nav-tabs-border-color: #e0e1e2;\n"
     "  --bs-nav-tabs-link-hover-border-color: #f0f1f2 #f0f1f2 #e0e1e2;\n"
     "  --bs-nav-tabs-link-active-color: #000;\n"
     "  --bs-nav-tabs-link-active-bg: #fff;\n"
     "  --bs-nav-tabs-link-active-border-color: #e0e1e2 #e0e1e2 #fff;\n"
     "}\n"
     ".navbar {\n"
     "  --bs-navbar-color: rgba(0, 0, 0, 0.3);\n"
     "  --bs-navbar-hover-color: #1a1a1a;\n"
     "  --bs-navbar-active-color: #1a1a1a;\n"
     "  --bs-navbar-brand-color: #1a1a1a;\n"
     "  --bs-navbar-brand-hover-color: #1a1a1a;\n"
     "  --bs-navbar-brand-font-size: 1.25rem;\n"
     "  --bs-navbar-brand-margin-end: 1rem;\n"
     "  font-size: 0.875rem;\n"
     "  font-weight: 600;\n"
     "  text-transform: uppercase;\n"
     "}\n"
     ".nav-tabs .nav-link {\n"
     "  background-color: rgba(237, 238, 239, 0.977);\n"
     "}\n"))

  (define overlay-bootstrap
    (string-append
     ":root {\n"
     "  --bs-body-color: #55595c;\n"
     "  --bs-body-bg: #ffffff;\n"
     "  --bs-border-color: #e0e1e2;\n"
     "}\n"
     ".dropdown-menu {\n"
     "  --bs-dropdown-bg: #ffffff;\n"
     "  --bs-dropdown-color: #55595c;\n"
     "  --bs-dropdown-border-color: rgba(0, 0, 0, 0.15);\n"
     "  --bs-dropdown-border-width: 0;\n"
     "  --bs-dropdown-border-radius: 0;\n"
     "  --bs-dropdown-box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.175);\n"
     "  --bs-dropdown-padding-y: 0.5rem;\n"
     "  --bs-dropdown-header-color: #adb5bd;\n"
     "  --bs-dropdown-header-padding-x: 1rem;\n"
     "  --bs-dropdown-header-padding-y: 0.5rem;\n"
     "  --bs-dropdown-divider-bg: #e9ecef;\n"
     "  --bs-dropdown-item-padding-x: 1rem;\n"
     "  --bs-dropdown-item-padding-y: 0.25rem;\n"
     "  --bs-dropdown-link-active-bg: #1a1a1a;\n"
     "  --bs-dropdown-link-active-color: #ffffff;\n"
     "}\n"
     ".dropdown-item {\n"
     "  color: #55595c;\n"
     "  font-size: 1rem;\n"
     "  font-weight: 400;\n"
     "  text-transform: uppercase;\n"
     "}\n"
     ".dropdown-item:hover {\n"
     "  color: #1a1a1a;\n"
     "  background-color: #f0f1f2;\n"
     "}\n"
     ".tooltip {\n"
     "  --bs-tooltip-bg: #1a1a1a;\n"
     "  --bs-tooltip-color: #ffffff;\n"
     "  --bs-tooltip-font-size: 0.875rem;\n"
     "  --bs-tooltip-padding-x: 0.75rem;\n"
     "  --bs-tooltip-padding-y: 0.35rem;\n"
     "  --bs-tooltip-border-radius: 0;\n"
     "  --bs-tooltip-opacity: 1;\n"
     "}\n"
     ".popover {\n"
     "  --bs-popover-bg: #ffffff;\n"
     "  --bs-popover-max-width: 276px;\n"
     "  --bs-popover-font-size: 0.875rem;\n"
     "  --bs-popover-body-color: #55595c;\n"
     "  --bs-popover-border-color: rgba(0, 0, 0, 0.2);\n"
     "  --bs-popover-border-width: 0;\n"
     "  --bs-popover-border-radius: 0;\n"
     "  --bs-popover-box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.15);\n"
     "  --bs-popover-arrow-width: 1rem;\n"
     "  --bs-popover-arrow-height: 0.5rem;\n"
     "  --bs-popover-header-bg: #f7f7f7;\n"
     "  --bs-popover-header-color: #1a1a1a;\n"
     "  --bs-popover-header-padding-x: 1rem;\n"
     "  --bs-popover-header-padding-y: 0.5rem;\n"
     "  --bs-popover-body-padding-x: 1rem;\n"
     "  --bs-popover-body-padding-y: 0.5rem;\n"
     "}\n"
     ".popover-header {\n"
     "  font-size: 1rem;\n"
     "  font-weight: 600;\n"
     "}\n"))

  (define button-card-bootstrap
    (string-append
     ":root {\n"
     "  --bs-body-color: #55595c;\n"
     "  --bs-body-bg: #ffffff;\n"
     "  --bs-border-color: #e0e1e2;\n"
     "  --bs-primary: #1a1a1a;\n"
     "  --bs-secondary: #fff;\n"
     "  --bs-light: #f0f1f2;\n"
     "}\n"
     ".btn {\n"
      "  --bs-btn-padding-x: 1.5rem;\n"
      "  --bs-btn-padding-y: 0.75rem;\n"
      "  --bs-btn-font-size: 1rem;\n"
      "  --bs-btn-font-weight: 600;\n"
      "  --bs-btn-line-height: 1.5rem;\n"
      "  --bs-btn-border-radius: 0.375rem;\n"
      "  text-transform: uppercase;\n"
      "}\n"
     "button {\n"
     "  border-radius: 0;\n"
     "}\n"
     ".btn-secondary {\n"
     "  --bs-btn-color: #000;\n"
     "  --bs-btn-bg: #fff;\n"
     "  --bs-btn-border-color: #fff;\n"
     "  --bs-btn-hover-bg: #f0f1f2;\n"
     "  --bs-btn-hover-border-color: #f0f1f2;\n"
     "}\n"
     ".btn-light {\n"
     "  --bs-btn-color: #000;\n"
     "  --bs-btn-bg: #f0f1f2;\n"
     "  --bs-btn-border-color: #f0f1f2;\n"
     "  --bs-btn-hover-bg: #e0e1e2;\n"
     "  --bs-btn-hover-border-color: #e0e1e2;\n"
     "}\n"
     ".btn-outline-secondary {\n"
     "  --bs-btn-color: #000;\n"
     "  --bs-btn-border-color: #000;\n"
     "  --bs-btn-hover-color: #000;\n"
     "  --bs-btn-hover-bg: #f0f1f2;\n"
     "  --bs-btn-hover-border-color: #f0f1f2;\n"
     "}\n"
     ".btn-success {\n"
     "  --bs-btn-color: #fff;\n"
     "}\n"
     ".btn-info {\n"
     "  --bs-btn-color: #fff;\n"
     "}\n"
     ".btn-warning {\n"
     "  --bs-btn-color: #fff;\n"
     "}\n"
     ".btn-danger {\n"
     "  --bs-btn-color: #fff;\n"
     "}\n"
     ".card {\n"
      "  --bs-card-border-color: rgba(0, 0, 0, 0.125);\n"
      "  --bs-card-cap-padding-y: 0.5rem;\n"
      "  --bs-card-cap-padding-x: 1rem;\n"
      "  --bs-card-cap-bg: rgba(85, 89, 92, 0.03);\n"
      "  --bs-card-bg: #fff;\n"
      "  --bs-card-spacer-x: 1rem;\n"
      "  --bs-card-spacer-y: 1rem;\n"
      "}\n"
     ".badge {\n"
     "  --bs-badge-padding-x: 0.65em;\n"
     "  --bs-badge-padding-y: 0.35em;\n"
     "  --bs-badge-font-size: 0.75em;\n"
     "  --bs-badge-font-weight: 600;\n"
     "  --bs-badge-border-radius: 0.375rem;\n"
      "  --bs-badge-color: #fff;\n"
     "  color: #55595c;\n"
     "}\n"
     ".badge.bg-secondary, .badge.bg-light {\n"
     "  color: #343a40;\n"
     "}\n"))

  (define sandstone-button-bootstrap
    (string-append
     ":root {\n"
     "  --bs-body-color: #3e3f3a;\n"
     "  --bs-body-bg: #ffffff;\n"
     "}\n"
     ".btn {\n"
     "  --bs-btn-padding-x: 0.75rem;\n"
     "  --bs-btn-padding-y: 0.375rem;\n"
     "  --bs-btn-font-size: 1rem;\n"
     "  --bs-btn-font-weight: 400;\n"
     "  --bs-btn-line-height: 1.5;\n"
     "  --bs-btn-border-radius: 0.375rem;\n"
     "}\n"
     ".sandstone, .btn {\n"
     "  font-size: 13px;\n"
     "  font-weight: 500;\n"
     "  line-height: 22px;\n"
     "  text-transform: uppercase;\n"
     "}\n"))

  (define list-table-bootstrap
    (string-append
     ":root {\n"
     "  --bs-body-color: #55595c;\n"
     "  --bs-body-bg: #ffffff;\n"
     "  --bs-body-font-weight: 300;\n"
     "  --bs-border-color: #e0e1e2;\n"
     "  --bs-secondary-color: rgba(85, 89, 92, 0.75);\n"
     "  --bs-emphasis-color: #1a1a1a;\n"
     "  --bs-secondary-bg: #f0f1f2;\n"
     "  --bs-tertiary-bg: #f8f9fa;\n"
     "}\n"
     ".list-group {\n"
     "  --bs-list-group-bg: var(--bs-body-bg);\n"
     "  --bs-list-group-border-color: var(--bs-border-color);\n"
     "  --bs-list-group-item-padding-x: 1rem;\n"
     "  --bs-list-group-item-padding-y: 0.5rem;\n"
     "  --bs-list-group-color: var(--bs-body-color);\n"
     "  --bs-list-group-active-color: #fff;\n"
     "  --bs-list-group-active-bg: #1a1a1a;\n"
     "  --bs-list-group-active-border-color: #1a1a1a;\n"
     "}\n"
     ".table {\n"
     "  --bs-table-bg: var(--bs-body-bg);\n"
     "  --bs-table-border-color: rgba(0, 0, 0, 0.05);\n"
     "  --bs-table-striped-bg: rgba(26, 26, 26, 0.05);\n"
     "  --bs-table-active-bg: rgba(26, 26, 26, 0.1);\n"
     "  --bs-table-hover-bg: rgba(26, 26, 26, 0.075);\n"
     "}\n"
     ".table th,\n"
     ".table td {\n"
     "  padding: 1.5rem;\n"
     "}\n"
     "th {\n"
     "  font-size: 0.875rem;\n"
     "  text-transform: uppercase;\n"
     "}\n"))

  (define forms-modal-bootstrap
    (string-append
     ":root {\n"
     "  --bs-body-color: #55595c;\n"
     "  --bs-body-bg: #ffffff;\n"
     "  --bs-body-font-weight: 300;\n"
     "  --bs-border-color: #e0e1e2;\n"
     "}\n"
     ".form-control {\n"
     "  color: #55595c;\n"
     "  background-color: #f0f1f2;\n"
     "  border: 0 solid #e0e1e2;\n"
     "  border-radius: 0;\n"
     "  padding: 0.75rem 1.5rem;\n"
     "  font-size: 1rem;\n"
     "  font-weight: 300;\n"
     "}\n"
     ".form-control:focus {\n"
     "  background-color: #f0f1f2;\n"
     "  border-color: #1a1a1a;\n"
     "}\n"
     ".form-select {\n"
     "  --bs-form-select-bg-img: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='none' stroke='%2355595c' stroke-linecap='round' stroke-linejoin='round'%3e%3cpath d='m2 5 6 6 6-6'/%3e%3c/svg%3e\");\n"
     "  color: #55595c;\n"
     "  background-color: #f0f1f2;\n"
     "  border: 0 solid #e0e1e2;\n"
     "  border-radius: 0;\n"
     "  padding: 0.75rem 4.5rem 0.75rem 1.5rem;\n"
     "  font-size: 1rem;\n"
     "  font-weight: 300;\n"
     "}\n"
     ".input-group-text {\n"
     "  padding: 0.75rem 1.5rem;\n"
     "  font-size: 1rem;\n"
     "  font-weight: 300;\n"
     "  color: #55595c;\n"
     "  background-color: #e0e1e2;\n"
     "  border: 0 solid #e0e1e2;\n"
     "}\n"
     ".form-check-input {\n"
     "  --bs-form-check-bg: #f0f1f2;\n"
     "  width: 1em;\n"
     "  height: 1em;\n"
     "  background-color: var(--bs-form-check-bg);\n"
     "  border: 1px solid #e0e1e2;\n"
     "}\n"
     ".form-check-input:focus {\n"
     "  border-color: #8d8d8d;\n"
     "  box-shadow: 0 0 0 0.25rem rgba(26, 26, 26, 0.25);\n"
     "}\n"
     ".form-check-input:checked {\n"
     "  background-color: #1a1a1a;\n"
     "  border-color: #1a1a1a;\n"
     "}\n"
     ".form-check-input:checked[type=checkbox] {\n"
     "  --bs-form-check-bg-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 20 20'%3e%3cpath fill='none' stroke='%23fff' stroke-linecap='round' stroke-linejoin='round' stroke-width='3' d='m6 10 3 3 6-6'/%3e%3c/svg%3e\");\n"
     "}\n"
     ".form-check-input:disabled {\n"
     "  opacity: 0.5;\n"
     "}\n"
     ".form-switch .form-check-input {\n"
     "  --bs-form-switch-bg: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-4 -4 8 8'%3e%3ccircle r='3' fill='rgba%280, 0, 0, 0.25%29'/%3e%3c/svg%3e\");\n"
     "  width: 2em;\n"
     "  background-image: var(--bs-form-switch-bg);\n"
     "  background-position: left center;\n"
     "  border-radius: 0;\n"
     "}\n"
     ".form-switch .form-check-input:focus {\n"
     "  --bs-form-switch-bg: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-4 -4 8 8'%3e%3ccircle r='3' fill='%238d8d8d'/%3e%3c/svg%3e\");\n"
     "}\n"
     ".form-switch .form-check-input:checked {\n"
     "  --bs-form-switch-bg: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-4 -4 8 8'%3e%3ccircle r='3' fill='%23fff'/%3e%3c/svg%3e\");\n"
     "}\n"
     ".modal-content {\n"
     "  color: #55595c;\n"
     "  background-color: #ffffff;\n"
     "  border: 1px solid rgba(0, 0, 0, 0.2);\n"
     "  border-radius: 0;\n"
     "  box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.15);\n"
     "}\n"
     ".modal-header {\n"
     "  padding: 1rem 1rem;\n"
     "  border-bottom: 1px solid #e0e1e2;\n"
     "}\n"
     ".modal-body {\n"
     "  padding: 1rem;\n"
     "}\n"
     ".modal-footer {\n"
     "  padding: 1rem;\n"
     "  border-top: 1px solid #e0e1e2;\n"
     "}\n"
     "h5 {\n"
     "  font-weight: 600;\n"
     "  text-transform: uppercase;\n"
     "}\n"
     ".modal-title {\n"
     "  color: #1a1a1a;\n"
     "  font-size: 1rem;\n"
     "}\n"
     ".btn-close {\n"
     "  --bs-btn-close-color: #000;\n"
     "  --bs-btn-close-bg: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='%23000'%3e%3cpath d='M.293.293a1 1 0 0 1 1.414 0L8 6.586 14.293.293a1 1 0 1 1 1.414 1.414L9.414 8l6.293 6.293a1 1 0 0 1-1.414 1.414L8 9.414l-6.293 6.293a1 1 0 0 1-1.414-1.414L6.586 8 .293 1.707a1 1 0 0 1 0-1.414'/%3e%3c/svg%3e\");\n"
     "  --bs-btn-close-opacity: 0.5;\n"
     "  --bs-btn-close-hover-opacity: 0.75;\n"
     "}\n"
     ".modal .btn-close {\n"
     "  background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='%2355595c'%3e%3cpath d='M.293.293a1 1 0 0 1 1.414 0L8 6.586 14.293.293a1 1 0 1 1 1.414 1.414L9.414 8l6.293 6.293a1 1 0 0 1-1.414 1.414L8 9.414l-6.293 6.293a1 1 0 0 1-1.414-1.414L6.586 8 .293 1.707a1 1 0 0 1 0-1.414'/%3e%3c/svg%3e\");\n"
     "}\n"
     ".valid-feedback {\n"
     "  color: var(--bs-form-valid-color);\n"
     "}\n"
     ".invalid-feedback {\n"
     "  color: var(--bs-form-invalid-color);\n"
     "}\n"
     ".form-control.is-valid {\n"
     "  border-color: var(--bs-form-valid-border-color);\n"
     "  background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 8 8'%3e%3cpath fill='%234bbf73' d='M2.3 6.73.6 4.53c-.4-1.04.46-1.4 1.1-.8l1.1 1.4 3.4-3.8c.6-.63 1.6-.27 1.2.7l-4 4.6c-.43.5-.8.4-1.1.1'/%3e%3c/svg%3e\");\n"
     "}\n"
     ".form-control.is-invalid {\n"
     "  border-color: var(--bs-form-invalid-border-color);\n"
     "  background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 12 12' width='12' height='12' fill='none' stroke='%23d9534f'%3e%3ccircle cx='6' cy='6' r='4.5'/%3e%3cpath stroke-linejoin='round' d='M5.8 3.6h.4L6 6.5z'/%3e%3ccircle cx='6' cy='8.2' r='.6' fill='%23d9534f' stroke='none'/%3e%3c/svg%3e\");\n"
     "}\n"))

  (define accordion-bootstrap
    (string-append
     ":root {\n"
     "  --bs-body-color: #55595c;\n"
     "  --bs-body-bg: #ffffff;\n"
     "  --bs-border-color: #e0e1e2;\n"
     "  --bs-primary-text-emphasis: #0a0a0a;\n"
     "  --bs-primary-bg-subtle: #d1d1d1;\n"
     "}\n"
     ".accordion {\n"
     "  --bs-accordion-bg: var(--bs-body-bg);\n"
     "  --bs-accordion-border-color: var(--bs-border-color);\n"
     "  --bs-accordion-border-width: 1px;\n"
     "  --bs-accordion-border-radius: 0;\n"
     "  --bs-accordion-btn-padding-x: 1.25rem;\n"
     "  --bs-accordion-btn-padding-y: 1rem;\n"
     "  --bs-accordion-btn-color: var(--bs-body-color);\n"
     "  --bs-accordion-btn-bg: var(--bs-accordion-bg);\n"
     "  --bs-accordion-btn-icon: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='none' stroke='%2355595c' stroke-linecap='round' stroke-linejoin='round'%3e%3cpath d='m2 5 6 6 6-6'/%3e%3c/svg%3e\");\n"
     "  --bs-accordion-btn-active-icon: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='none' stroke='%230a0a0a' stroke-linecap='round' stroke-linejoin='round'%3e%3cpath d='m2 5 6 6 6-6'/%3e%3c/svg%3e\");\n"
     "  --bs-accordion-btn-icon-width: 1.25rem;\n"
     "  --bs-accordion-btn-icon-transform: rotate(-180deg);\n"
     "  --bs-accordion-body-padding-x: 1.25rem;\n"
     "  --bs-accordion-body-padding-y: 1rem;\n"
     "  --bs-accordion-active-color: var(--bs-primary-text-emphasis);\n"
     "  --bs-accordion-active-bg: var(--bs-primary-bg-subtle);\n"
     "}\n"
     ".accordion-button {\n"
     "  padding: var(--bs-accordion-btn-padding-y) var(--bs-accordion-btn-padding-x);\n"
     "  color: var(--bs-accordion-btn-color);\n"
     "  background-color: var(--bs-accordion-btn-bg);\n"
     "}\n"
     ".accordion-button:not(.collapsed) {\n"
     "  color: var(--bs-accordion-active-color);\n"
     "  background-color: var(--bs-accordion-active-bg);\n"
     "}\n"
     ".accordion-item {\n"
     "  background-color: var(--bs-accordion-bg);\n"
     "  border: var(--bs-accordion-border-width) solid var(--bs-accordion-border-color);\n"
     "}\n"
     ".accordion-body {\n"
     "  padding: var(--bs-accordion-body-padding-y) var(--bs-accordion-body-padding-x);\n"
     "}\n"))

  (define pagination-progress-bootstrap
    (string-append
     ":root {\n"
     "  --bs-body-color: #55595c;\n"
     "  --bs-body-bg: #ffffff;\n"
     "  --bs-border-color: #e0e1e2;\n"
     "  --bs-link-color: rgba(85, 89, 92, 0.75);\n"
     "  --bs-link-hover-color: #1a1a1a;\n"
     "  --bs-secondary-color: rgba(85, 89, 92, 0.75);\n"
     "  --bs-secondary-bg: #f0f1f2;\n"
     "}\n"
     ".pagination {\n"
     "  --bs-pagination-padding-x: 0.75rem;\n"
     "  --bs-pagination-padding-y: 0.375rem;\n"
     "  --bs-pagination-font-size: 1rem;\n"
     "  --bs-pagination-color: var(--bs-link-color);\n"
     "  --bs-pagination-bg: var(--bs-body-bg);\n"
     "  --bs-pagination-border-width: 0;\n"
     "  --bs-pagination-border-color: transparent;\n"
     "  --bs-pagination-border-radius: 0;\n"
     "  --bs-pagination-hover-color: var(--bs-link-hover-color);\n"
     "  --bs-pagination-hover-bg: var(--bs-secondary-bg);\n"
     "  --bs-pagination-hover-border-color: transparent;\n"
     "  --bs-pagination-active-color: #fff;\n"
     "  --bs-pagination-active-bg: #1a1a1a;\n"
     "  --bs-pagination-active-border-color: #1a1a1a;\n"
     "  --bs-pagination-disabled-color: var(--bs-secondary-color);\n"
     "  --bs-pagination-disabled-bg: var(--bs-secondary-bg);\n"
     "  --bs-pagination-disabled-border-color: transparent;\n"
     "}\n"
     ".progress, .progress-stacked {\n"
     "  --bs-progress-height: 1rem;\n"
     "  --bs-progress-font-size: 0.75rem;\n"
     "  --bs-progress-bg: var(--bs-secondary-bg);\n"
     "  --bs-progress-border-radius: 0;\n"
     "  --bs-progress-box-shadow: none;\n"
     "  --bs-progress-bar-color: #fff;\n"
     "  --bs-progress-bar-bg: #1a1a1a;\n"
     "}\n"))

  (define breadcrumb-toast-bootstrap
    (string-append
     ":root {\n"
     "  --bs-body-color: #55595c;\n"
     "  --bs-body-bg: #ffffff;\n"
     "  --bs-border-color-translucent: rgba(0, 0, 0, 0.175);\n"
     "  --bs-link-color: #1a1a1a;\n"
     "  --bs-secondary-color: rgba(85, 89, 92, 0.75);\n"
     "}\n"
     ".breadcrumb {\n"
     "  --bs-breadcrumb-divider-color: var(--bs-secondary-color);\n"
     "  --bs-breadcrumb-item-padding-x: 0.5rem;\n"
     "  --bs-breadcrumb-item-active-color: var(--bs-secondary-color);\n"
     "}\n"
     ".breadcrumb-item a {\n"
     "  color: #1a1a1a;\n"
     "}\n"
     ".toast {\n"
     "  --bs-toast-padding-x: 0.75rem;\n"
     "  --bs-toast-padding-y: 0.5rem;\n"
     "  --bs-toast-font-size: 0.875rem;\n"
     "  --bs-toast-bg: rgba(255, 255, 255, 0.85);\n"
     "  --bs-toast-border-width: 0;\n"
     "  --bs-toast-border-color: rgba(0, 0, 0, 0.175);\n"
     "  --bs-toast-box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.15);\n"
     "  --bs-toast-header-color: rgba(85, 89, 92, 0.75);\n"
     "  --bs-toast-header-bg: transparent;\n"
     "  --bs-toast-header-border-color: rgba(0, 0, 0, 0.175);\n"
     "}\n"
     ".toast-body {\n"
     "  padding: 0.75rem;\n"
     "}\n"
     ".btn-close {\n"
     "  --bs-btn-close-color: #000;\n"
     "  --bs-btn-close-bg: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='%23000'%3e%3cpath d='M.293.293a1 1 0 0 1 1.414 0L8 6.586 14.293.293a1 1 0 1 1 1.414 1.414L9.414 8l6.293 6.293a1 1 0 0 1-1.414 1.414L8 9.414l-6.293 6.293a1 1 0 0 1-1.414-1.414L6.586 8 .293 1.707a1 1 0 0 1 0-1.414'/%3e%3c/svg%3e\");\n"
     "  --bs-btn-close-opacity: 0.5;\n"
     "  --bs-btn-close-hover-opacity: 0.75;\n"
     "}\n"
     ".modal .btn-close {\n"
     "  background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='%2355595c'%3e%3cpath d='M.293.293a1 1 0 0 1 1.414 0L8 6.586 14.293.293a1 1 0 1 1 1.414 1.414L9.414 8l6.293 6.293a1 1 0 0 1-1.414 1.414L8 9.414l-6.293 6.293a1 1 0 0 1-1.414-1.414L6.586 8 .293 1.707a1 1 0 0 1 0-1.414'/%3e%3c/svg%3e\");\n"
     "}\n"))

  (test-case "read-css-rules parses top-level declarations"
    (define rules (read-css-rules sample-bootstrap))
    (define style-rules
      (filter css-style-rule? (css-stylesheet-rules rules)))
    (check-equal? (length style-rules) 3)
    (check-equal? (css-style-rule-raw-selector (first style-rules)) ":root")
    (check-equal? (css-declaration-name
                   (first (filter css-declaration?
                                  (css-style-rule-block
                                   (first style-rules)))))
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

  (test-case "bootstrap mapping keeps body font weight when present"
    (define theme
      (bootstrap-props->theme-model
       (extract-bootstrap-theme
        ":root { --bs-body-color: #111; --bs-body-bg: #fff; --bs-border-color: #ddd; --bs-primary: #0d6efd; --bs-body-font-weight: 300; }"
        ":root")))
    (check-equal? (bootstrap-theme-font-weight-normal theme)
                  "300"))

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
    (define plain-map (theme->token-map theme))
    (define boosted-map (theme->token-map theme #t))
    (check-not-equal? (hash-ref plain-map "--we-primary-subtle")
                      (hash-ref boosted-map "--we-primary-subtle"))
    (check-not-equal? (hash-ref plain-map "--we-primary-border")
                      (hash-ref boosted-map "--we-primary-border")))

  (test-case "component rule extraction feeds navbar and tab tokens"
    (define rules (read-css-rules nav-bootstrap))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-navbar-link-color")
                  "rgba(0, 0, 0, 0.3)")
    (check-equal? (hash-ref token-map "--we-nav-text-transform")
                  "uppercase")
    (check-equal? (hash-ref token-map "--we-tab-active-border-color")
                  "#e0e1e2 #e0e1e2 #fff"))

  (test-case "component rule extraction feeds button card and badge tokens"
    (define rules (read-css-rules button-card-bootstrap))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-button-padding-x") "1.5rem")
    (check-equal? (hash-ref token-map "--we-button-text-transform") "uppercase")
    (check-equal? (hash-ref token-map "--we-button-radius") "0")
    (check-equal? (hash-ref token-map "--we-button-secondary-bg") "#fff")
    (check-equal? (hash-ref token-map "--we-button-success-color") "#fff")
    (check-equal? (hash-ref token-map "--we-button-danger-color") "#fff")
    (check-equal? (hash-ref token-map "--we-card-radius") "0")
    (check-equal? (hash-ref token-map "--we-card-cap-bg") "rgba(85, 89, 92, 0.03)")
    (check-equal? (hash-ref token-map "--we-badge-font-size") "0.75em")
    (check-equal? (hash-ref token-map "--we-badge-color") "#fff")
    (check-equal? (hash-ref token-map "--we-badge-radius") "0")
    (check-equal? (hash-ref token-map "--we-badge-secondary-color") "#343a40")
    (check-equal? (hash-ref token-map "--we-badge-light-color") "#343a40"))

  (test-case "late .btn selector typography overrides button token defaults"
    (define rules (read-css-rules sandstone-button-bootstrap))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-button-font-size") "13px")
    (check-equal? (hash-ref token-map "--we-button-font-weight") "500")
    (check-equal? (hash-ref token-map "--we-button-line-height") "22px")
    (check-equal? (hash-ref token-map "--we-button-text-transform") "uppercase"))

  (test-case "component rule extraction feeds list group and table tokens"
    (define rules (read-css-rules list-table-bootstrap))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-list-group-active-bg") "#1a1a1a")
    (check-equal? (hash-ref token-map "--we-list-group-padding-x") "1rem")
    (check-equal? (hash-ref token-map "--we-table-border-color") "rgba(0, 0, 0, 0.05)")
    (check-equal? (hash-ref token-map "--we-table-cell-padding-x") "1.5rem")
    (check-equal? (hash-ref token-map "--we-table-header-text-transform") "uppercase")
    (check-equal? (hash-ref token-map "--we-table-body-font-weight") "300"))

  (test-case "component rule extraction feeds form and dialog tokens"
    (define rules (read-css-rules forms-modal-bootstrap))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-field-bg") "#f0f1f2")
    (check-equal? (hash-ref token-map "--we-input-group-addon-bg") "#e0e1e2")
    (check-equal? (hash-ref token-map "--we-input-group-addon-font-weight") "300")
    (check-equal? (hash-ref token-map "--we-field-border-width") "0")
    (check-equal? (hash-ref token-map "--we-field-padding-x") "1.5rem")
    (check-equal? (hash-ref token-map "--we-field-font-weight") "300")
    (check-equal? (hash-ref token-map "--we-field-valid-color") "#198754")
    (check-equal? (hash-ref token-map "--we-field-invalid-color") "#dc3545")
    (check-true (regexp-match? #px"^url\\(" (hash-ref token-map "--we-field-valid-icon")))
    (check-true (regexp-match? #px"^url\\(" (hash-ref token-map "--we-field-invalid-icon")))
    (check-true (regexp-match? #px"^url\\(" (hash-ref token-map "--we-field-select-icon")))
    (check-equal? (hash-ref token-map "--we-field-select-padding-right") "4.5rem")
    (check-equal? (hash-ref token-map "--we-check-size") "1em")
    (check-equal? (hash-ref token-map "--we-check-checked-bg") "#1a1a1a")
    (check-true (regexp-match? #px"^url\\(" (hash-ref token-map "--we-check-checked-icon")))
    (check-equal? (hash-ref token-map "--we-switch-width") "2em")
    (check-true (regexp-match? #px"^url\\(" (hash-ref token-map "--we-switch-bg-icon")))
    (check-equal? (hash-ref token-map "--we-dialog-border-color") "rgba(0, 0, 0, 0.2)")
    (check-equal? (hash-ref token-map "--we-dialog-header-padding-y") "1rem")
    (check-equal? (hash-ref token-map "--we-dialog-footer-gap") "0.5rem")
    (check-equal? (hash-ref token-map "--we-dialog-title-color") "#1a1a1a")
    (check-equal? (hash-ref token-map "--we-dialog-title-text-transform") "uppercase")
    (check-true (regexp-match? #px"^url\\(" (hash-ref token-map "--we-dialog-close-icon"))))

  (test-case "component rule extraction feeds accordion tokens"
    (define rules (read-css-rules accordion-bootstrap))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-accordion-bg") "#ffffff")
    (check-equal? (hash-ref token-map "--we-accordion-border-color") "#e0e1e2")
    (check-equal? (hash-ref token-map "--we-accordion-border-width") "1px")
    (check-equal? (hash-ref token-map "--we-accordion-radius") "0")
    (check-equal? (hash-ref token-map "--we-accordion-button-padding-x") "1.25rem")
    (check-equal? (hash-ref token-map "--we-accordion-button-padding-y") "1rem")
    (check-equal? (hash-ref token-map "--we-accordion-button-color") "#55595c")
    (check-equal? (hash-ref token-map "--we-accordion-icon-width") "1.25rem")
    (check-true (string-prefix? (hash-ref token-map "--we-accordion-icon") "url("))
    (check-equal? (hash-ref token-map "--we-accordion-icon-transform") "rotate(-180deg)")
    (check-equal? (hash-ref token-map "--we-accordion-active-bg") "#d1d1d1")
    (check-equal? (hash-ref token-map "--we-accordion-active-color") "#0a0a0a")
    (check-equal? (hash-ref token-map "--we-accordion-body-padding-x") "1.25rem")
    (check-equal? (hash-ref token-map "--we-accordion-body-padding-y") "1rem"))

  (test-case "component rule extraction feeds dropdown tooltip and popover tokens"
    (define rules (read-css-rules overlay-bootstrap))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-menu-popup-bg") "#ffffff")
    (check-equal? (hash-ref token-map "--we-menu-popup-border-width") "0")
    (check-equal? (hash-ref token-map "--we-menu-item-text-transform") "uppercase")
    (check-equal? (hash-ref token-map "--we-menu-item-active-bg") "#1a1a1a")
    (check-equal? (hash-ref token-map "--we-menu-item-active-fg") "#ffffff")
    (check-equal? (hash-ref token-map "--we-tooltip-bg") "#1a1a1a")
    (check-equal? (hash-ref token-map "--we-tooltip-color") "#ffffff")
    (check-equal? (hash-ref token-map "--we-tooltip-radius") "0")
    (check-equal? (hash-ref token-map "--we-popover-max-width") "276px")
    (check-equal? (hash-ref token-map "--we-popover-font-size") "0.875rem")
    (check-equal? (hash-ref token-map "--we-popover-arrow-width") "1rem")
    (check-equal? (hash-ref token-map "--we-popover-arrow-height") "0.5rem")
    (check-equal? (hash-ref token-map "--we-popover-header-bg") "#f7f7f7")
    (check-equal? (hash-ref token-map "--we-popover-body-padding-x") "1rem")
    (check-equal? (hash-ref token-map "--we-popover-title-font-weight") "600"))

  (test-case "descendant selectors feed overlay typography tokens"
    (define sandstone-overlay-bootstrap
      (string-append
       ":root {\n"
       "  --bs-body-color: #3e3f3a;\n"
       "  --bs-body-bg: #ffffff;\n"
       "  --bs-dropdown-bg: #ffffff;\n"
       "  --bs-dropdown-header-color: #8e8c84;\n"
       "}\n"
       ".sandstone, .tooltip, .dropdown-menu .dropdown-item {\n"
       "  font-size: 13px;\n"
       "  font-weight: 500;\n"
       "  line-height: 22px;\n"
       "  text-transform: uppercase;\n"
       "}\n"
       ".dropdown-header {\n"
       "  font-size: 0.875rem;\n"
       "  color: #8e8c84;\n"
       "}\n"
       ".tooltip {\n"
       "  --bs-tooltip-bg: #1a1a1a;\n"
       "  --bs-tooltip-color: #ffffff;\n"
       "}\n"))
    (define rules (read-css-rules sandstone-overlay-bootstrap))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-menu-item-font-size") "13px")
    (check-equal? (hash-ref token-map "--we-menu-item-font-weight") "500")
    (check-equal? (hash-ref token-map "--we-menu-item-text-transform") "uppercase")
    (check-equal? (hash-ref token-map "--we-tooltip-font-size") "13px"))

  (test-case "selector border radius overrides token border radius for overlays"
    (define sketchy-overlay-bootstrap
      (string-append
       ":root {\n"
       "  --bs-body-color: #333333;\n"
       "  --bs-body-bg: #ffffff;\n"
       "  --bs-border-radius: 25px;\n"
       "}\n"
       ".dropdown-menu {\n"
       "  --bs-dropdown-border-radius: var(--bs-border-radius);\n"
       "  border-radius: 555px 25px 25px 25px/25px 25px 25px 555px;\n"
       "}\n"
       ".tooltip {\n"
       "  --bs-tooltip-border-radius: var(--bs-border-radius);\n"
       "}\n"
       ".tooltip-inner {\n"
       "  border-radius: 255px 25px 225px 25px/25px 225px 25px 255px;\n"
       "}\n"
       ".popover {\n"
       "  --bs-popover-border-radius: 35px;\n"
       "  border-radius: 45px 85px 15px 25px/15px 10px 35px 555px;\n"
       "}\n"))
    (define rules (read-css-rules sketchy-overlay-bootstrap))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-menu-popup-radius")
                  "555px 25px 25px 25px/25px 25px 25px 555px")
    (check-equal? (hash-ref token-map "--we-tooltip-radius")
                  "255px 25px 225px 25px/25px 225px 25px 255px")
    (check-equal? (hash-ref token-map "--we-popover-radius")
                  "45px 85px 15px 25px/15px 10px 35px 555px"))

  (test-case "component rule extraction feeds pagination and progress tokens"
    (define rules (read-css-rules pagination-progress-bootstrap))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-pagination-padding-x") "0.75rem")
    (check-equal? (hash-ref token-map "--we-pagination-active-bg") "#1a1a1a")
    (check-equal? (hash-ref token-map "--we-pagination-disabled-bg") "#f0f1f2")
    (check-equal? (hash-ref token-map "--we-progress-height") "1rem")
    (check-equal? (hash-ref token-map "--we-progress-radius") "0")
    (check-equal? (hash-ref token-map "--we-progress-bar-bg") "#1a1a1a"))

  (test-case "component rule extraction preserves selector geometry"
    (define css
      (string-append
       ":root { --bs-border-radius: 25px; --bs-border-width: 2px; }\n"
       ".accordion { --bs-accordion-border-radius: var(--bs-border-radius);"
       " --bs-accordion-inner-border-radius: calc(var(--bs-border-radius) - (var(--bs-border-width))); }\n"
       ".pagination { --bs-pagination-border-radius: var(--bs-border-radius); }\n"
       ".pagination .page-link { border-radius: 425px 255px 25px 25px/25px 25px 5px 25px; }\n"
       ".list-group { border: 2px solid #333; border-radius: 45px 15px 35px 5px/15px 5px 15px 65px; }\n"
       ".list-group-item { border-radius: 255px 5px 225px 5px/25px 225px 25px 255px; }\n"))
    (define rules (read-css-rules css))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-accordion-radius") "25px")
    (check-equal? (hash-ref token-map "--we-accordion-trigger-radius-top")
                  "calc(25px - (2px))")
    (check-equal? (hash-ref token-map "--we-pagination-radius")
                  "425px 255px 25px 25px/25px 25px 5px 25px")
    (check-equal? (hash-ref token-map "--we-pagination-edge-radius") "25px")
    (check-equal? (hash-ref token-map "--we-list-group-border-width") "2px")
    (check-equal? (hash-ref token-map "--we-list-group-item-radius")
                  "255px 5px 225px 5px/25px 225px 25px 255px"))

  (test-case "component rule extraction feeds breadcrumb and toast tokens"
    (define rules (read-css-rules breadcrumb-toast-bootstrap))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-breadcrumb-link-color") "#1a1a1a")
    (check-equal? (hash-ref token-map "--we-breadcrumb-active-color") "rgba(85, 89, 92, 0.75)")
    (check-equal? (hash-ref token-map "--we-toast-font-size") "0.875rem")
    (check-equal? (hash-ref token-map "--we-toast-bg") "rgba(255, 255, 255, 0.85)")
    (check-equal? (hash-ref token-map "--we-toast-border-width") "0")
    (check-equal? (hash-ref token-map "--we-toast-body-padding") "0.75rem")
    (check-equal? (hash-ref token-map "--we-close-button-opacity") "0.5")
    (check-equal? (hash-ref token-map "--we-close-button-radius") "0"))

  (test-case "component rule extraction preserves alert variant overrides"
    (define css
      (string-append
       ":root {\n"
       "  --bs-body-bg: #fff;\n"
       "  --bs-body-color: #333;\n"
       "  --bs-border-color: #ddd;\n"
       "  --bs-primary: #325d88;\n"
       "  --bs-warning-text-emphasis: #623218;\n"
       "}\n"
       ".alert {\n"
       "  --bs-alert-color: inherit;\n"
       "  color: var(--bs-alert-color);\n"
       "}\n"
       ".alert-warning {\n"
       "  --bs-alert-color: var(--bs-warning-text-emphasis);\n"
       "  background-color: #f47c3c;\n"
       "  border-color: #fbcbb1;\n"
       "}\n"
       ".alert { color: #fff; }\n"))
    (define rules (read-css-rules css))
    (define token-map (bootstrap-rules->token-map rules))
    (check-equal? (hash-ref token-map "--we-alert-warning-bg") "#f47c3c")
    (check-equal? (hash-ref token-map "--we-alert-warning-border") "#fbcbb1")
    (check-equal? (hash-ref token-map "--we-alert-warning-color") "#fff"))

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
