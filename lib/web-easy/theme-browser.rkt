#lang webracket

;;;
;;; web-easy Browser Themes
;;;

;; Browser-only helpers for stylesheet-based theme installation and switching.

(struct theme (id class-name core-css theme-css extra-css) #:transparent)
(struct theme-manager (html-node core-link theme-link extra-link current-theme)
  #:mutable
  #:transparent)

;; extern-nullish?/theme : any/c -> boolean?
;;   Check whether v encodes JavaScript null or undefined.
(define (extern-nullish?/theme v)
  (define s (js-value->string v))
  (or (string=? s "null")
      (string=? s "undefined")))

;; check-theme : symbol? any/c -> void?
;;   Ensure v is a theme descriptor.
(define (check-theme who v)
  (unless (theme? v)
    (raise-argument-error who "theme?" v)))

;; check-theme-manager : symbol? any/c -> void?
;;   Ensure v is a theme manager.
(define (check-theme-manager who v)
  (unless (theme-manager? v)
    (raise-argument-error who "theme-manager?" v)))

;; check-observable : symbol? any/c -> void?
;;   Ensure v is an observable.
(define (check-observable who v)
  (unless (obs? v)
    (raise-argument-error who "obs?" v)))

;; validate-theme-fields! : symbol? theme? -> void?
;;   Ensure theme descriptor fields have the expected string/#f shape.
(define (validate-theme-fields! who th)
  (unless (string? (theme-class-name th))
    (raise-argument-error who "string?" (theme-class-name th)))
  (unless (string? (theme-core-css th))
    (raise-argument-error who "string?" (theme-core-css th)))
  (unless (string? (theme-theme-css th))
    (raise-argument-error who "string?" (theme-theme-css th)))
  (unless (or (not (theme-extra-css th))
              (string? (theme-extra-css th)))
    (raise-argument-error who "(or/c #f string?)" (theme-extra-css th))))

;; theme-link-id/core : -> string?
;;   Return the fixed id used for the core theme stylesheet link.
(define (theme-link-id/core)
  "we-theme-core-css")

;; theme-link-id/theme : -> string?
;;   Return the fixed id used for the primary theme stylesheet link.
(define (theme-link-id/theme)
  "we-theme-css")

;; theme-link-id/extra : -> string?
;;   Return the fixed id used for the optional extra theme stylesheet link.
(define (theme-link-id/extra)
  "we-theme-extra-css")

;; ensure-theme-link! : string? -> external?
;;   Return existing stylesheet link with id link-id, or create and attach one in head.
(define (ensure-theme-link! link-id)
  (define existing (js-get-element-by-id link-id))
  (if (or (not existing)
          (extern-nullish?/theme existing))
      (let ([head (js-document-head)]
            [link (js-create-element "link")])
        (js-set-attribute! link "id" link-id)
        (js-set-attribute! link "rel" "stylesheet")
        (js-append-child! head link)
        link)
      existing))

;; theme-class-list : external? -> any/c
;;   Return classList object for html-node, or #f when unavailable.
(define (theme-class-list html-node)
  (define class-list (js-ref/extern html-node "classList"))
  (if (or (not class-list)
          (extern-nullish?/theme class-list))
      #f
      class-list))

;; update-root-theme-class! : external? (or/c theme? #f) theme? -> void?
;;   Replace the previous theme class on html-node with the class from next-theme.
(define (update-root-theme-class! html-node current-theme next-theme)
  (define class-list (theme-class-list html-node))
  (define old-class
    (if (and current-theme (theme? current-theme))
        (theme-class-name current-theme)
        ""))
  (define new-class (theme-class-name next-theme))
  (cond
    [class-list
     (when (and (string? old-class)
                (not (string=? old-class "")))
       (js-send/extern/nullish class-list "remove" (vector old-class)))
     (when (and (string? new-class)
                (not (string=? new-class "")))
       (js-send/extern/nullish class-list "add" (vector new-class)))]
    [else
     (js-set-attribute! html-node "class" new-class)]))

;; update-theme-links! : theme-manager? theme? -> void?
;;   Update core/theme/extra stylesheet hrefs for next-theme.
(define (update-theme-links! manager next-theme)
  (js-set-attribute! (theme-manager-core-link manager)
                     "href"
                     (theme-core-css next-theme))
  (js-set-attribute! (theme-manager-theme-link manager)
                     "href"
                     (theme-theme-css next-theme))
  (if (theme-extra-css next-theme)
      (js-set-attribute! (theme-manager-extra-link manager)
                         "href"
                         (theme-extra-css next-theme))
      (js-remove-attribute! (theme-manager-extra-link manager) "href")))

;; install-theme-manager! : theme? -> theme-manager?
;;   Install stylesheet link nodes and apply initial-theme to the root html element.
(define (install-theme-manager! initial-theme)
  (check-theme 'install-theme-manager! initial-theme)
  (validate-theme-fields! 'install-theme-manager! initial-theme)
  (define manager
    (theme-manager (js-document-element)
                   (ensure-theme-link! (theme-link-id/core))
                   (ensure-theme-link! (theme-link-id/theme))
                   (ensure-theme-link! (theme-link-id/extra))
                   #f))
  (set-theme! manager initial-theme)
  manager)

;; set-theme! : theme-manager? theme? -> void?
;;   Apply next-theme using the installed stylesheet links and html root class.
(define (set-theme! manager next-theme)
  (check-theme-manager 'set-theme! manager)
  (check-theme 'set-theme! next-theme)
  (validate-theme-fields! 'set-theme! next-theme)
  (update-root-theme-class! (theme-manager-html-node manager)
                            (theme-manager-current-theme manager)
                            next-theme)
  (update-theme-links! manager next-theme)
  (set-theme-manager-current-theme! manager next-theme)
  (void))

;; observe-theme! : theme-manager? obs? procedure? -> void?
;;   Sync manager with @theme using theme->descriptor for the current and future values.
(define (observe-theme! manager @theme theme->descriptor)
  (check-theme-manager 'observe-theme! manager)
  (check-observable 'observe-theme! @theme)
  (unless (procedure? theme->descriptor)
    (raise-argument-error 'observe-theme! "procedure?" theme->descriptor))
  (set-theme! manager (theme->descriptor (obs-peek @theme)))
  (obs-observe! @theme
                (lambda (next-theme)
                  (set-theme! manager
                              (theme->descriptor next-theme))))
  (void))
