#lang webracket

(require racket/list
         (for-syntax racket/base
                     racket/list
                     racket/path))

;;;
;;; web-easy define/element
;;;

;; define/element is web-easy internal and included with:
;;   (include/reader "define-element.rkt" read-syntax/skip-first-line)
;;
;; Phase 1 constructor shape:
;;   (define/element Name Base fixed-positional ...)
;;
;; Example:
;;   (define/element H1 html-element 'h1)
;; expands to a function constructor `H1` that:
;;   - expects one positional content argument
;;   - supports #:attrs as an escape hatch
;;   - treats any other keyword as a root HTML attribute

;; keyword->attr-key : keyword? -> symbol?
;;   Convert keyword value to attribute symbol key.
(define (keyword->attr-key kw)
  (string->symbol (keyword->string kw)))

;; primitive-event-attr-keys : list?
;;   Supported generic primitive DOM event keys accepted as #:on-* keywords.
(define primitive-event-attr-keys
  '(on-click
    on-doubleclick
    on-contextmenu
    on-copy
    on-cut
    on-paste
    on-compositionstart
    on-compositionupdate
    on-compositionend
    on-keydown
    on-keyup
    on-focus
    on-blur
    on-focusin
    on-focusout
    on-input
    on-change
    on-beforeinput
    on-submit
    on-reset
    on-invalid
    on-wheel
    on-scroll
    on-drag
    on-dragstart
    on-dragend
    on-dragenter
    on-dragleave
    on-dragover
    on-drop
    on-touchstart
    on-touchmove
    on-touchend
    on-touchcancel
    on-load
    on-error
    on-abort
    on-animationstart
    on-animationend
    on-animationiteration
    on-transitionend
    on-mousedown
    on-mousemove
    on-mouseup
    on-mouseenter
    on-mouseleave
    on-mouseover
    on-mouseout
    on-pointerdown
    on-pointermove
    on-pointerup
    on-pointerenter
    on-pointerleave
    on-pointerover
    on-pointerout
    on-pointercancel
    on-gotpointercapture
    on-lostpointercapture
    on-loadeddata
    on-loadedmetadata
    on-canplay
    on-canplaythrough
    on-play
    on-playing
    on-pause
    on-ended
    on-timeupdate
    on-volumechange))

;; primitive-event-attr-key? : symbol? -> boolean?
;;   Check whether attr-key is a supported generic primitive DOM event key.
(define (primitive-event-attr-key? attr-key)
  (and (symbol? attr-key)
       (memq attr-key primitive-event-attr-keys)))

;; event-handler-value-accepted? : any/c -> boolean?
;;   Check whether v can be used as a primitive event handler value.
(define (event-handler-value-accepted? v)
  (or (procedure? v)
      (obs? v)
      (eq? v #f)))

;; string-prefix?/internal : string? string? -> boolean?
;;   Return #t when s starts with prefix.
(define (string-prefix?/internal s prefix)
  (define n  (string-length s))
  (define pn (string-length prefix))
  (and (<= pn n)
       (string=? (substring s 0 pn) prefix)))

;; wildcard-attr-symbol? : symbol? -> boolean?
;;   Return #t for wildcard symbols like 'data-*.
(define (wildcard-attr-symbol? sym)
  (define s (symbol->string sym))
  (and (>= (string-length s) 2)
       (char=? (string-ref s (- (string-length s) 1)) #\*)
       (char=? (string-ref s (- (string-length s) 2)) #\-)))

;; wildcard-attr-prefix : symbol? -> string?
;;   Return wildcard prefix for symbol like 'data-* -> "data-".
(define (wildcard-attr-prefix sym)
  (define s (symbol->string sym))
  (substring s 0 (- (string-length s) 1)))

;; attr-key-allowed? : symbol? (or/c #f list?) -> boolean?
;;   Check exact and wildcard matches for allowed attribute symbols.
(define (attr-key-allowed? attr-key allowed-attrs)
  (or (not allowed-attrs)
      (let ([key-s (symbol->string attr-key)])
        (let loop ([rest allowed-attrs])
          (cond
            [(null? rest) #f]
            [(eq? attr-key (car rest)) #t]
            [(and (symbol? (car rest))
                  (wildcard-attr-symbol? (car rest))
                  (string-prefix?/internal key-s
                                           (wildcard-attr-prefix (car rest))))
             #t]
            [else
             (loop (cdr rest))])))))

;; attrs-entry->pair : any/c -> (or/c pair? #f)
;;   Normalize attr entry as (cons symbol? any/c) when possible.
(define (attrs-entry->pair entry)
  (cond
    [(and (list? entry)
          (= (length entry) 2)
          (symbol? (car entry)))
     (cons (car entry) (cadr entry))]
    [(and (pair? entry)
          (symbol? (car entry)))
     (cons (car entry) (cdr entry))]
    [else
     #f]))

;; reject-procedure-attrs! : symbol? any/c -> void?
;;   Reject attribute values that are procedures (direct or observable current value).
(define (internal-action-attr-symbol? attr-key)
  (or (eq? attr-key 'on-click-action)
      (eq? attr-key 'on-change-action)
      (eq? attr-key 'on-enter-action)
      (primitive-event-attr-key? attr-key)))

(define (reject-procedure-attrs! who attrs)
  (when (list? attrs)
    (for-each
     (lambda (entry)
       (define normalized (attrs-entry->pair entry))
       (when normalized
         (define attr-key (car normalized))
         (define value (cdr normalized))
         (define current-value
           (if (obs? value)
               (obs-peek value)
               value))
         (when (and (procedure? current-value)
                    (not (internal-action-attr-symbol? attr-key)))
           (error who
                  "attribute value cannot be procedure: ~a"
                  attr-key))))
     attrs)))

;; reject-invalid-ref-attrs! : symbol? any/c -> void?
;;   Reject ref attribute values unless they are observables.
(define (reject-invalid-ref-attrs! who attrs)
  (when (list? attrs)
    (for-each
     (lambda (entry)
       (define normalized (attrs-entry->pair entry))
       (when normalized
         (define attr-key (car normalized))
         (define value (cdr normalized))
         (when (and (eq? attr-key 'ref)
                    (not (obs? value)))
           (error who
                  "ref attribute value must be observable?: ~a"
                  attr-key))))
     attrs)))

;; parse-element-call-args : symbol? list? (or/c #f list?) -> values list? any/c any/c
;;   Parse raw args into positional-rev, raw-#:attrs value, and extra attrs (reversed).
(define (parse-element-call-args who all-args allowed-attrs)
  (define attrs '())
  (define positional-rev '())
  (define extra-attrs-rev '())
  (let loop ([rest all-args])
    (cond
      [(null? rest)
       (values positional-rev attrs extra-attrs-rev)]
      [(keyword? (car rest))
       (when (null? (cdr rest))
         (error who "missing value after keyword argument: ~a" (car rest)))
       (define kw (car rest))
       (define v  (cadr rest))
       (cond
         [(eq? kw '#:attrs)
          (set! attrs v)]
         [else
          (define attr-key (keyword->attr-key kw))
          (when (not (attr-key-allowed? attr-key allowed-attrs))
            (error who "unknown keyword argument: ~a" kw))
          (when (and (eq? attr-key 'ref)
                     (not (obs? v)))
            (error who "expected #:ref as observable?: ~a" kw))
          (when (and (primitive-event-attr-key? attr-key)
                     (not (event-handler-value-accepted? v)))
            (error who
                   "expected event handler as procedure?, observable?, or #f: ~a"
                   kw))
          (set! extra-attrs-rev
                (cons (cons attr-key v)
                      extra-attrs-rev))])
       (loop (cddr rest))]
      [else
       (set! positional-rev (cons (car rest) positional-rev))
       (loop (cdr rest))])))

;; validate-positional-count! : symbol? list? (or/c natural? 'any) -> void?
;;   Validate positional argument count against constructor arity unless arity is 'any.
(define (validate-positional-count! who positional expected-positional-count)
  (unless (eq? expected-positional-count 'any)
    (unless (= (length positional) expected-positional-count)
      (error who
             "wrong number of positional arguments (expected ~a, got ~a)"
             expected-positional-count
             (length positional)))))

;; merge-element-attrs : symbol? any/c list? -> any/c
;;   Merge raw #:attrs payload with keyword-derived attrs.
(define (merge-element-attrs who attrs extra-attrs)
  (cond
    [(eq? attrs #f)
     (if (null? extra-attrs)
         #f
         extra-attrs)]
    [(null? extra-attrs)
     attrs]
    [(list? attrs)
     (append attrs extra-attrs)]
    [else
     (error who "expected #:attrs as list? or #f, got ~a" attrs)]))

;; normalize-element-call : symbol? list? (or/c natural? 'any) (or/c #f list?) list? list? -> values list? any/c
;;   Parse constructor call payload into positional args and merged attrs.
(define (normalize-element-call who all-args expected-positional-count allowed-attrs required-keywords required-any-keywords)
  (define (dedupe-preserve-order xs)
    (let loop ([rest xs]
               [seen '()]
               [out-rev '()])
      (cond
        [(null? rest)
         (reverse out-rev)]
        [else
         (define x (car rest))
         (if (member x seen)
             (loop (cdr rest) seen out-rev)
             (loop (cdr rest)
                   (cons x seen)
                   (cons x out-rev)))])))
  (define (keyword-count+ counts kw)
    (cond
      [(null? counts)
       (list (cons kw 1))]
      [(eq? (caar counts) kw)
       (cons (cons kw (add1 (cdar counts)))
             (cdr counts))]
      [else
       (cons (car counts)
             (keyword-count+ (cdr counts) kw))]))
  (define (keyword-count-ref counts kw)
    (cond
      [(null? counts) 0]
      [(eq? (caar counts) kw) (cdar counts)]
      [else (keyword-count-ref (cdr counts) kw)]))
  (define-values (positional-rev attrs extra-attrs-rev)
    (parse-element-call-args who all-args allowed-attrs))
  (define required-target-kws
    (dedupe-preserve-order (append required-keywords required-any-keywords)))
  (define required-counts
    (let loop ([rest all-args]
               [counts '()])
      (cond
        [(null? rest) counts]
        [(keyword? (car rest))
         (define kw (car rest))
         (if (memq kw required-target-kws)
             (loop (cddr rest) (keyword-count+ counts kw))
             (loop (cddr rest) counts))]
        [else
         (loop (cdr rest) counts)])))
  (for-each
   (lambda (required-kw)
     (define count (keyword-count-ref required-counts required-kw))
     (unless (= count 1)
       (error who "expected exactly one ~a keyword argument" required-kw)))
   required-keywords)
  (unless (null? required-any-keywords)
    (define any-count
      (let loop ([rest required-any-keywords]
                 [sum 0])
        (cond
          [(null? rest) sum]
          [else
           (loop (cdr rest)
                 (+ sum (keyword-count-ref required-counts (car rest))))])))
    (when (= any-count 0)
      (error who
             "expected at least one of keyword arguments: ~a"
             required-any-keywords)))
  (define positional (reverse positional-rev))
  (validate-positional-count! who positional expected-positional-count)
  (define extra-attrs (reverse extra-attrs-rev))
  (define merged-attrs (merge-element-attrs who attrs extra-attrs))
  (reject-procedure-attrs! who merged-attrs)
  (reject-invalid-ref-attrs! who merged-attrs)
  (values positional merged-attrs))

;; alist-set : list? any/c any/c -> list?
;;   Return alist with key updated to value, adding key when absent.
(define (alist-set alist0 k v)
  (let loop ([rest alist0]
             [acc  '()])
    (cond
      [(null? rest)
       (reverse (cons (cons k v) acc))]
      [(eq? (caar rest) k)
       (reverse (append acc
                        (cons (cons k v) (cdr rest))))]
      [else
       (loop (cdr rest)
             (cons (car rest) acc))])))

;; normalize-component-call : symbol? list? list? -> values list? list? list?
;;   Parse component call into positional args, forwarded args, and component keyword values.
;;   `component-defaults` is an alist `(list (cons keyword default) ...)`.
(define (normalize-component-call who all-args component-defaults)
  (define positional-rev '())
  (define forwarded-rev '())
  (define component-values component-defaults)
  (define component-counts '())
  (define (keyword-count+ counts kw)
    (cond
      [(null? counts)
       (list (cons kw 1))]
      [(eq? (caar counts) kw)
       (cons (cons kw (add1 (cdar counts)))
             (cdr counts))]
      [else
       (cons (car counts)
             (keyword-count+ (cdr counts) kw))]))
  (let loop ([rest all-args])
    (cond
      [(null? rest) (void)]
      [(keyword? (car rest))
       (when (null? (cdr rest))
         (error who "missing value after keyword argument: ~a" (car rest)))
       (define kw (car rest))
       (define v  (cadr rest))
       (if (assq kw component-defaults)
           (begin
             (set! component-counts
                   (keyword-count+ component-counts kw))
             (set! component-values
                   (alist-set component-values kw v)))
           (set! forwarded-rev
                 (cons v
                       (cons kw forwarded-rev))))
       (loop (cddr rest))]
      [else
       (set! positional-rev (cons (car rest) positional-rev))
       (loop (cdr rest))]))
  (for-each
   (lambda (entry)
     (define kw (car entry))
     (define count
       (let loop ([counts component-counts])
         (cond
           [(null? counts) 0]
           [(eq? (caar counts) kw) (cdar counts)]
           [else (loop (cdr counts))])))
     (when (> count 1)
       (error who "duplicate component keyword argument: ~a" kw)))
   component-defaults)
  (define forwarded (reverse forwarded-rev))
  (define positional (reverse positional-rev))
  ;; Preserve existing attribute safety for forwarded #:attrs payload.
  (let loop ([rest forwarded])
    (cond
      [(null? rest) (void)]
      [(keyword? (car rest))
       (when (null? (cdr rest))
         (error who "missing value after keyword argument: ~a" (car rest)))
       (when (eq? (car rest) '#:attrs)
         (reject-procedure-attrs! who (cadr rest)))
       (loop (cddr rest))]
      [else
       (loop (cdr rest))]))
  (values positional
          forwarded
          component-values))

;; valid-component-positional-count-spec? : any/c -> boolean?
;;   Check whether positional-count spec is `any`, non-negative integer, or non-empty list of them.
(define (valid-component-positional-count-spec? spec)
  (or (eq? spec 'any)
      (and (integer? spec) (>= spec 0))
      (and (list? spec)
           (pair? spec)
           (andmap (lambda (n)
                     (and (integer? n) (>= n 0)))
                   spec))))

;; component-positional-count->string : any/c -> string?
;;   Render positional-count spec for error messages.
(define (component-positional-count->string spec)
  (cond
    [(eq? spec 'any) "any"]
    [(and (integer? spec) (>= spec 0))
     (number->string spec)]
    [(and (list? spec) (pair? spec))
     (string-join (map number->string spec) " or ")]
    [else
     "~invalid~"]))

;; validate-component-positional-count! : symbol? list? any/c -> void?
;;   Validate component positional count against allowed count spec.
(define (validate-component-positional-count! who positional count-spec)
  (unless (valid-component-positional-count-spec? count-spec)
    (error who "invalid #:positional-count spec: ~a" count-spec))
  (unless (eq? count-spec 'any)
    (define n (length positional))
    (define ok?
      (cond
        [(and (integer? count-spec) (>= count-spec 0))
         (= n count-spec)]
        [(and (list? count-spec) (pair? count-spec))
         (memv n count-spec)]
        [else
         #f]))
    (unless ok?
      (error who
             "wrong number of positional arguments (expected ~a, got ~a)"
             (component-positional-count->string count-spec)
             n))))

;; component-positional-ref/default : list? natural? any/c -> any/c
;;   Return positional value at index when present, else default-value.
(define (component-positional-ref/default positional idx default-value)
  (if (> (length positional) idx)
      (list-ref positional idx)
      default-value))

;; component-keyword-ref : list? keyword? -> any/c
;;   Lookup normalized component keyword value.
(define (component-keyword-ref component-values kw)
  (define p (assq kw component-values))
  (if p
      (cdr p)
      (error 'component-keyword-ref "unknown component keyword: ~a" kw)))

;; split-forwarded-attrs-arg : symbol? list? -> values list? any/c
;;   Remove #:attrs from forwarded args and return values `(args-without-attrs attrs-value)`.
;;   When #:attrs is not present, attrs-value is #f.
(define (split-forwarded-attrs-arg who forwarded-args)
  (define attrs-value #f)
  (define without-rev '())
  (let loop ([rest forwarded-args])
    (cond
      [(null? rest)
       (values (reverse without-rev) attrs-value)]
      [(keyword? (car rest))
       (when (null? (cdr rest))
         (error who "missing value after keyword argument: ~a" (car rest)))
       (define kw (car rest))
       (define v  (cadr rest))
       (if (eq? kw '#:attrs)
           (set! attrs-value v)
           (set! without-rev (cons v (cons kw without-rev))))
       (loop (cddr rest))]
      [else
       (set! without-rev (cons (car rest) without-rev))
       (loop (cdr rest))])))

;; forwarded-keywords->attrs : symbol? list? (or/c #f list?) -> list?
;;   Convert forwarded keyword args to attr pairs `(cons symbol value)`.
;;   Reject positional values in forwarded payload.
(define (forwarded-keywords->attrs who forwarded-args [allowed-attrs #f])
  (define attrs-rev '())
  (let loop ([rest forwarded-args])
    (cond
      [(null? rest)
       (reverse attrs-rev)]
      [(keyword? (car rest))
       (when (null? (cdr rest))
         (error who "missing value after keyword argument: ~a" (car rest)))
       (define attr-key (keyword->attr-key (car rest)))
       (when (not (or (attr-key-allowed? attr-key allowed-attrs)
                      (primitive-event-attr-key? attr-key)))
         (error who "unknown keyword argument: ~a" (car rest)))
       (when (and (eq? attr-key 'ref)
                  (not (obs? (cadr rest))))
         (error who "expected #:ref as observable?: ~a" (car rest)))
       (when (and (primitive-event-attr-key? attr-key)
                  (not (event-handler-value-accepted? (cadr rest))))
         (error who
                "expected event handler as procedure?, observable?, or #f: ~a"
                (car rest)))
       (set! attrs-rev
             (cons (cons attr-key
                         (cadr rest))
                   attrs-rev))
       (loop (cddr rest))]
      [else
       (error who
              "unexpected positional argument in forwarded keyword payload: ~a"
              (car rest))])))

;; component-root-attrs : symbol? list? (or/c #f list?) list? -> any/c
;;   Return merged root attrs with forwarded keyword attrs normalized and validated.
(define (component-root-attrs who forwarded-args allowed-attrs root-attrs)
  (define-values (forwarded-args/without-attrs forwarded-attrs)
    (split-forwarded-attrs-arg who forwarded-args))
  (define forwarded-attrs/normalized
    (cond
      [(eq? forwarded-attrs #f)
       '()]
      [(list? forwarded-attrs)
       (map (lambda (entry)
              (define normalized
                (attrs-entry->pair entry))
              (if normalized
                  normalized
                  (error who
                         "expected attr entry as pair? or 2-element list?"
                         entry)))
            forwarded-attrs)]
      [else
       (error who
              "expected #:attrs as list? or #f, got ~a"
              forwarded-attrs)]))
  (define forwarded-attr-pairs
    (filter (lambda (entry)
              (not (and (eq? (car entry) 'class)
                        (eq? (cdr entry) #f))))
            (forwarded-keywords->attrs who
                                       forwarded-args/without-attrs
                                       allowed-attrs)))
  (define forwarded-merged
    (merge-element-attrs who
                         forwarded-attrs/normalized
                         forwarded-attr-pairs))
  (define attrs/final
    (merge-element-attrs who
                         root-attrs
                         forwarded-merged))
  attrs/final)

;; make-component-root-attrs : symbol? list? (or/c #f list?) -> (-> list? any/c)
;;   Return helper function that merges component root attrs for a fixed call context.
(define (make-component-root-attrs who forwarded-args allowed-attrs)
  (lambda (attrs)
    (component-root-attrs who
                          forwarded-args
                          allowed-attrs
                          attrs)))

;; define/component : syntax -> definition
;;   Define internal semantic component constructor with:
;;   - optional `#:root-tag 'tag` for HTML attr validation via committed spec
;;   - optional `#:positional` entries as `[id]` required and `[id default]` optional
;;   - optional `#:positional-count` (inferred from `#:positional` when omitted)
;;   - optional `#:component-keywords ([#:kw id default] ...)`
;;   - optional `#:root-attrs root-id` where `(define root-id attrs-expr)` in body is
;;     rewritten to merge attrs through generated forwarded/global attr handling.
;;   Style note: prefer plain body-level `define`s together with `#:root-attrs <id>`
;;   instead of wrapping setup in `let*`/`let` unless lexical scoping is needed.
(define-syntax (define/component stx)
  (define who 'define/component)
  ;; valid-component-positional-count-spec/syntax? : syntax? -> boolean?
  ;;   Check compile-time positional-count spec syntax for define/component.
  (define (valid-component-positional-count-spec/syntax? stx-count)
    (define datum (syntax->datum stx-count))
    (or (eq? datum 'any)
        (and (integer? datum) (>= datum 0))
        (and (list? datum)
             (pair? datum)
             (andmap (lambda (n)
                     (and (integer? n) (>= n 0)))
                     datum))))
  ;; normalize-count-datum/component : any/c -> any/c
  ;;   Normalize count datum to canonical comparable shape.
  (define (normalize-count-datum/component datum)
    (cond
      [(eq? datum 'any) 'any]
      [(and (integer? datum) (>= datum 0))
       (list datum)]
      [(and (list? datum)
            (pair? datum)
            (andmap (lambda (n)
                      (and (integer? n) (>= n 0)))
                    datum))
       (sort (remove-duplicates datum) <)]
      [else
       #f]))
  ;; range-list/component : natural? natural? -> list?
  ;;   Return inclusive integer range [a,b].
  (define (range-list/component a b)
    (let loop ([i a]
               [acc '()])
      (if (> i b)
          (reverse acc)
          (loop (add1 i)
                (cons i acc)))))
  ;; parse-positional-spec/component : syntax? -> values list? natural? natural?
  ;;   Parse #:positional entries and return entry specs plus required/total counts.
  ;;   Each entry is `(list id-stx has-default? default-stx-or-#f)`.
  (define (parse-positional-spec/component positional-stx)
    (define entry-stxs (syntax->list positional-stx))
    (unless entry-stxs
      (raise-syntax-error who
                          "expected positional list as `([id] [id default] ...)`"
                          stx
                          positional-stx))
    (define saw-default? #f)
    (define required-count 0)
    (define entries
      (map
       (lambda (entry-stx)
         (syntax-case entry-stx ()
           [(id)
            (begin
              (unless (identifier? #'id)
                (raise-syntax-error who
                                    "expected identifier in positional entry"
                                    stx
                                    entry-stx))
              (when saw-default?
                (raise-syntax-error who
                                    "required positional entries must come before defaulted entries"
                                    stx
                                    entry-stx))
              (set! required-count (add1 required-count))
              (list #'id #f #f))]
           [(id default)
            (begin
              (unless (identifier? #'id)
                (raise-syntax-error who
                                    "expected identifier in positional entry"
                                    stx
                                    entry-stx))
              (set! saw-default? #t)
              (list #'id #t #'default))]
           [_
            (raise-syntax-error who
                                "expected positional entry as `[id]` or `[id default]`"
                                stx
                                entry-stx)]))
       entry-stxs))
    (values entries
            required-count
            (length entries)))
  ;; inferred-count-datum/component : natural? natural? -> any/c
  ;;   Build inferred positional count datum from required and total positional entries.
  (define (inferred-count-datum/component required-count total-count)
    (if (= required-count total-count)
        required-count
        (range-list/component required-count total-count)))
  ;; rewrite-root-attrs-define/component : list? syntax? syntax? -> list?
  ;;   Rewrite `(define root-id expr)` in body to `(define root-id (root-attrs/fn expr))`.
  (define (rewrite-root-attrs-define/component body-stxs root-id-stx root-attrs-fn-stx)
    (define target (syntax-e root-id-stx))
    (define seen? #f)
    (define rewritten
      (map
       (lambda (form)
         (syntax-case form (define)
           [(define id expr)
            (if (and (identifier? #'id)
                     (eq? (syntax-e #'id) target))
                (begin
                  (set! seen? #t)
                  #`(define #,root-id-stx
                      (#,root-attrs-fn-stx expr)))
                form)]
           [_
            form]))
       body-stxs))
    (unless seen?
      (raise-syntax-error who
                          "expected body to contain `(define <id> ...)` matching #:root-attrs identifier"
                          stx
                          root-id-stx))
    rewritten)
  ;; html-attr-table-cache/component : (or/c #f (hash/c string? list?))
  ;;   Cache table parsed from spec html-element-attributes.sexp for define/component.
  (define html-attr-table-cache/component #f)
  ;; load-html-attr-table/component : syntax? -> hash?
  ;;   Read HTML attribute table from `spec/html-element-attributes.sexp`.
  (define (load-html-attr-table/component use-stx)
    (if html-attr-table-cache/component
        html-attr-table-cache/component
        (let* ([src0 (syntax-source use-stx)]
               [src (cond
                      [(path? src0) src0]
                      [(string? src0) (string->path src0)]
                      [else #f])])
          (unless src
            (raise-syntax-error who
                                "cannot resolve source location for html attribute table lookup"
                                use-stx))
          (define table-path
            (build-path (path-only src)
                        "spec"
                        "html-element-attributes.sexp"))
          (unless (file-exists? table-path)
            (raise-syntax-error who
                                "missing spec/html-element-attributes.sexp (run tools/fetch-html-element-attributes.mjs)"
                                use-stx))
          (define datum
            (with-input-from-file table-path
              (lambda ()
                (read))))
          (unless (and (list? datum)
                       (pair? datum)
                       (eq? (car datum) 'html-element-attributes))
            (raise-syntax-error who
                                "malformed spec/html-element-attributes.sexp header"
                                use-stx))
          (define table-entry
            (let loop ([rest (cdr datum)])
              (cond
                [(null? rest) #f]
                [(and (pair? (car rest))
                      (eq? (caar rest) 'table))
                 (car rest)]
                [else
                 (loop (cdr rest))])))
          (unless (and table-entry (list? table-entry))
            (raise-syntax-error who
                                "malformed spec/html-element-attributes.sexp table"
                                use-stx))
          (define ht (make-hash))
          (for-each
           (lambda (row)
             (when (and (list? row)
                        (= (length row) 2)
                        (string? (car row))
                        (list? (cadr row)))
               (define attr-symbols
                 (filter symbol?
                         (map string->symbol
                              (filter string? (cadr row)))))
               (hash-set! ht (car row) attr-symbols)))
           (cdr table-entry))
          (set! html-attr-table-cache/component ht)
          ht)))
  ;; allowed-attrs-for-html-tag/component : syntax? symbol? -> list?
  ;;   Return deduplicated allowed attrs from globals + element-specific attrs.
  (define (allowed-attrs-for-html-tag/component use-stx tag-sym)
    (define table (load-html-attr-table/component use-stx))
    (define globals (hash-ref table "*" '()))
    (define specific (hash-ref table (symbol->string tag-sym) '()))
    (remove-duplicates (append globals
                               specific
                               '(data-* aria-*))))
  (syntax-case stx (quote)
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:component-keywords ([kw-id kw-var kw-default] ...)
        #:rest rest-id
        #:root-attrs root-attrs-id
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (identifier? #'rest-id)
          (identifier? #'root-attrs-id)
          (andmap keyword? (syntax->datum #'(kw-id ...)))
          (andmap identifier? (syntax->list #'(kw-var ...))))
     (let* ([allowed-attrs
             (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))]
            [root-attrs/fn-id
             (datum->syntax stx 'root-attrs/fn stx stx)]
            [rewritten-body
             (rewrite-root-attrs-define/component (syntax->list #'(body ...))
                                                  #'root-attrs-id
                                                  root-attrs/fn-id)])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [root-attrs/fn-id/stx root-attrs/fn-id]
                     [(body* ...) rewritten-body])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym
                                         all-args
                                         (list (cons 'kw-id kw-default) ...)))
             (define component-allowed-attrs/id allowed-attrs-stx)
             (define kw-var (component-keyword-ref component-values/id 'kw-id))
             ...
             (define rest-id positional/id)
             (define root-attrs/fn-id/stx
               (make-component-root-attrs 'name/sym
                                          forwarded-args/id
                                          component-allowed-attrs/id))
             body* ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:rest rest-id
        #:root-attrs root-attrs-id
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (identifier? #'rest-id)
          (identifier? #'root-attrs-id))
     (let* ([allowed-attrs
             (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))]
            [root-attrs/fn-id
             (datum->syntax stx 'root-attrs/fn stx stx)]
            [rewritten-body
             (rewrite-root-attrs-define/component (syntax->list #'(body ...))
                                                  #'root-attrs-id
                                                  root-attrs/fn-id)])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [root-attrs/fn-id/stx root-attrs/fn-id]
                     [(body* ...) rewritten-body])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym all-args '()))
             (define component-allowed-attrs/id allowed-attrs-stx)
             (void component-values/id)
             (define rest-id positional/id)
             (define root-attrs/fn-id/stx
               (make-component-root-attrs 'name/sym
                                          forwarded-args/id
                                          component-allowed-attrs/id))
             body* ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:component-keywords ([kw-id kw-var kw-default] ...)
        #:rest rest-id
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (identifier? #'rest-id)
          (andmap keyword? (syntax->datum #'(kw-id ...)))
          (andmap identifier? (syntax->list #'(kw-var ...))))
     (let ([allowed-attrs
            (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym
                                         all-args
                                         (list (cons 'kw-id kw-default) ...)))
             (define component-allowed-attrs/id allowed-attrs-stx)
             (define kw-var (component-keyword-ref component-values/id 'kw-id))
             ...
             (define rest-id positional/id)
             body ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:rest rest-id
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (identifier? #'rest-id))
     (let ([allowed-attrs
            (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym all-args '()))
             (define component-allowed-attrs/id allowed-attrs-stx)
             (void component-values/id)
             (define rest-id positional/id)
             body ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:positional (pos-entry ...)
        #:component-keywords ([kw-id kw-var kw-default] ...)
        #:root-attrs root-attrs-id
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (identifier? #'root-attrs-id)
          (andmap keyword? (syntax->datum #'(kw-id ...)))
          (andmap identifier? (syntax->list #'(kw-var ...))))
     (let* ([allowed-attrs
             (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))]
            [parsed-positional
             (call-with-values
                 (lambda ()
                   (parse-positional-spec/component #'(pos-entry ...)))
               list)]
            [entries (list-ref parsed-positional 0)]
            [required-count (list-ref parsed-positional 1)]
            [total-count (list-ref parsed-positional 2)]
            [count-datum
             (inferred-count-datum/component required-count total-count)]
            [positional-id
             (datum->syntax stx 'positional stx stx)]
            [root-attrs/fn-id
             (datum->syntax stx 'root-attrs/fn stx stx)]
            [rewritten-body
             (rewrite-root-attrs-define/component (syntax->list #'(body ...))
                                                  #'root-attrs-id
                                                  root-attrs/fn-id)]
            [pos-bindings
             (let loop ([rest entries]
                        [idx 0]
                        [acc '()])
               (cond
                 [(null? rest)
                  (reverse acc)]
                 [else
                  (define id-stx (list-ref (car rest) 0))
                  (define has-default? (list-ref (car rest) 1))
                  (define default-stx (list-ref (car rest) 2))
                  (define idx-stx (datum->syntax stx idx stx stx))
                  (define bind-stx
                    (if has-default?
                        #`(define #,id-stx
                            (component-positional-ref/default #,positional-id
                                                             #,idx-stx
                                                             #,default-stx))
                        #`(define #,id-stx
                            (list-ref #,positional-id #,idx-stx))))
                  (loop (cdr rest)
                        (add1 idx)
                        (cons bind-stx acc))]))])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [count-spec-stx (datum->syntax stx (list 'quote count-datum) stx stx)]
                     [root-attrs/fn-id/stx root-attrs/fn-id]
                     [(pos-binding ...) pos-bindings]
                     [(body* ...) rewritten-body])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym
                                         all-args
                                         (list (cons 'kw-id kw-default) ...)))
             (validate-component-positional-count! 'name/sym
                                                   positional/id
                                                   count-spec-stx)
             (define component-allowed-attrs/id allowed-attrs-stx)
             pos-binding ...
             (define kw-var (component-keyword-ref component-values/id 'kw-id))
             ...
             (define root-attrs/fn-id/stx
               (make-component-root-attrs 'name/sym
                                          forwarded-args/id
                                          component-allowed-attrs/id))
             body* ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:positional-count positional-count
        #:positional (pos-entry ...)
        #:root-attrs root-attrs-id
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (valid-component-positional-count-spec/syntax? #'positional-count)
          (identifier? #'root-attrs-id))
     (let* ([allowed-attrs
             (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))]
            [count-datum
             (syntax->datum #'positional-count)]
            [count-normalized
             (normalize-count-datum/component count-datum)]
            [_ (unless count-normalized
                 (raise-syntax-error who
                                     "invalid #:positional-count for define/component"
                                     stx
                                     #'positional-count))]
            [parsed-positional
             (call-with-values
                 (lambda ()
                   (parse-positional-spec/component #'(pos-entry ...)))
               list)]
            [entries (list-ref parsed-positional 0)]
            [required-count (list-ref parsed-positional 1)]
            [total-count (list-ref parsed-positional 2)]
            [inferred-count
             (inferred-count-datum/component required-count total-count)]
            [inferred-normalized
             (normalize-count-datum/component inferred-count)]
            [_ (unless (equal? count-normalized inferred-normalized)
                 (raise-syntax-error who
                                     "inconsistent #:positional-count and #:positional specification"
                                     stx
                                     #'positional-count))]
            [positional-id
             (datum->syntax stx 'positional stx stx)]
            [root-attrs/fn-id
             (datum->syntax stx 'root-attrs/fn stx stx)]
            [rewritten-body
             (rewrite-root-attrs-define/component (syntax->list #'(body ...))
                                                  #'root-attrs-id
                                                  root-attrs/fn-id)]
            [pos-bindings
             (let loop ([rest entries]
                        [idx 0]
                        [acc '()])
               (cond
                 [(null? rest)
                  (reverse acc)]
                 [else
                  (define id-stx (list-ref (car rest) 0))
                  (define has-default? (list-ref (car rest) 1))
                  (define default-stx (list-ref (car rest) 2))
                  (define idx-stx (datum->syntax stx idx stx stx))
                  (define bind-stx
                    (if has-default?
                        #`(define #,id-stx
                            (component-positional-ref/default #,positional-id
                                                             #,idx-stx
                                                             #,default-stx))
                        #`(define #,id-stx
                            (list-ref #,positional-id #,idx-stx))))
                  (loop (cdr rest)
                        (add1 idx)
                        (cons bind-stx acc))]))])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [count-spec-stx (datum->syntax stx (list 'quote count-datum) stx stx)]
                     [root-attrs/fn-id/stx root-attrs/fn-id]
                     [(pos-binding ...) pos-bindings]
                     [(body* ...) rewritten-body])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym all-args '()))
             (validate-component-positional-count! 'name/sym
                                                   positional/id
                                                   count-spec-stx)
             (define component-allowed-attrs/id allowed-attrs-stx)
             (void component-values/id)
             pos-binding ...
             (define root-attrs/fn-id/stx
               (make-component-root-attrs 'name/sym
                                          forwarded-args/id
                                          component-allowed-attrs/id))
             body* ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:positional (pos-entry ...)
        #:root-attrs root-attrs-id
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (identifier? #'root-attrs-id))
     (let* ([allowed-attrs
             (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))]
            [parsed-positional
             (call-with-values
                 (lambda ()
                   (parse-positional-spec/component #'(pos-entry ...)))
               list)]
            [entries (list-ref parsed-positional 0)]
            [required-count (list-ref parsed-positional 1)]
            [total-count (list-ref parsed-positional 2)]
            [count-datum
             (inferred-count-datum/component required-count total-count)]
            [positional-id
             (datum->syntax stx 'positional stx stx)]
            [root-attrs/fn-id
             (datum->syntax stx 'root-attrs/fn stx stx)]
            [rewritten-body
             (rewrite-root-attrs-define/component (syntax->list #'(body ...))
                                                  #'root-attrs-id
                                                  root-attrs/fn-id)]
            [pos-bindings
             (let loop ([rest entries]
                        [idx 0]
                        [acc '()])
               (cond
                 [(null? rest)
                  (reverse acc)]
                 [else
                  (define id-stx (list-ref (car rest) 0))
                  (define has-default? (list-ref (car rest) 1))
                  (define default-stx (list-ref (car rest) 2))
                  (define idx-stx (datum->syntax stx idx stx stx))
                  (define bind-stx
                    (if has-default?
                        #`(define #,id-stx
                            (component-positional-ref/default #,positional-id
                                                             #,idx-stx
                                                             #,default-stx))
                        #`(define #,id-stx
                            (list-ref #,positional-id #,idx-stx))))
                  (loop (cdr rest)
                        (add1 idx)
                        (cons bind-stx acc))]))])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [count-spec-stx (datum->syntax stx (list 'quote count-datum) stx stx)]
                     [root-attrs/fn-id/stx root-attrs/fn-id]
                     [(pos-binding ...) pos-bindings]
                     [(body* ...) rewritten-body])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym all-args '()))
             (validate-component-positional-count! 'name/sym
                                                   positional/id
                                                   count-spec-stx)
             (define component-allowed-attrs/id allowed-attrs-stx)
             (void component-values/id)
             pos-binding ...
             (define root-attrs/fn-id/stx
               (make-component-root-attrs 'name/sym
                                          forwarded-args/id
                                          component-allowed-attrs/id))
             body* ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:positional-count positional-count
        #:positional (pos-entry ...)
        #:component-keywords ([kw-id kw-var kw-default] ...)
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (valid-component-positional-count-spec/syntax? #'positional-count)
          (andmap keyword? (syntax->datum #'(kw-id ...)))
          (andmap identifier? (syntax->list #'(kw-var ...))))
     (let* ([allowed-attrs
             (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))]
            [count-datum
             (syntax->datum #'positional-count)]
            [count-normalized
             (normalize-count-datum/component count-datum)]
            [_ (unless count-normalized
                 (raise-syntax-error who
                                     "invalid #:positional-count for define/component"
                                     stx
                                     #'positional-count))]
            [parsed-positional
             (call-with-values
                 (lambda ()
                   (parse-positional-spec/component #'(pos-entry ...)))
               list)]
            [entries (list-ref parsed-positional 0)]
            [required-count (list-ref parsed-positional 1)]
            [total-count (list-ref parsed-positional 2)]
            [inferred-count
             (inferred-count-datum/component required-count total-count)]
            [inferred-normalized
             (normalize-count-datum/component inferred-count)]
            [_ (unless (equal? count-normalized inferred-normalized)
                 (raise-syntax-error who
                                     "inconsistent #:positional-count and #:positional specification"
                                     stx
                                     #'positional-count))]
            [positional-id
             (datum->syntax stx 'positional stx stx)]
            [pos-bindings
             (let loop ([rest entries]
                        [idx 0]
                        [acc '()])
               (cond
                 [(null? rest)
                  (reverse acc)]
                 [else
                  (define id-stx (list-ref (car rest) 0))
                  (define has-default? (list-ref (car rest) 1))
                  (define default-stx (list-ref (car rest) 2))
                  (define idx-stx (datum->syntax stx idx stx stx))
                  (define bind-stx
                    (if has-default?
                        #`(define #,id-stx
                            (component-positional-ref/default #,positional-id
                                                             #,idx-stx
                                                             #,default-stx))
                        #`(define #,id-stx
                            (list-ref #,positional-id #,idx-stx))))
                  (loop (cdr rest)
                        (add1 idx)
                        (cons bind-stx acc))]))])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [count-spec-stx (datum->syntax stx (list 'quote count-datum) stx stx)]
                     [(pos-binding ...) pos-bindings])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym
                                         all-args
                                         (list (cons 'kw-id kw-default) ...)))
             (validate-component-positional-count! 'name/sym
                                                  positional/id
                                                  count-spec-stx)
             (define component-allowed-attrs/id allowed-attrs-stx)
             pos-binding ...
             (define kw-var (component-keyword-ref component-values/id 'kw-id))
             ...
             body ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:positional (pos-entry ...)
        #:component-keywords ([kw-id kw-var kw-default] ...)
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (andmap keyword? (syntax->datum #'(kw-id ...)))
          (andmap identifier? (syntax->list #'(kw-var ...))))
     (let* ([allowed-attrs
             (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))]
            [parsed-positional
             (call-with-values
                 (lambda ()
                   (parse-positional-spec/component #'(pos-entry ...)))
               list)]
            [entries (list-ref parsed-positional 0)]
            [required-count (list-ref parsed-positional 1)]
            [total-count (list-ref parsed-positional 2)]
            [count-datum
             (inferred-count-datum/component required-count total-count)]
            [positional-id
             (datum->syntax stx 'positional stx stx)]
            [pos-bindings
             (let loop ([rest entries]
                        [idx 0]
                        [acc '()])
               (cond
                 [(null? rest)
                  (reverse acc)]
                 [else
                  (define id-stx (list-ref (car rest) 0))
                  (define has-default? (list-ref (car rest) 1))
                  (define default-stx (list-ref (car rest) 2))
                  (define idx-stx (datum->syntax stx idx stx stx))
                  (define bind-stx
                    (if has-default?
                        #`(define #,id-stx
                            (component-positional-ref/default #,positional-id
                                                             #,idx-stx
                                                             #,default-stx))
                        #`(define #,id-stx
                            (list-ref #,positional-id #,idx-stx))))
                  (loop (cdr rest)
                        (add1 idx)
                        (cons bind-stx acc))]))])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [count-spec-stx (datum->syntax stx (list 'quote count-datum) stx stx)]
                     [(pos-binding ...) pos-bindings])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym
                                         all-args
                                         (list (cons 'kw-id kw-default) ...)))
             (validate-component-positional-count! 'name/sym
                                                  positional/id
                                                  count-spec-stx)
             (define component-allowed-attrs/id allowed-attrs-stx)
             pos-binding ...
             (define kw-var (component-keyword-ref component-values/id 'kw-id))
             ...
             body ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:positional-count positional-count
        #:component-keywords ([kw-id kw-var kw-default] ...)
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (valid-component-positional-count-spec/syntax? #'positional-count)
          (andmap keyword? (syntax->datum #'(kw-id ...)))
          (andmap identifier? (syntax->list #'(kw-var ...))))
     (let ([allowed-attrs
            (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))]
           [count-datum
            (syntax->datum #'positional-count)])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [count-spec-stx (datum->syntax stx (list 'quote count-datum) stx stx)])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym
                                         all-args
                                         (list (cons 'kw-id kw-default) ...)))
             (validate-component-positional-count! 'name/sym
                                                  positional/id
                                                  count-spec-stx)
             (define component-allowed-attrs/id allowed-attrs-stx)
             (define kw-var (component-keyword-ref component-values/id 'kw-id))
             ...
             body ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:component-keywords ([kw-id kw-var kw-default] ...)
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (andmap keyword? (syntax->datum #'(kw-id ...)))
          (andmap identifier? (syntax->list #'(kw-var ...))))
     (let ([allowed-attrs
            (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym
                                         all-args
                                         (list (cons 'kw-id kw-default) ...)))
             (define component-allowed-attrs/id allowed-attrs-stx)
             (define kw-var (component-keyword-ref component-values/id 'kw-id))
             ...
             body ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        #:positional-count positional-count
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym))
          (valid-component-positional-count-spec/syntax? #'positional-count))
     (let ([allowed-attrs
            (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))]
           [count-datum
            (syntax->datum #'positional-count)])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [count-spec-stx (datum->syntax stx (list 'quote count-datum) stx stx)])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym all-args '()))
             (validate-component-positional-count! 'name/sym
                                                  positional/id
                                                  count-spec-stx)
             (define component-allowed-attrs/id allowed-attrs-stx)
             (void component-values/id)
             body ...)))]
    [(_ name
        #:root-tag (quote root-tag-sym)
        body ...)
     (and (identifier? #'name)
          (symbol? (syntax-e #'root-tag-sym)))
     (let ([allowed-attrs
            (allowed-attrs-for-html-tag/component stx (syntax-e #'root-tag-sym))])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym all-args '()))
             (define component-allowed-attrs/id allowed-attrs-stx)
             (void component-values/id)
             body ...)))]
    [(_ name
        #:component-keywords ([kw-id kw-var kw-default] ...)
        body ...)
     (and (identifier? #'name)
          (andmap keyword? (syntax->datum #'(kw-id ...)))
          (andmap identifier? (syntax->list #'(kw-var ...))))
     (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                   [positional/id (datum->syntax stx 'positional stx stx)]
                   [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                   [component-values/id (datum->syntax stx 'component-values stx stx)]
                   [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)])
       #'(define (name . all-args)
           (define-values (positional/id forwarded-args/id component-values/id)
             (normalize-component-call 'name/sym
                                       all-args
                                       (list (cons 'kw-id kw-default) ...)))
           (define component-allowed-attrs/id #f)
             (define kw-var (component-keyword-ref component-values/id 'kw-id))
             ...
             body ...))]
    [(_ name
        #:positional-count positional-count
        #:component-keywords ([kw-id kw-var kw-default] ...)
        body ...)
     (and (identifier? #'name)
          (valid-component-positional-count-spec/syntax? #'positional-count)
          (andmap keyword? (syntax->datum #'(kw-id ...)))
          (andmap identifier? (syntax->list #'(kw-var ...))))
     (let ([count-datum
            (syntax->datum #'positional-count)])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [count-spec-stx (datum->syntax stx (list 'quote count-datum) stx stx)])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym
                                         all-args
                                         (list (cons 'kw-id kw-default) ...)))
             (validate-component-positional-count! 'name/sym
                                                  positional/id
                                                  count-spec-stx)
             (define component-allowed-attrs/id #f)
             (define kw-var (component-keyword-ref component-values/id 'kw-id))
             ...
             body ...)))]
    [(_ name
        #:positional-count positional-count
        body ...)
     (and (identifier? #'name)
          (valid-component-positional-count-spec/syntax? #'positional-count))
     (let ([count-datum
            (syntax->datum #'positional-count)])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [positional/id (datum->syntax stx 'positional stx stx)]
                     [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                     [component-values/id (datum->syntax stx 'component-values stx stx)]
                     [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)]
                     [count-spec-stx (datum->syntax stx (list 'quote count-datum) stx stx)])
         #'(define (name . all-args)
             (define-values (positional/id forwarded-args/id component-values/id)
               (normalize-component-call 'name/sym all-args '()))
             (validate-component-positional-count! 'name/sym
                                                  positional/id
                                                  count-spec-stx)
             (define component-allowed-attrs/id #f)
             (void component-values/id)
             body ...)))]
    [(_ name body ...)
     (identifier? #'name)
     (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                   [positional/id (datum->syntax stx 'positional stx stx)]
                   [forwarded-args/id (datum->syntax stx 'forwarded-args stx stx)]
                   [component-values/id (datum->syntax stx 'component-values stx stx)]
                   [component-allowed-attrs/id (datum->syntax stx 'component-allowed-attrs stx stx)])
       #'(define (name . all-args)
           (define-values (positional/id forwarded-args/id component-values/id)
             (normalize-component-call 'name/sym all-args '()))
           (define component-allowed-attrs/id #f)
           (void component-values/id)
           body ...))]
    [_
     (raise-syntax-error who
                         "expected `(define/component Name [#:root-tag 'tag] [#:positional-count count] [#:positional ([id] [id default] ...)] [#:component-keywords ([#:kw id default] ...)] [#:root-attrs id] body ...)`"
                         stx)]))

;; define/element : (define/element Name Base fixed-positional ...) -> definition
;;   Define generic uppercase element constructor.
(define-syntax (define/element stx)
  (define who 'define/element)
  ;; html-attr-table-cache : (or/c #f (hash/c string? list?))
  ;;   Cache table parsed from spec html-element-attributes.sexp.
  (define html-attr-table-cache #f)
  ;; load-html-attr-table : syntax? -> hash?
  ;;   Read HTML attribute table from `spec/html-element-attributes.sexp`.
  (define (load-html-attr-table use-stx)
    (if html-attr-table-cache
        html-attr-table-cache
        (let* ([src0 (syntax-source use-stx)]
               [src (cond
                      [(path? src0) src0]
                      [(string? src0) (string->path src0)]
                      [else #f])])
          (unless src
            (raise-syntax-error who
                                "cannot resolve source location for html attribute table lookup"
                                use-stx))
          (define table-path
            (build-path (path-only src)
                        "spec"
                        "html-element-attributes.sexp"))
          (unless (file-exists? table-path)
            (raise-syntax-error who
                                "missing spec/html-element-attributes.sexp (run tools/fetch-html-element-attributes.mjs)"
                                use-stx))
          (define datum
            (with-input-from-file table-path
              (lambda ()
                (read))))
          (unless (and (list? datum)
                       (pair? datum)
                       (eq? (car datum) 'html-element-attributes))
            (raise-syntax-error who
                                "malformed spec/html-element-attributes.sexp header"
                                use-stx))
          (define table-entry
            (let loop ([rest (cdr datum)])
              (cond
                [(null? rest) #f]
                [(and (pair? (car rest))
                      (eq? (caar rest) 'table))
                 (car rest)]
                [else
                 (loop (cdr rest))])))
          (unless (and table-entry (list? table-entry))
            (raise-syntax-error who
                                "malformed spec/html-element-attributes.sexp table"
                                use-stx))
          (define ht (make-hash))
          (for-each
           (lambda (row)
             (when (and (list? row)
                        (= (length row) 2)
                        (string? (car row))
                        (list? (cadr row)))
               (define attr-symbols
                 (filter symbol?
                         (map string->symbol
                              (filter string? (cadr row)))))
               (hash-set! ht (car row) attr-symbols)))
           (cdr table-entry))
          (set! html-attr-table-cache ht)
          ht)))
  ;; allowed-attrs-for-html-tag : syntax? symbol? -> list?
  ;;   Return deduplicated allowed attrs from globals + element-specific attrs.
  (define (allowed-attrs-for-html-tag use-stx tag-sym)
    (define table (load-html-attr-table use-stx))
    (define globals (hash-ref table "*" '()))
    (define specific (hash-ref table (symbol->string tag-sym) '()))
    (remove-duplicates (append globals
                               specific
                               '(data-* aria-*)
                               '(on-click
                                 ref
                                 on-doubleclick
                                 on-contextmenu
                                 on-copy
                                 on-cut
                                 on-paste
                                 on-compositionstart
                                 on-compositionupdate
                                 on-compositionend
                                 on-keydown
                                 on-keyup
                                 on-focus
                                 on-blur
                                 on-focusin
                                 on-focusout
                                 on-input
                                 on-change
                                 on-beforeinput
                                 on-submit
                                 on-reset
                                 on-invalid
                                 on-wheel
                                 on-scroll
                                 on-drag
                                 on-dragstart
                                 on-dragend
                                 on-dragenter
                                 on-dragleave
                                 on-dragover
                                 on-drop
                                 on-touchstart
                                 on-touchmove
                                 on-touchend
                                 on-touchcancel
                                 on-load
                                 on-error
                                 on-abort
                                 on-animationstart
                                 on-animationend
                                 on-animationiteration
                                 on-transitionend
                                 on-mousedown
                                 on-mousemove
                                 on-mouseup
                                 on-mouseenter
                                 on-mouseleave
                                 on-mouseover
                                 on-mouseout
                                 on-pointerdown
                                 on-pointermove
                                 on-pointerup
                                 on-pointerenter
                                 on-pointerleave
                                 on-pointerover
                                 on-pointerout
                                 on-pointercancel
                                 on-gotpointercapture
                                 on-lostpointercapture
                                 on-loadeddata
                                 on-loadedmetadata
                                 on-canplay
                                 on-canplaythrough
                                 on-play
                                 on-playing
                                 on-pause
                                 on-ended
                                 on-timeupdate
                                 on-volumechange))))
  ;; `#:content-mode text-or-children` is for primitive HTML elements such as
  ;; `Button` that preserve the old single text-like content form while also
  ;; accepting ordered mixed content sequences like a real container element.
  (syntax-case stx ()
    [(_ name base fixed ...
        #:required-keywords (required-kw ...)
        #:required-any-keywords (required-any-kw ...)
        #:content-mode content-mode)
     (identifier? #'name)
     (let* ([base-sym (syntax-e #'base)]
            [fixed-list (syntax->list #'(fixed ...))]
            [content-mode-datum (syntax-e #'content-mode)]
            [required-kws (syntax->datum #'(required-kw ...))]
            [required-any-kws (syntax->datum #'(required-any-kw ...))]
            [_ (unless (eq? content-mode-datum 'text-or-children)
                 (raise-syntax-error who
                                     "expected `text-or-children` for #:content-mode"
                                     stx
                                     #'content-mode))]
            [allowed-attrs
             (if (or (eq? base-sym 'html-element)
                     (eq? base-sym 'html-element-children))
                 (let ()
                   (unless (and fixed-list (= (length fixed-list) 1))
                     (raise-syntax-error who
                                         "html-element wrappers must provide exactly one fixed tag literal"
                                         stx
                                         #'(fixed ...)))
                   (define fixed0 (car fixed-list))
                   (syntax-case fixed0 (quote)
                     [(quote tag-sym)
                      (if (symbol? (syntax-e #'tag-sym))
                          (allowed-attrs-for-html-tag stx (syntax-e #'tag-sym))
                          (raise-syntax-error who
                                              "expected quoted symbol tag in html-element wrapper"
                                              stx
                                              fixed0))]
                     [_
                      (raise-syntax-error who
                                          "expected quoted symbol tag in html-element wrapper"
                                          stx
                                          fixed0)]))
                 #f)])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [html-element-id (datum->syntax #'base 'html-element #'base #'base)]
                     [html-element-children-id (datum->syntax #'base 'html-element-children #'base #'base)]
                     [content-item->child-view-id (datum->syntax #'base 'content-item->child-view/internal #'base #'base)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [required-kws-stx (datum->syntax stx (list 'quote required-kws) stx stx)]
                     [required-any-kws-stx (datum->syntax stx (list 'quote required-any-kws) stx stx)])
         #'(define (name . all-args)
             (define-values (positional attrs)
               (normalize-element-call 'name/sym
                                       all-args
                                       'any
                                       allowed-attrs-stx
                                       required-kws-stx
                                       required-any-kws-stx))
             (when (null? positional)
               (error 'name/sym
                      "wrong number of positional arguments (expected at least 1, got 0)"))
             (if (and (= (length positional) 1)
                      (not (view? (car positional))))
                 (html-element-id fixed ...
                                  (car positional)
                                  #:attrs attrs)
                 (apply html-element-children-id
                        (append (list fixed ...)
                                (map (lambda (item)
                                       (content-item->child-view-id 'name/sym item))
                                     positional)
                                (list #:attrs attrs)))))))]
    [(_ name base fixed ...
        #:required-keywords (required-kw ...)
        #:content-mode content-mode)
     (identifier? #'name)
     (let* ([base-sym (syntax-e #'base)]
            [fixed-list (syntax->list #'(fixed ...))]
            [content-mode-datum (syntax-e #'content-mode)]
            [required-kws (syntax->datum #'(required-kw ...))]
            [_ (unless (eq? content-mode-datum 'text-or-children)
                 (raise-syntax-error who
                                     "expected `text-or-children` for #:content-mode"
                                     stx
                                     #'content-mode))]
            [allowed-attrs
             (if (or (eq? base-sym 'html-element)
                     (eq? base-sym 'html-element-children))
                 (let ()
                   (unless (and fixed-list (= (length fixed-list) 1))
                     (raise-syntax-error who
                                         "html-element wrappers must provide exactly one fixed tag literal"
                                         stx
                                         #'(fixed ...)))
                   (define fixed0 (car fixed-list))
                   (syntax-case fixed0 (quote)
                     [(quote tag-sym)
                      (if (symbol? (syntax-e #'tag-sym))
                          (allowed-attrs-for-html-tag stx (syntax-e #'tag-sym))
                          (raise-syntax-error who
                                              "expected quoted symbol tag in html-element wrapper"
                                              stx
                                              fixed0))]
                     [_
                      (raise-syntax-error who
                                          "expected quoted symbol tag in html-element wrapper"
                                          stx
                                          fixed0)]))
                 #f)])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [html-element-id (datum->syntax #'base 'html-element #'base #'base)]
                     [html-element-children-id (datum->syntax #'base 'html-element-children #'base #'base)]
                     [content-item->child-view-id (datum->syntax #'base 'content-item->child-view/internal #'base #'base)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [required-kws-stx (datum->syntax stx (list 'quote required-kws) stx stx)])
         #'(define (name . all-args)
             (define-values (positional attrs)
               (normalize-element-call 'name/sym
                                       all-args
                                       'any
                                       allowed-attrs-stx
                                       required-kws-stx
                                       '()))
             (when (null? positional)
               (error 'name/sym
                      "wrong number of positional arguments (expected at least 1, got 0)"))
             (if (and (= (length positional) 1)
                      (not (view? (car positional))))
                 (html-element-id fixed ...
                                  (car positional)
                                  #:attrs attrs)
                 (apply html-element-children-id
                        (append (list fixed ...)
                                (map (lambda (item)
                                       (content-item->child-view-id 'name/sym item))
                                     positional)
                                (list #:attrs attrs)))))))]
    [(_ name base fixed ...
        #:content-mode content-mode)
     (identifier? #'name)
     (let* ([base-sym (syntax-e #'base)]
            [fixed-list (syntax->list #'(fixed ...))]
            [content-mode-datum (syntax-e #'content-mode)]
            [_ (unless (eq? content-mode-datum 'text-or-children)
                 (raise-syntax-error who
                                     "expected `text-or-children` for #:content-mode"
                                     stx
                                     #'content-mode))]
            [allowed-attrs
             (if (or (eq? base-sym 'html-element)
                     (eq? base-sym 'html-element-children))
                 (let ()
                   (unless (and fixed-list (= (length fixed-list) 1))
                     (raise-syntax-error who
                                         "html-element wrappers must provide exactly one fixed tag literal"
                                         stx
                                         #'(fixed ...)))
                   (define fixed0 (car fixed-list))
                   (syntax-case fixed0 (quote)
                     [(quote tag-sym)
                      (if (symbol? (syntax-e #'tag-sym))
                          (allowed-attrs-for-html-tag stx (syntax-e #'tag-sym))
                          (raise-syntax-error who
                                              "expected quoted symbol tag in html-element wrapper"
                                              stx
                                              fixed0))]
                     [_
                      (raise-syntax-error who
                                          "expected quoted symbol tag in html-element wrapper"
                                          stx
                                          fixed0)]))
                 #f)])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [html-element-id (datum->syntax #'base 'html-element #'base #'base)]
                     [html-element-children-id (datum->syntax #'base 'html-element-children #'base #'base)]
                     [content-item->child-view-id (datum->syntax #'base 'content-item->child-view/internal #'base #'base)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)])
         #'(define (name . all-args)
             (define-values (positional attrs)
               (normalize-element-call 'name/sym
                                       all-args
                                       'any
                                       allowed-attrs-stx
                                       '()
                                       '()))
             (when (null? positional)
               (error 'name/sym
                      "wrong number of positional arguments (expected at least 1, got 0)"))
             (if (and (= (length positional) 1)
                      (not (view? (car positional))))
                 (html-element-id fixed ...
                                  (car positional)
                                  #:attrs attrs)
                 (apply html-element-children-id
                        (append (list fixed ...)
                                (map (lambda (item)
                                       (content-item->child-view-id 'name/sym item))
                                     positional)
                                (list #:attrs attrs)))))))]
    [(_ name base fixed ...
        #:required-keywords (required-kw ...)
        #:required-any-keywords (required-any-kw ...)
        #:positional-count positional-count)
     (identifier? #'name)
     (let* ([base-sym (syntax-e #'base)]
            [fixed-list (syntax->list #'(fixed ...))]
            [required-kws (syntax->datum #'(required-kw ...))]
            [required-any-kws (syntax->datum #'(required-any-kw ...))]
            [count-datum/raw (syntax-e #'positional-count)]
            [count-datum (if (eq? count-datum/raw 'any)
                             'any
                             count-datum/raw)]
            [_ (unless (or (eq? count-datum 'any)
                           (and (integer? count-datum) (>= count-datum 0)))
                 (raise-syntax-error who
                                     "expected non-negative integer or `any` for #:positional-count"
                                     stx
                                     #'positional-count))]
            [allowed-attrs
             (if (or (eq? base-sym 'html-element)
                     (eq? base-sym 'html-element-children))
                 (let ()
                   (unless (and fixed-list (= (length fixed-list) 1))
                     (raise-syntax-error who
                                         "html-element wrappers must provide exactly one fixed tag literal"
                                         stx
                                         #'(fixed ...)))
                   (define fixed0 (car fixed-list))
                   (syntax-case fixed0 (quote)
                     [(quote tag-sym)
                      (if (symbol? (syntax-e #'tag-sym))
                          (allowed-attrs-for-html-tag stx (syntax-e #'tag-sym))
                          (raise-syntax-error who
                                              "expected quoted symbol tag in html-element wrapper"
                                              stx
                                              fixed0))]
                     [_
                      (raise-syntax-error who
                                          "expected quoted symbol tag in html-element wrapper"
                                          stx
                                          fixed0)]))
                 #f)])
       (define count-expr-datum
         (if (eq? count-datum 'any)
             '(quote any)
             count-datum))
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [required-kws-stx (datum->syntax stx (list 'quote required-kws) stx stx)]
                     [required-any-kws-stx (datum->syntax stx (list 'quote required-any-kws) stx stx)]
                     [expected-count-stx (datum->syntax stx count-expr-datum stx stx)])
         (cond
           [(and (eq? base-sym 'html-element) (equal? count-datum 0))
            #'(define (name . all-args)
                (define-values (positional attrs)
                  (normalize-element-call 'name/sym
                                          all-args
                                          expected-count-stx
                                          allowed-attrs-stx
                                          required-kws-stx
                                          required-any-kws-stx))
                (base fixed ...
                      ""
                      #:attrs attrs))]
           [else
            #'(define (name . all-args)
                (define-values (positional attrs)
                  (normalize-element-call 'name/sym
                                          all-args
                                          expected-count-stx
                                          allowed-attrs-stx
                                          required-kws-stx
                                          required-any-kws-stx))
                (apply base
                       (append (list fixed ...)
                               positional
                               (list #:attrs attrs))))])))]
    [(_ name base fixed ...
        #:required-keywords (required-kw ...)
        #:positional-count positional-count)
     (identifier? #'name)
     (let* ([base-sym (syntax-e #'base)]
            [fixed-list (syntax->list #'(fixed ...))]
            [required-kws (syntax->datum #'(required-kw ...))]
            [count-datum/raw (syntax-e #'positional-count)]
            [count-datum (if (eq? count-datum/raw 'any)
                             'any
                             count-datum/raw)]
            [_ (unless (or (eq? count-datum 'any)
                           (and (integer? count-datum) (>= count-datum 0)))
                 (raise-syntax-error who
                                     "expected non-negative integer or `any` for #:positional-count"
                                     stx
                                     #'positional-count))]
            [allowed-attrs
             (if (or (eq? base-sym 'html-element)
                     (eq? base-sym 'html-element-children))
                 (let ()
                   (unless (and fixed-list (= (length fixed-list) 1))
                     (raise-syntax-error who
                                         "html-element wrappers must provide exactly one fixed tag literal"
                                         stx
                                         #'(fixed ...)))
                   (define fixed0 (car fixed-list))
                   (syntax-case fixed0 (quote)
                     [(quote tag-sym)
                      (if (symbol? (syntax-e #'tag-sym))
                          (allowed-attrs-for-html-tag stx (syntax-e #'tag-sym))
                          (raise-syntax-error who
                                              "expected quoted symbol tag in html-element wrapper"
                                              stx
                                              fixed0))]
                     [_
                      (raise-syntax-error who
                                          "expected quoted symbol tag in html-element wrapper"
                                          stx
                                          fixed0)]))
                 #f)])
       (define count-expr-datum
         (if (eq? count-datum 'any)
             '(quote any)
             count-datum))
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)]
                     [required-kws-stx (datum->syntax stx (list 'quote required-kws) stx stx)]
                     [expected-count-stx (datum->syntax stx count-expr-datum stx stx)])
         (cond
           [(and (eq? base-sym 'html-element) (equal? count-datum 0))
            #'(define (name . all-args)
                (define-values (positional attrs)
                  (normalize-element-call 'name/sym
                                          all-args
                                          expected-count-stx
                                          allowed-attrs-stx
                                          required-kws-stx
                                          '()))
                (base fixed ...
                      ""
                      #:attrs attrs))]
           [else
            #'(define (name . all-args)
                (define-values (positional attrs)
                  (normalize-element-call 'name/sym
                                          all-args
                                          expected-count-stx
                                          allowed-attrs-stx
                                          required-kws-stx
                                          '()))
                (apply base
                       (append (list fixed ...)
                               positional
                               (list #:attrs attrs))))])))]
    [(_ name base fixed ...)
     (identifier? #'name)
     (let* ([base-sym (syntax-e #'base)]
            [fixed-list (syntax->list #'(fixed ...))]
            [allowed-attrs
             (if (or (eq? base-sym 'html-element)
                     (eq? base-sym 'html-element-children))
                 (let ()
                   (unless (and fixed-list (= (length fixed-list) 1))
                     (raise-syntax-error who
                                         "html-element wrappers must provide exactly one fixed tag literal"
                                         stx
                                         #'(fixed ...)))
                   (define fixed0 (car fixed-list))
                   (syntax-case fixed0 (quote)
                     [(quote tag-sym)
                      (if (symbol? (syntax-e #'tag-sym))
                          (allowed-attrs-for-html-tag stx (syntax-e #'tag-sym))
                          (raise-syntax-error who
                                              "expected quoted symbol tag in html-element wrapper"
                                              stx
                                              fixed0))]
                     [_
                     (raise-syntax-error who
                                          "expected quoted symbol tag in html-element wrapper"
                                          stx
                                          fixed0)]))
                 #f)])
       (with-syntax ([name/sym (datum->syntax #'name (syntax-e #'name) #'name #'name)]
                     [allowed-attrs-stx (datum->syntax stx (list 'quote allowed-attrs) stx stx)])
         #'(define (name . all-args)
             (define-values (positional attrs)
               (normalize-element-call 'name/sym all-args 1 allowed-attrs-stx '() '()))
             (base fixed ...
                   (car positional)
                   #:attrs attrs))))]
    [_
     (raise-syntax-error who
                         "expected `(define/element Name Base fixed-positional ...)`"
                         stx)]))
