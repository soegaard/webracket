#lang racket/base

;;;
;;; find-callback-source
;;;

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/string)

;; map-entry : string? path-string? exact-nonnegative-integer? exact-nonnegative-integer?
;;             exact-nonnegative-integer? (or/c string? #f) (or/c string? #f) -> hash?
;;   Build one parsed label-map entry.
(define (map-entry label src line col span same-as form)
  (hash 'label   label
        'src     src
        'line    line
        'col     col
        'span    span
        'same-as same-as
        'form    form))

;; parse-path-token : string? -> string?
;;   Decode a source token from the emitted label map.
(define (parse-path-token token)
  (define trimmed (string-trim token))
  (match (regexp-match #px"^#<path:(.*)>$" trimmed)
    [(list _ p) p]
    [_ trimmed]))

;; block->entry : (listof string?) -> (or/c hash? #f)
;;   Parse one `(label ...)` block from a `.wasm.map.sexp` file.
(define (block->entry lines)
  (define joined (string-join lines "\n"))
  (define label-line
    (for/first ([line (in-list lines)]
                #:when (regexp-match? #px"^  \"[^\"]+\"$" line))
      line))
  (define src-line
    (for/first ([line (in-list lines)]
                #:when (regexp-match? #px"^  \\(src .+ [0-9]+ [0-9]+ [0-9]+\\)$" line))
      line))
  (define label-match
    (and label-line
         (regexp-match #px"^  \"([^\"]+)\"$" label-line)))
  (define src-match
    (and src-line
         (regexp-match #px"^  \\(src (.+) ([0-9]+) ([0-9]+) ([0-9]+)\\)$" src-line)))
  (and label-match
       src-match
       (let* ([label   (second label-match)]
              [src     (parse-path-token (second src-match))]
              [line    (string->number (list-ref src-match 2))]
              [col     (string->number (list-ref src-match 3))]
              [span    (string->number (list-ref src-match 4))]
              [same-as-match
               (regexp-match #px"^  \\(same-as \"([^\"]+)\"\\)$" joined)]
              [form-match
               (regexp-match #px"(?s:  \\(form\\s*(.*?)\\)\\s*$)" joined)]
              [same-as (and same-as-match (second same-as-match))]
              [form    (and form-match
                            (string-trim (second form-match)))])
         (map-entry label src line col span same-as form))))

;; parse-label-map : path-string? -> (hash/c string? hash?)
;;   Parse a `.wasm.map.sexp` file into a label table.
(define (parse-label-map map-path)
  (define table (make-hash))
  (define lines (file->lines map-path))
  (define current '())
  (define in-block? #f)
  (define (flush!)
    (when in-block?
      (define entry (block->entry (reverse current)))
      (when entry
        (hash-set! table (hash-ref entry 'label) entry)))
    (set! current '())
    (set! in-block? #f))
  (for ([line (in-list lines)])
    (cond
      [(string=? line " (label")
       (flush!)
       (set! in-block? #t)
       (set! current (list line))]
      [in-block?
       (set! current (cons line current))]))
  (flush!)
  table)

;; format-location : hash? -> string?
;;   Render one parsed entry as `path:line:col`.
(define (format-location entry)
  (format "~a:~a:~a"
          (hash-ref entry 'src)
          (hash-ref entry 'line)
          (hash-ref entry 'col)))

;; print-entry : hash? boolean? -> void?
;;   Print a human-readable result block.
(define (print-entry entry show-form?)
  (printf "label: ~a\n" (hash-ref entry 'label))
  (printf "source: ~a\n" (format-location entry))
  (printf "span: ~a\n" (hash-ref entry 'span))
  (define same-as (hash-ref entry 'same-as))
  (when same-as
    (printf "same-as: ~a\n" same-as))
  (when show-form?
    (define form (hash-ref entry 'form))
    (when form
      (printf "form: ~a\n" form))))

(define show-form? #f)

(define-values (map-path label)
  (command-line
   #:program "find-callback-source"
   #:once-each
   [("--show-form") "Print the recorded source form when available."
                    (set! show-form? #t)]
   #:args (map-path label)
   (values map-path label)))

(define table (parse-label-map map-path))
(define entry (hash-ref table label #f))

(cond
  [entry
   (print-entry entry show-form?)
   (exit 0)]
  [else
   (eprintf "find-callback-source: label not found: ~a\n" label)
   (eprintf "  map: ~a\n" (path->string (simplify-path (path->complete-path map-path))))
   (exit 1)])
