#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system)

;; test-callback-source-map : -> void
;;   Ensure callback debug ids resolve to the user source form, not compiler wrappers.
(define (test-callback-source-map)
  (define repo-root
    (simplify-path (build-path (current-directory) "..")))
  (define webracket-rkt (build-path repo-root "webracket.rkt"))
  (define standard-ffi "ffi/standard.ffi")

  (define (run-command cwd . args)
    (define stdout (open-output-string))
    (define stderr (open-output-string))
    (define status
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr]
                     [current-directory cwd])
        (apply system*/exit-code args)))
    (values status
            (string-append (get-output-string stdout)
                           (get-output-string stderr))))

  (define (find-entry-by-form map-path needle)
    (define lines (file->lines map-path))
    (define current '())
    (define in-block? #f)
    (define found #f)
    (define (flush!)
      (when (and in-block? (not found))
        (define block (reverse current))
        (define joined (string-join block "\n"))
        (define label-line
          (for/first ([line (in-list block)]
                      #:when (regexp-match? #px"^  \"[^\"]+\"$" line))
            line))
        (define label-match
          (and label-line
               (regexp-match #px"^  \"([^\"]+)\"$" label-line)))
        (define src-line
          (for/first ([line (in-list block)]
                      #:when (regexp-match? #px"^  \\(src .+ [0-9]+ [0-9]+ [0-9]+\\)$" line))
            line))
        (define src-match
          (and src-line
               (regexp-match #px"^  \\(src (.+) ([0-9]+) ([0-9]+) ([0-9]+)\\)$"
                             src-line)))
        (when (and label-match src-match (string-contains? joined needle))
          (set! found
                (hash 'label (second label-match)
                      'src (second src-match)
                      'line (list-ref src-match 2)
                      'col (list-ref src-match 3)
                      'span (list-ref src-match 4)
                      'block joined))))
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
    found)

  (define src-path (string->path "/tmp/webracket-callback-source-map-test.rkt"))
  (define src-base (path-replace-extension src-path #""))
  (define map-path (path-add-extension src-base ".wasm.map.sexp"))
  (define generated-paths
    (list (path-add-extension src-base ".wat")
          (path-add-extension src-base ".wasm")
          (path-add-extension src-base ".js")
          map-path))

  (dynamic-wind
    void
    (λ ()
      (call-with-output-file src-path
        (λ (out)
          (displayln "(define callback" out)
          (displayln "  (procedure->external" out)
          (displayln "   (procedure-rename" out)
          (displayln "    (lambda ()" out)
          (displayln "      'callback-source-sentinel)" out)
          (displayln "    'pump-init!)))" out)
          (displayln "callback" out))
        #:exists 'truncate/replace)

      (define-values (compile-status compile-output)
        (run-command repo-root
                     (find-executable-path "racket")
                     (path->string webracket-rkt)
                     "--ffi"
                     standard-ffi
                     (path->string src-path)))
      (unless (zero? compile-status)
        (error 'test-callback-source-map
               (format "callback repro failed (~a): ~a"
                       compile-status
                       compile-output)))
      (unless (file-exists? map-path)
        (error 'test-callback-source-map
               (format "expected source map to exist: ~a" map-path)))

      (define entry
        (find-entry-by-form
         map-path
         "(lambda () (quote callback-source-sentinel))"))
      (unless entry
        (error 'test-callback-source-map
               (format "expected callback closure label in source map: ~a"
                       map-path)))

      (define src-token (hash-ref entry 'src))
      (define normalized-src
        (string-trim
         (if (regexp-match? #px"^#<path:.*>$" src-token)
             (second (regexp-match #px"^#<path:(.*)>$" src-token))
             src-token)
         "\""))
      (unless (string=? normalized-src (path->string src-path))
        (error 'test-callback-source-map
               (format "expected callback source to point at temp source file, got: ~a"
                       (hash-ref entry 'block))))
      (when (string-contains? (hash-ref entry 'block) "compiler.rkt:")
        (error 'test-callback-source-map
               (format "expected callback source to avoid compiler wrapper site, got: ~a"
                       (hash-ref entry 'block))))
      (unless (string-contains? (hash-ref entry 'block)
                                "(lambda () (quote callback-source-sentinel))")
        (error 'test-callback-source-map
               (format "expected recorded form to include the user lambda, got: ~a"
                       (hash-ref entry 'block)))))
    (λ ()
      (for ([p (in-list generated-paths)])
        (with-handlers ([exn:fail:filesystem? void])
          (when (file-exists? p)
            (delete-file p))))
      (with-handlers ([exn:fail:filesystem? void])
        (delete-file src-path)))))

(module+ main
  (test-callback-source-map)
  (displayln "ok"))
