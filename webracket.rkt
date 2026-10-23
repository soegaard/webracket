#lang racket/base

(require racket/runtime-path)

;; Keep the repository root as an executable entrypoint, not a collection root.
;; The checkout-local launcher exposes `pkgs/` while loading the real CLI.
(define-runtime-path package-parent "pkgs")
(define-runtime-path cli-path "pkgs/webracket-lib/compiler/cli.rkt")

(define collection-paths
  (current-library-collection-paths))

;; The CLI is often launched with this checkout's `pkgs/` directory already
;; present in PLTCOLLECTS. Do not install a second copy: duplicate collection
;; roots can make the module extractor resolve one provider through two paths.
(define normalized-package-parent
  (simplify-path (path->complete-path package-parent)))
(define package-parent-present?
  (for/or ([collection-path (in-list collection-paths)])
    (equal? normalized-package-parent
            (simplify-path (path->complete-path collection-path)))))
(define launcher-collection-paths
  (if package-parent-present?
      collection-paths
      (cons package-parent collection-paths)))

(parameterize ([current-library-collection-paths launcher-collection-paths])
  (dynamic-require cli-path #f))
