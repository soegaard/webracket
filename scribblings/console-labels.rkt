#lang racket/base

(require racket/contract/base)

;; This module provides docs-only fake bindings for console Scribble links.
;;
;; Console bindings are implemented outside ordinary Racket modules, so
;; Scribble needs label-only bindings to make @racketblock examples link
;; to the documented identifiers.

(provide
  console-log
  console-info
  console-warn
  console-error
  console-debug
  console-assert
  console-clear
  console-dir
  console-dirxml
  console-table
  console-group
  console-group-collapsed
  console-group-end
  console-count
  console-count-reset
  console-time
  console-time-end
  console-time-log
  console-time-stamp
  console-trace
  console-profile
  console-profile-end
  console-exception
  js-console-log
  js-console-info
  js-console-warn
  js-console-error
  js-console-debug
  js-console-assert
  js-console-clear
  js-console-dir
  js-console-dirxml
  js-console-table
  js-console-group
  js-console-group-collapsed
  js-console-group-end
  js-console-count
  js-console-count-reset
  js-console-time
  js-console-time-end
  js-console-time-log
  js-console-time-stamp
  js-console-trace
  js-console-profile
  js-console-profile-end
  js-console-exception
  (for-label (all-defined-out)))

(define console-log any/c)
(define console-info any/c)
(define console-warn any/c)
(define console-error any/c)
(define console-debug any/c)
(define console-assert any/c)
(define console-clear any/c)
(define console-dir any/c)
(define console-dirxml any/c)
(define console-table any/c)
(define console-group any/c)
(define console-group-collapsed any/c)
(define console-group-end any/c)
(define console-count any/c)
(define console-count-reset any/c)
(define console-time any/c)
(define console-time-end any/c)
(define console-time-log any/c)
(define console-time-stamp any/c)
(define console-trace any/c)
(define console-profile any/c)
(define console-profile-end any/c)
(define console-exception any/c)
(define js-console-log any/c)
(define js-console-info any/c)
(define js-console-warn any/c)
(define js-console-error any/c)
(define js-console-debug any/c)
(define js-console-assert any/c)
(define js-console-clear any/c)
(define js-console-dir any/c)
(define js-console-dirxml any/c)
(define js-console-table any/c)
(define js-console-group any/c)
(define js-console-group-collapsed any/c)
(define js-console-group-end any/c)
(define js-console-count any/c)
(define js-console-count-reset any/c)
(define js-console-time any/c)
(define js-console-time-end any/c)
(define js-console-time-log any/c)
(define js-console-time-stamp any/c)
(define js-console-trace any/c)
(define js-console-profile any/c)
(define js-console-profile-end any/c)
(define js-console-exception any/c)
