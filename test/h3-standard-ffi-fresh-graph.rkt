#lang webracket/base

(require (only-in webracket/unsafe/standard js-eval))

(define h3-standard-ffi-fresh-graph-sentinel
  (begin
    (js-eval "0;")
    0))
