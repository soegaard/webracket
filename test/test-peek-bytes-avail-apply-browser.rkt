#lang webracket/base

;; Browser regression for dynamically applying a finite-rest port primitive.
;; The procedure object must select its specialized invoker rather than the
;; fixed-arity fallback, which cannot decode a reflected arity range. The
;; custom port mirrors the lazy regexp input ports that exercise this path.

(define string-buffer (make-bytes 1))
(define string-input (open-input-string "x"))
(define string-actual
  (apply peek-bytes-avail! (list string-buffer 0 #f string-input)))

(define custom-source #"x")
(define custom-input
  (make-input-port
   'peek-bytes-avail-browser
   (lambda (_dest) eof)
   (lambda (dest skip _evt)
     (if (= skip 0)
         (begin
           (bytes-copy! dest 0 custom-source)
           1)
         eof))
   (lambda () (void))))
(define custom-buffer (make-bytes 1))
(define custom-actual
  (apply peek-bytes-avail! (list custom-buffer 0 #f custom-input)))

(js-log
 (if (and (equal? string-actual 1)
          (equal? string-buffer #"x")
          (equal? custom-actual 1)
          (equal? custom-buffer #"x"))
     "peek-bytes-avail-apply-browser-ok"
     "peek-bytes-avail-apply-browser-failed"))
