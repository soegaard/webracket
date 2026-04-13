;;;
;;; fetch.ffi
;;;

;; Focused tests for the `fetch` wrapper library.
;;
;; Build:
;;   racket -l errortrace -t ../webracket.rkt -- --browser -r test-fetch.rkt

(include-lib fetch)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal label)))

(define (check-true got label)
  (unless got
    (error 'check-true label)))

(list
 (list "Fetch wrappers"
       (let ()
         (define req (make-fetch-request "https://example.com/fetch-test"))
         (define hdrs (make-fetch-headers))
         (fetch-headers-set! hdrs 'x-webracket '1)
         (define resp
           (fetch-response
            (js-eval "new Response('ok', {status: 201, headers: {'x-webracket': '2'}})")))
         (check-true (fetch-request? req) "request predicate")
         (check-true (fetch-headers? hdrs) "headers predicate")
         (check-true (fetch-response? resp) "response predicate")
         (check-equal (fetch-request-url req)
                      "https://example.com/fetch-test"
                      "request url")
         (check-equal (fetch-headers-get hdrs 'x-webracket)
                      "1"
                      "header get")
         (check-true (fetch-response-ok? resp) "response ok")
         (check-equal (fetch-response-status resp) 201 "response status")
         (check-equal (fetch-response-status-text resp) "" "response status text")
         (check-equal (fetch-headers-get (fetch-response-headers resp) 'x-webracket)
                      "2"
                      "response headers")
         #t)))
