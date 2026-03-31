;; download-calendar.rkt
;;
;; WebRacket: download an .ics file via window.fetch and show it in the page.

;; Build:
;;   racket ../../../../webracket.rkt -b --ffi standard --ffi dom download-calendar.rkt

(define URL "https://better-f1-calendar.vercel.app/api/calendar.ics")

;; Make a <pre> element and attach it to document.body.
(define (make-output-pre!)
  (define doc  (js-window-document))
  (define body (js-document-body))

  (define pre (js-create-element "pre"))
  (js-set-attribute! pre "style"
                     (string-append
                      "white-space: pre-wrap;"
                      "overflow-wrap: anywhere;"
                      "font-family: ui-monospace, SFMono-Regular, Menlo, Monaco,"
                      " Consolas, 'Liberation Mono', 'Courier New', monospace;"
                      "font-size: 12px;"
                      "padding: 12px;"))
  (js-set! pre "textContent" "Downloading calendar.ics …")

  (js-append-child! body pre)
  pre)

(define out (make-output-pre!))

(define (set-output! s)
  (js-set! out "textContent" s)
  (void))

(define (set-error! e)
  (set-output! (string-append "Download failed:\n" (format "~a" e))))

;; download-ics : (string -> void) (any -> void) -> void
(define (download-ics on-ok on-err)
  (define fetch-promise
    (js-window-fetch URL (void))) ; (void) = missing init/options

  ;; fetch(...).then(resp => resp.text()).then(text => on-ok(text)).catch(on-err)
  (define text-promise
    (js-send/extern fetch-promise "then"
                    (lambda (resp)
                      (js-send/extern resp "text" (void)))))

  (js-send text-promise "then" on-ok)
  (js-send text-promise "catch" on-err)
  (void))

(download-ics
 (lambda (ics)
   (set-output! ics))
 (lambda (e)
   (set-error! e)))
