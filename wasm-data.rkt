#lang racket
(provide wasm-data)

;;;
;;; Pretty Write and Byte Strings
;;;

;; The driver uses `pretty-write` to output the generated
;; code to a text file. The way Racket writes a byte string
;; doesn't match the grammar used by Web Assembly.

;; Wrapping a byte string in the `wasm-data` struct,
;; creates a value that prints correctly.

(require racket/pretty)

;; format-byte : byte? -> string?
;; Returns a WAT data-string fragment representing exactly that byte.
(define (format-byte b)
  (unless (byte? b)
    (raise-argument-error 'format-byte "byte?" b))
  (define (hex2 n)
    (define s (string-downcase (number->string n 16)))
    (if (= (string-length s) 1) (string-append "0" s) s))
  (define (hex-escape n) (string-append "\\" (hex2 n)))
  (case b
    [(9)  "\\t"]                      ; tab
    [(10) "\\n"]                      ; line feed
    [(13) "\\r"]                      ; carriage return
    [(34) "\\\""]                     ; double quote
    [(39) "\\'"]                      ; single quote
    [(92) "\\\\"]                     ; backslash
    [else
     (cond
       [(and (<= 32 b) (<= b 126) (not (member b '(34 39 92))))
        (string (integer->char b))]   ; printable ASCII => itself
       [else
        (hex-escape b)])]))           ; everything else => \xx


(define (data-print bytes port mode)
  ; mode : ; #t = write, #f = display, 0/1 = print
  (case mode
    [(0 1 #t #f)
     (write-string
      (string-append "\""
                     (string-append*
                      (for/list ([b bytes])
                        (format-byte b)))
                     "\"")
      port)]
    [else
     (write-string "huh?" port)])
  (void))



(struct wasm-data (ref) #:methods gen:custom-write
  [(define (write-proc obj port mode)
     (data-print (wasm-data-ref obj) port mode))]) 

;;;
;;; Test
;;;

;; (data-print #"bar" (current-output-port) #t)
;; (pretty-write (wasm-data #"hé"))
