(displayln (string->number "AFe1" 16))
(displayln (string->number "afe1" 16))
(displayln (string->number "AFE1" 16))

(let ([out (get-output-string (current-output-port))])
  (unless (string=? out "")
    (js-log out)))
