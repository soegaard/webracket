(list
 (list "Number functions"
       (list
        (list "js-number-finite?"
              (and (equal? (js-number-finite? 1.0) 1)
                   (equal? (js-number-finite? 0.0) 1)
                   (equal? (js-number-finite? (js-number-positive-infinity)) 0)
                   (equal? (js-number-finite? (js-number-negative-infinity)) 0)
                   (equal? (js-number-finite? (js-number-nan)) 0))))))
