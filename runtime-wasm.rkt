        (func $raise-argument-error  (param $x (ref eq)) (unreachable))
        (func $raise-expected-fixnum (param $x (ref eq)) (unreachable))

        ;; Like Racket's `match:error` but since WebRacket does not
        ;; yet support exceptions, this simply traps.
        (func $match:error
              (param $val      (ref eq))   ;; any value that failed to match
              (param $srclocs  (ref eq))   ;; (listof srcloc?)
              (param $form-name (ref eq))  ;; symbol naming the match form
              (unreachable))
