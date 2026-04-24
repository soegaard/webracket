#lang webracket

;;;
;;; Current Input, Output and Error Ports
;;;

;; Current port cells are runtime primitives so port defaults in runtime
;; procedures can consult the same state as stdlib code.
