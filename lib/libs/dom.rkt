#lang webracket

;;;
;;; DOM wrappers
;;;

;; This library reexports the family wrappers so `include-lib dom` gives
;; callers one Rackety entry point for the DOM surface.

(include-lib window)
(include-lib performance)
(include-lib document)
(include-lib domrect)
(include-lib element)
(include-lib canvas)
(include-lib image)
(include-lib event)
(include-lib fetch)
(include-lib storage)
(include-lib indexed-db)
