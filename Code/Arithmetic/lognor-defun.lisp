(cl:in-package #:sicl-arithmetic)

(defun lognor (integer-1 integer-2)
  (lognot (logior integer-1 integer-2)))
