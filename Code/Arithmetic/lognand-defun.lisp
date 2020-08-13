(cl:in-package #:sicl-arithmetic)

(defun lognand (integer-1 integer-2)
  (lognot (logand integer-1 integer-2)))
