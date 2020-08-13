(cl:in-package #:sicl-arithmetic)

(defun lognor (integer-1 integer-2)
  (lognot (logor integer-1 integer-2)))
