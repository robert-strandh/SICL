(cl:in-package #:sicl-arithmetic)

(defun lognot (integer)
  (logxor integer -1))
