(cl:in-package #:sicl-arithmetic)

(defun logorc1 (integer-1 integer-2)
  (logior (lognot integer-1) integer-2))
