(cl:in-package #:sicl-arithmetic)

(defun logorc1 (integer-1 integer-2)
  (logior integer-1 (lognot integer-2)))
