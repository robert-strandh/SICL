(cl:in-package #:sicl-arithmetic)

(defun evenp (integer)
  (zerop (logand integer 1)))
