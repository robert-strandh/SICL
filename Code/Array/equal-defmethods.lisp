(cl:in-package #:sicl-array)

;;; We might want to do this one more efficiently.
(defmethod equal ((x bit-vector) (y bit-vector))
  (and (= (length x) (length y))
       (every #'eql x y)))
