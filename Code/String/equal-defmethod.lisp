(cl:in-package #:sicl-string)

(defmethod equal ((x string) (y string))
  (and (= (length x) (length y))
       (every #'eql x y)))
