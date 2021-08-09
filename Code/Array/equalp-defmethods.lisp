(cl:in-package #:sicl-array)

(defmethod equalp ((x vector) (y vector))
  (and (= (length x) (length y))
       (every #'equalp x y)))

(defmethod equalp ((x array) (y array))
  (and (equal (array-dimensions x) (array-dimensions y))
       (loop for i from 0 below (array-total-size x)
             always (equalp (row-major-aref x i)
                            (row-major-aref y i)))))
