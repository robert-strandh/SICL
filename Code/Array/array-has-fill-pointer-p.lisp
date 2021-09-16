(cl:in-package #:sicl-array)

(defgeneric array-has-fill-pointer-p (array))

(defmethod array-has-fill-pointer-p ((array array))
  nil)

(defmethod array-has-fill-pointer-p ((array vector))
  (not (null (vector-fill-pointer array))))

