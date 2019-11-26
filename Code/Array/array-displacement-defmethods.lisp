(cl:in-package #:sicl-array)

(defmethod array-displacement (object)
  (error 'argument-to-array-displacement-must-be-an-array
         :datum object
         :expected-type 'array))

(defmethod array-displacement ((array array))
  (values nil 0))

(defmethod array-displacement ((array displaced-array))
  (values (target array) (offset array)))
