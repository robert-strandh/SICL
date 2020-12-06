(cl:in-package #:sicl-array)

(defmethod array-row-major-index (object &rest subscripts)
  (error 'argument-to-array-row-major-index-must-be-an-array
         :datum object
         :expected-type 'array))

(defmethod array-row-major-index ((array array) &rest subscripts)
  (let ((dimensions (array-dimensions array)))
    (unless (= (length dimensions) (length subscripts))
      (error 'number-of-indices-must-equal-array-rank
             :subscripts subscripts
             :array array))
    (loop for index in subscripts
          for dimension in dimensions
          for index-number from 0
          for result = index then (+ (* result dimension) index)
          unless (and (integerp index) (>= index 0) (< index dimension))
            do (error 'index-must-be-non-negative-and-less-than-dimension
                      :array array
                      :index-number index-number)
          finally (return result))))
