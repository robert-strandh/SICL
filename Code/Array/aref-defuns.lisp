(cl:in-package #:sicl-array)

(defun aref (array &rest subscripts)
  (row-major-aref array (apply #'array-row-major-index array subscripts)))

(defun (setf aref) (new-element array &rest subscripts)
  (setf (row-major-aref array (apply #'array-row-major-index array subscripts))
        new-element))
