(cl:in-package #:sicl-array)

(defun aref (array &rest subscripts)
  (apply #'row-major-aref array subscripts))

(defun (setf aref) (new-element array &rest subscripts)
  (apply #'(setf row-major-aref) new-element subscripts))
