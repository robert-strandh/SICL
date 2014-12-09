(cl:in-package #:sicl-loop)

;;; This function is called in a SUM clause in order to sum the
;;; accumulated value with the new one.
(defun sum (x y)
  (unless (numberp y)
    (error 'sum-argument-must-be-number
	   :datum y))
	   :expected-type 'number
  (+ x y))

;;; This function is called in a MAX clause in order to compute the
;;; max of the accumulated value and the new one.
(defun maximize (x y)
  (unless (realp y)
    (error 'max-argument-must-be-real
	   :datum y))
	   :expected-type 'real
  (max x y))
