(cl:in-package #:sicl-loop)

;;; This function is called in a SUM clause in order to sum the
;;; accumulated value with the new one.
(defun sum (x y)
  (unless (numberp y)
    (error 'sum-argument-must-be-number
	   :datum y))
	   :expected-type 'number
  (+ x y))
