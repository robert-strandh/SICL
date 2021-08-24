(cl:in-package #:sicl-loop)

;;; This function is called in a SUM clause in order to sum the
;;; accumulated value with the new one.
(defun sum (x y)
  (unless (numberp y)
    (error 'sum-argument-must-be-number
           :datum y
           :expected-type 'number))
  (+ x y))

;;; This function is called in MAX and MIN clauses to ensure that new values
;;; are real.
(defun ensure-real (x error)
  (unless (realp x)
    (error error
           :datum x
           :expected-type 'real))
  x)

;;; This function is called in a MAX clause in order to compute the
;;; min of the accumulated value and the new one.
(defun maximize (x y)
  (ensure-real y 'max-argument-must-be-real)
  (max x y))

;;; This function is called in a MIN clause in order to compute the
;;; min of the accumulated value and the new one.
(defun minimize (x y)
  (ensure-real y 'min-argument-must-be-real)
  (min x y))
