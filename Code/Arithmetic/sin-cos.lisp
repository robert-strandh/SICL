(cl:in-package #:sicl-arithmetic)

;;; This code is highly experimental, and I am not sure that it will
;;; be any better than other techniques.  Also, some of the
;;; computations made here are much better done on the binary
;;; representation of the floating-point numbers.

;;; We use the SIN and COS functions to fill the tables, but that is
;;; obviously wrong.  Some other methods must be used.

(defparameter *significant-bits* 7)

;;; This is the number of fractions that a radian is divided into.
(defparameter *radian-fractions-count*
  (ash 2 *significant-bits*))

;;; This is the size of the table that we need in order to represent
;;; fractions of radians from 0 below pi/4.
(defparameter *table-length*
  (truncate (* *radian-fractions-count* (/ pi 4d0))))

;;; This function computes a base angle from ANGLE.  The base angle is
;;; the largest value that is smaller than or equal to ANGLE and that
;;; can be expressed as (/ N *RADIAN-FRACTION-COUNT*) where N is an
;;; integer.  Normally, this function would be computed by
;;; manipulating the bits of the floating-point representation of
;;; ANGLE.  The angle is assumed to be less than or equal to pi/4.
(defun base-angle (angle)
  (/ (float (floor (* angle *radian-fractions-count*)) 1d0)
     (float *radian-fractions-count* 1d0)))
