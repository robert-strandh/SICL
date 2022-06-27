(cl:in-package #:sicl-arithmetic)

;;; This code is highly experimental, and I am not sure that it will
;;; be any better than other techniques.  Also, some of the
;;; computations made here are much better done on the binary
;;; representation of the floating-point numbers.

;;; The basic idea is as follows.  We use the well known identity:
;;; (= (SIN (+ X Y) (+ (* (SIN X) (COS Y)) (* (COS X) (SIN Y)))))

;;; X is called the BASE ANGLE and Y is called the SMALL ANGLE.  We
;;; take an arbitrary angle (<= 0 ANGLE (/ PI 4)) and we split it so
;;; that X is a multiple of an integer fraction of a radian.  The
;;; integer is a power of 2, so in our case, the base angle is a
;;; multiple of 1/128 of a radian.  The base angle can be determined
;;; from the floating-point value of the angle by the use of integer
;;; bit manipulation of the components of the value.  The difference
;;; between the original angle and X is then small.  The values of
;;; (SIN X) and (COS X) are kept in two tables that are relatively
;;; small, and one can hope that they will be in the cache if an
;;; application uses the SIN and COS functions frequently.  The values
;;; for (SIN Y) and (COS Y) are computed using an approximation
;;; polynomial.  Here, I just use the first few terms of the Taylor
;;; expansion, but that's just because I haven't yet studied how to
;;; get something optimal.  It might also be better to increase the
;;; size of the tables if that means a lower degree of the
;;; approximation polynomials.

;;; The total cost for the formula above is 2 multiplications and 1
;;; addition.  To that should be added the computation of the
;;; approximation polynomials.  In this experiment, the approximations
;;; cost 4 multiplications and 2 or 3 additions, but we may be able to
;;; do better with lower-degree polynomials.  So we have a total of 6
;;; multiplications and 3 or 4 additions.

(defparameter *significant-bits* 7)

;;; This is the number of fractions that a radian is divided into.
(defparameter *radian-fractions-count*
  (ash 1 *significant-bits*))

;;; This is the size of the table that we need in order to represent
;;; fractions of radians from 0 below pi/4.
(defparameter *table-length*
  (truncate (* *radian-fractions-count* (/ pi 4d0))))

(defun radian-fraction-number (angle)
  (floor (* angle *radian-fractions-count*)))

;;; This function computes a base angle from ANGLE.  The base angle is
;;; the largest value that is smaller than or equal to ANGLE and that
;;; can be expressed as (/ N *RADIAN-FRACTION-COUNT*) where N is an
;;; integer.  Normally, this function would be computed by
;;; manipulating the bits of the floating-point representation of
;;; ANGLE.  The angle is assumed to be less than or equal to pi/4.
(defun base-angle (radian-fraction-number)
  (/ (float radian-fraction-number)
     (float *radian-fractions-count* 1d0)))

;;; This table contains values for (SIN X) where X is a base angle
;;; between 0 and pi/4.  We currently use the host SIN function to
;;; fill the table, but that is obviously going to change.
(defparameter *sin-table*
  (let ((result (make-array *table-length* :element-type 'double-float)))
    (loop for i from 0 below *table-length*
          for angle = (float (/ i *radian-fractions-count*) 1d0)
          do (setf (aref result i)
                   (sin angle)))
    result))

;;; This table contains values for (COS X) where X is a base angle
;;; between 0 and pi/4.  We currently use the host COS function to
;;; fill the table, but that is obviously going to change.
(defparameter *cos-table*
  (let ((result (make-array *table-length* :element-type 'double-float)))
    (loop for i from 0 below *table-length*
          for angle = (float (/ i *radian-fractions-count*) 1d0)
          do (setf (aref result i)
                   (cos angle)))
    result))

;;; Compute an approximation of (SIN ANGLE) for small values of ANGLE.
;;; By small, we mean less than (/ *RADIAN-FRACTIONS-COUNT*).  For
;;; now, we do the straightforward thing of using the first few terms
;;; in the Taylor series.  Later, figure out a better polynomial,
;;; perhaps with a lower degree than we use here.
(defun small-sin (angle)
  (assert (< angle (float (/ *radian-fractions-count*) 1d0)))
  (let* ((x2 (* angle angle))
         (t1 (- (/ 6d0) (/ x2 120d0)))
         (t2 (- 1d0 (* x2 t1))))
    (* angle t2)))

;;; Compute an approximation of (COS ANGLE) for small values of ANGLE.
(defun small-cos (angle)
  (assert (< angle (float (/ *radian-fractions-count*) 1d0)))
  (let* ((x2 (* angle angle))
         (t1 (- (/ 24d0) (/ x2 720d0)))
         (t2 (- (/ 2d0) (* x2 t1))))
    (- 1d0 (* x2 t2))))

;;; Compute (SIN X) where X is between 0 and pi/4.
(defun dsin (angle)
  (let* ((radian-fraction-number (radian-fraction-number angle))
         (base-angle (base-angle radian-fraction-number))
         (small-angle (- angle base-angle)))
    (+ (* (aref *sin-table* radian-fraction-number)
          (small-cos small-angle))
       (* (aref *cos-table* radian-fraction-number)
          (small-sin small-angle)))))
