(cl:in-package #:sicl-printer)

(defparameter *digits* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;;; We want to print a positive integer in any base, but we don't want
;;; to use a buffer of digits.  The basic technique we are using is
;;; that we recursively print the most significant digits first, and
;;; then the least significant digits, in a way that roughly half the
;;; digits are printed in earch recursive call.  Suppose there are D
;;; digits to print.  Then we want to split D into two integers Du and
;;; Dl, such that Du + Dl = D, and Du and Dl are roughly D/2.  The
;;; closer to D/2 we get, the fewer recursive calls we need.
;;; Furthermore, Du and Dl must both be greater than 0, or else we
;;; have an infinite recursion.
;;;
;;; Once we have Du and Dl, we can compute two numbers Nu and Nl as
;;; the values of (floor N B^Dl).  We then recursively print Nu first
;;; and then Nl.
;;;
;;; To get the number of digits in a number N, using a particular base
;;; B, we could use logarithms.  But logarithms are expensive to
;;; compute, and could be tricky to get right for bignums.  As it
;;; turns out, INTEGER-LENGTH is a reasonable approximation of the
;;; logarithm (base 2) of a number.  Let IN be the INTEGER-LENGTH of N
;;; and IB be the INTEGER-LENGTH of B.  If we divide the IN by IB, we
;;; get a rough approximation of the number of digits in N.  The
;;; problem is that for small values of B, in particular 2 and 3, the
;;; approximation is not that great.  A very good approximation for
;;; all values of B from 2 to 36 happens to be: (FLOOR (ASH IN 1) (1-
;;; (INTEGER-LENGTH (* B B)))).  By doubling IN and using the
;;; INTEGER-LENGTH of the square of the base (minus 1), we trick FLOOR
;;; into giving us a better approximation than if we simply use IN and
;;; IB.
;;;
;;; Since the computation of D is approximate, the only thing we know
;;; with absolute certainty is that Nl has exactly Dl digits.  So for
;;; Nl, we do not have to redo the computation of the number of
;;; digits.  But since Du is just D - Dl, Du is also approximate.
;;; When we print Nu, we must therefore redo the calculation of the
;;; number of digits.  This difference is reflected by two different
;;; lexical functions, one for printing upper halves and one for
;;; printing lower halves.

(defun print-positive-integer (integer base stream)
  (let ((divisor (1- (integer-length (* base base))))
        (digits *digits*))
    (labels ((upper (integer)
               (if (< integer base)
                   (princ (schar digits integer) stream)
                   (let* ((binary-length (integer-length integer))
                          (length (max 2 (floor (ash binary-length 1) divisor)))
                          (length/2 (floor length 2))
                          (split (expt base length/2)))
                     (multiple-value-bind (quotient remainder)
                         (floor integer split)
                       (upper quotient)
                       (lower remainder length/2)))))
             (lower (integer length)
               (if (= length 1)
                   (princ (schar digits integer) stream)
                   (let* ((length/2 (floor length 2))
                          (split (expt base length/2)))
                     (multiple-value-bind (quotient remainder)
                         (floor integer split)
                       (lower quotient  (- length length/2))
                       (lower remainder length/2))))))
      (upper integer))))

(defun print-integer (integer base stream)
  ;; Determine whether a radix prefix should be printed, and if so,
  ;; which one.
  (when *print-radix*
    (princ (case *print-base*
             (2 "#b")
             (8 "#o")
             (10 "")
             (16 "#x")
             (otherwise (format nil "#~dr" *print-base*)))
           stream))
  (cond ((zerop integer)
         (princ #\0 stream))
        ((minusp integer)
         (print-positive-integer (- integer) base stream))
        (t
         (print-positive-integer integer base stream)))
  ;; Determine whether a trailing dot should be printed.
  (when (and *print-radix* (= *print-base* 10))
    (princ #\. stream)))
