(cl:in-package :sicl-read)

;;; We need to discover at loadtime what the best floating-point
;;; type we are going to use for conversions from decimal to
;;; internal representation.  Implementations may want to alter the
;;; value of this variable if other floating-point types are
;;; supplied.  We use the names of those variables, as opposed to
;;; their values, which would be determined at compile time.
(defparameter *most-positive-float-names*
  '(most-positive-short-float
    most-positive-single-float
    most-positive-double-float
    most-positive-long-float))

(defvar *most-positive-float*)

(makunbound '*most-positive-float*)

(eval-when (:load-toplevel)
  (setf *most-positive-float*
        (loop for name in *most-positive-float-names*
              maximize (symbol-value name))))

;;; This variable contains a positive integer such that all positive
;;; integers that are less than or equal to this one have exact
;;; representations as integer floating-point numbers.  Calling
;;; the FLOAT function on any such integer, is quaranteed to yield
;;; an exact floating-point representation of the integer.
(defvar *upper-integer-float-bound*)

(makunbound '*upper-integer-float-bound*)

(eval-when (:load-toplevel)
  (setf *upper-integer-float-bound*
        (1- (expt 2 (float-precision *most-positive-float*)))))

;;; This variable contains the largest integer n such that for all
;;; values k such that 0 <= k <= n, 10^k has an exact representation
;;; as a floating-point number.
(defvar *upper-decimal-exponent-bound*)

(makunbound '*upper-decimal-exponent-bound*)

(eval-when (:load-toplevel)
  (setf *upper-decimal-exponent-bound*
        (loop for i from 0
              while (= (rational (float (expt 10 i) *most-positive-float*))
                       (expt 10 i))
              finally (return (1- i)))))

(defvar *float-powers-of-ten*)

(makunbound '*float-powers-of-ten*)

(defvar *integer-powers-of-ten*)

(makunbound '*integer-powers-of-ten*)

(eval-when (:load-toplevel)
  (let ((float-table (make-array (1+ *upper-decimal-exponent-bound*)
                                 :element-type (type-of *most-positive-float*)))
        (int-table (make-array (1+ *upper-decimal-exponent-bound*)
                               :element-type '(integer 0))))
    (loop for i from 0 to *upper-decimal-exponent-bound*
          do (let ((value (expt 10 i)))
               (setf (aref int-table i) value)
               (setf (aref float-table i) (float value *most-positive-float*))))
    (setf *integer-powers-of-ten* int-table)
    (setf *float-powers-of-ten* float-table)))

;;; We obtain the mantissa of a floating-point decimal number as a
;;; nonnegative integer.  The exponent is represented as
;;; an integer which may be positive, zero, or negative.  If the
;;; decimal point was moved in order to turn the mantissa into an
;;; integer, then the value of the exponent has been adjusted
;;; accordingly, so that the represented number is equal to m*10^e,
;;; where m is the mantissa and e is the exponent. When both the
;;; mantissa and the exponent of a decimal floating point number have
;;; an exact representation as floating-point numbers, then it is easy
;;; and fast to obtain the resulting floating-point number by just
;;; multiplying the two together.  In particular, this is the case
;;; when both the mantissa and the exponent have exact representations
;;; as integer floating-point numbers.  This is the case when the
;;; mantissa does not have too many digits, and the absolute value of
;;; the exponent is small.  This is often the case, so we want a test
;;; for it that is quick to execute, even though it means we might
;;; miss some cases.  Clearly, if the number of the digits of the
;;; mantissa is less than or equal to n, and all integers with at most
;;; n digits have representations as integer floating-point number,
;;; then, the mantissa has a representation as an integer
;;; floating-point number.  This is a simple test, because n can be
;;; precompiled.  There are even some simple improvements we can do.
;;; Initial 0s in the mantissa are not significant, so can be removed.
;;; Trailing 0s can be removed as well and the exponent can be
;;; adjusted accordingly.

;;; Convert a decimal to a float.
;;; The sign is a floating-point number that is either
;;; numerically equivalent to 1 or to -1.
;;; The mantissa is a nonnegative integer.
;;; The exponent is an integer that may be positive, zero, or
;;; negative.

;;; This function is called as a last resort.  We construct
;;; a rational number and call.  Implement this better.
(defun fallback-decimal-to-float (sign mantissa exponent)
  (float (* sign mantissa (expt 10 exponent)) *most-positive-float*))

(declaim (inline fallback-decimal-to-float))

;;; This function is called when the mantissa is small but the
;;; exponent e is too large, in that 10^e doesn't have an exact
;;; representation as a floating-point integer.  But perhaps the
;;; mantissa is *so* small that there is an integer k > 0 so that
;;; 10^(e-k) *does* have an exact representation as a floating-point
;;; integer, and m * 10^k is still small enough.
(defun large-exponent-decimal-to-float (sign mantissa exponent)
  (declare (type (integer 0) mantissa)
           (type fixnum sign exponent))
  (if (< exponent (* 2 *upper-decimal-exponent-bound*))
      (let* ((diff (- exponent *upper-decimal-exponent-bound*))
             (new-mantissa (* mantissa (aref *integer-powers-of-ten* diff))))
        (* sign
           (float new-mantissa *most-positive-float*)
           (aref *float-powers-of-ten* *upper-decimal-exponent-bound*)))
      (fallback-decimal-to-float sign mantissa exponent)))

(defun decimal-to-float (sign mantissa exponent)
  (declare (type (integer 0) mantissa)
           (type fixnum sign exponent))
  (if (<= mantissa *upper-integer-float-bound*)
      ;; The mantissa has an exact representation as a
      ;; floating-point integer.
      (if (minusp exponent)
          (if (<= (- exponent) *upper-decimal-exponent-bound*)
              ;; The exponent e has a sufficiently small absolute
              ;; value that we have the value of 10^|e| in a table
              ;; as a floating-point integer.  Since the exponent
              ;; is negative, divide the mantissa (converted to a
              ;; float) by the float that is represented by the
              ;; exponent.
              (* sign
                 (/ (float mantissa *most-positive-float*)
                    (aref *float-powers-of-ten* (- exponent))))
              (fallback-decimal-to-float sign mantissa exponent))
          (if (<= exponent *upper-decimal-exponent-bound*)
              ;; The exponent e has a sufficiently small absolute
              ;; value that we have the value of 10^|e| in a table
              ;; as a floating-point integer.  Since the exponent
              ;; is positive, multiply the mantissa (converted to a
              ;; float) by the float that is represented by the
              ;; exponent.
              (* sign
                 (float mantissa *most-positive-float*)
                 (aref *float-powers-of-ten* exponent))
              (large-exponent-decimal-to-float sign mantissa exponent)))
      (fallback-decimal-to-float sign mantissa exponent)))
