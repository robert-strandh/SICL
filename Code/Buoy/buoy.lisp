(cl:in-package #:buoy)

;; copysign: return a number with the same magnitude as x, but the sign of y
;; like a commuted cl float-sign
(declaim (ftype (function (single-float single-float) single-float) single-copysign))
(declaim (ftype (function (double-float double-float) double-float) double-copysign))
(defun single-copysign (x y)
  (let ((xb (float-features:single-float-bits x))
        (yb (float-features:single-float-bits y)))
    (float-features:bits-single-float (dpb xb (byte 31 0) yb))))
(defun double-copysign (x y)
  (let ((xb (float-features:double-float-bits x))
        (yb (float-features:double-float-bits y)))
    (float-features:bits-double-float (dpb xb (byte 63 0) yb))))

;; eval-when required because we require these functions later at read-time in the type proclamations for decode-float
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; next-before: return the greatest floating-point less than float, with the same representation
  ;; next-after: return the least floating-point greater than float, with the same representation
  (declaim (ftype (function ((and single-float (not (eql #.most-negative-single-float))))
                            (and single-float (not (eql #.most-positive-single-float))))
                  single-next-before)
           (ftype (function ((and single-float (not (eql #.most-positive-single-float))))
                            (and single-float (not (eql #.most-negative-single-float))))
                  single-next-after)
           (ftype (function ((and double-float (not (eql #.most-negative-double-float))))
                            (and double-float (not (eql #.most-positive-double-float))))
                  double-next-before)
           (ftype (function ((and double-float (not (eql #.most-positive-double-float))))
                            (and double-float (not (eql #.most-negative-double-float))))
                  double-next-after))
  ;; TODO: if there were an implicitly sign-extended version of float-bits, this would be nicer
  ;; (logior 1 (ash bits -31)) or so
  (defun single-next-before (float)
    (if (zerop float)
        least-negative-single-float
        (let ((bits (float-features:single-float-bits float)))
          (float-features:bits-single-float (+ bits (if (plusp float) -1 1))))))
  (defun single-next-after (float)
    (let ((bits (float-features:single-float-bits (+ 0s0 float)))) ; (eql 0s0 (+ 0s0 -0s0))
      (float-features:bits-single-float (+ bits (if (not (minusp float)) 1 -1)))))
  (defun double-next-before (float)
    (if (zerop float)
        least-negative-double-float
        (let ((bits (float-features:double-float-bits float)))
          (float-features:bits-double-float (+ bits (if (plusp float) -1 1))))))
  (defun double-next-after (float)
    (let ((bits (float-features:double-float-bits (+ 0s0 float))))
      (float-features:bits-double-float (+ bits (if (not (minusp float)) 1 -1))))))
    
;; decode-float: return (values significand exponent sign)
(declaim (ftype (function (single-float) (values
                                          (or (eql 0s0)
                                              (single-float 0.5s0 #.(single-next-before 1s0)))
                                          (integer -148 128)
                                          (member -1s0 1s0)))
                decode-single-float)
         (ftype (function (double-float) (values
                                          (or (eql 0d0)
                                              (double-float 0.5d0 #.(double-next-before 1d0)))
                                          (integer -1073 1024)
                                          (member -1d0 1d0)))
                decode-double-float))

(defun decode-single-float (float)
  (let ((bits (float-features:single-float-bits float)))
    ;; check not denormal (all-zero exponent)
    (if (ldb-test (byte 8 23) bits)
        ;; Replace the exponent with 126 (that is a biased -1).  Since a normalised ieee
        ;; fraction has range [1 2), this brings it into the desired [0.5 1) range.
        ;; While we're at it, clear out the sign bit, that is the most significant bit.
        (let ((significand (float-features:bits-single-float
                            (dpb 126 (byte 9 23) bits)))
              (exponent (+ -126 (ldb (byte 8 23) bits)))
              (sign (if (logbitp 31 bits) -1s0 1s0)))
          (values significand exponent sign))
        (if (zerop float)
            ;; zero; must be handled specially.  Logior is cheaper than copysign here, since we
            ;; know all the requisite bits are zero anyway; the only significant bit is the sign bit
            (values 0s0 0 (float-features:bits-single-float (logior bits (float-features:single-float-bits 1s0))))
            ;; denormal
            (let* ((significand-bits (ldb (byte 23 0) bits))
                   (significand-length (integer-length significand-bits))
                   (significand-leading-zeroes (- 23 significand-length))
                   (exponent (- (ldb (byte 8 23) bits) 126 significand-leading-zeroes))
                   (sign (if (logbitp 31 bits) -1s0 1s0)))
              (values
               (float-features:bits-single-float
                (dpb 126 (byte 9 23)
                     (ash significand-bits (1+ significand-leading-zeroes))))
               exponent sign))))))

(defun decode-double-float (float)
  (let ((bits (float-features:double-float-bits float)))
    ;; check for all-zero exponent (denormal)
    (if (ldb-test (byte 11 52) bits)
        ;; replace the exponent (8 bits starting at bit 23, past the mantissa)
        ;; with 126 (that is a biased -1).  Since an ieee fraction has range [1 2), this
        ;; brings it into the desired [0 1) range.
        ;; At the same time, clear out the sign bit, that is the most significant bit.
        (let ((significand (float-features:bits-double-float
                            (dpb 1022 (byte 12 52) bits)))
              (exponent (+ -1022 (ldb (byte 11 52) bits)))
              (sign (if (logbitp 63 bits) -1d0 1d0)))
          (values significand exponent sign))
        (if (zerop float)
            ;; zero; must be handled specially.  Logior is cheaper than copysign here, since we
            ;; know all the requisite bits are zero anyway; the only significant bit is the sign bit
            (values 0d0 0 (float-features:bits-double-float (logior bits (float-features:double-float-bits 1d0))))
            ;; denormal
            (let* ((significand-bits (ldb (byte 52 0) bits))
                   (significand-length (integer-length significand-bits))
                   (significand-leading-zeroes (- 52 significand-length))
                   (exponent (- (ldb (byte 11 52) bits) 1022 significand-leading-zeroes))
                   (sign (if (logbitp 63 bits) -1d0 1d0)))
              (values
               (float-features:bits-double-float
                (dpb 1022 (byte 12 52)
                     (ash significand-bits (1+ significand-leading-zeroes))))
               exponent sign))))))

(declaim (ftype (function (single-float) (values
                                          (integer 0 #.(1- (expt 2 24)))
                                          (integer * *)
                                          (member -1 1)))
                integer-decode-single-float)
         (ftype (function (double-float) (values (integer 0 #.(1- (expt 2 53)))
                                                 (integer * *)
                                                 (member -1 1)))
                integer-decode-double-float))

;; TODO: a sign-extended float-bits would be nice here too
(defun integer-decode-single-float (float)
  (let* ((bits (float-features:single-float-bits float))
         (significand (ldb (byte 23 0) bits))
         (biased-exponent (ldb (byte 8 23) bits))
         (sign (if (logbitp 31 bits) -1 1))
         (normal-bit (if (zerop biased-exponent) 0 1))
         ;; realise virtual leading bit if necessary
         (significand (logior significand (ash normal-bit 23))))
    ;; subtract 22 instead of 23, but then additionally subtract 1 in the normal case
    (values significand (- biased-exponent 127 22 normal-bit) sign)))
(defun integer-decode-double-float (float)
  (let* ((bits (float-features:double-float-bits float))
         (significand (ldb (byte 52 0) bits))
         (biased-exponent (ldb (byte 11 52) bits))
         (sign (if (logbitp 63 bits) -1 1))
         (normal-bit (if (zerop biased-exponent) 0 1))
         ;; realise virtual leading bit if necessary
         (significand (logior significand (ash normal-bit 52))))
    ;; subtract 51 instead of 52, but then additionally subtract 1 in the normal case
    (values significand (- biased-exponent 1023 51 normal-bit) sign)))

;; float-precision: return the number of significant bits (including virtual)
(declaim (ftype (function (single-float) (integer 0 24)) single-float-precision)
         (ftype (function (double-float) (integer 0 53)) double-float-precision))
(defun single-float-precision (float)
  (let* ((bits (float-features:single-float-bits float))
         (significand-length (integer-length (ldb (byte 23 0) bits))))
    (if (ldb-test (byte 8 23) bits) 24 significand-length)))
(defun double-float-precision (float)
  (let* ((bits (float-features:double-float-bits float))
         (significand-length (integer-length (ldb (byte 52 0) bits))))
    (if (ldb-test (byte 11 52) bits) 53 significand-length)))

;;; scale-float: returns (* float (expt 2 integer)), correctly rounded.
;;; This is commonly implemented (both in CL and in C) by manual
;;; twiddling of exponent bits.  This is annoying to do, and requires
;;; manual handling of overflow and denormals.  Nicer, and faster, is
;;; simply to multiply.
;;; But there is a problem: the range of a single float's exponent is
;;; a bit more than 2^8, but the maximum magnitude of that exponent is
;;; about 2^7, so there are meaningful values of INTEGER for which
;;; (expt 2 integer) is not exactly representable.  To get around
;;; this, we multiply multiple times when INTEGER is too large.

;;; But could this change the result?  The only case in which we lose
;;; bits will be if we generate a denormal number from a normal one.
;;; If we round twice, we may get an incorrect result (one example
;;; with round to nearest, bias towards even, is 0 10 10 -> 0 10 -> 0
;;; (the correct result is 1)).  So we must make sure to round only
;;; once; we do this by performing one 'partial-width' multiplication,
;;; followed by some number of 'full-width' multiplications; the
;;; full-width multiplications are by (expt 2 -126) or (expt 2 127)
;;; (for single floats, and the analogous values for doubles); since
;;; 126 is more than the number of bits in a significand, this
;;; guarantees we only round once, and automatically accord with the
;;; current rounding mode.

;;; (In the case when we multiply 3 times, we actually do one
;;; full-width multiply, then a partial one, then another full-width
;;; one, for better scheduling.)

(declaim (ftype (function (single-float (integer *  277)) single-float) scale-single-float)
         (ftype (function (double-float (integer * 2098)) double-float) scale-double-float))
(defun scale-single-float (float integer)
  ;; can (expt 2 integer) be exactly represented in a normalised float?  If so, just multiply once
  ;; (the compiler should reduce this to a single branch)
  (if (<= -126 integer 127)
      (* float (float-features:bits-single-float (ash (+ 127 integer) 23)))
      (let* ((base-exp (if (minusp integer) -126 127))
             (base-mul (float-features:bits-single-float (ash (+ 127 base-exp) 23))))
        (if (<= -252 integer 254)
            (let* ((exp (- integer base-exp))
                   (float (* float (float-features:bits-single-float (ash (+ 127 exp) 23))))
                   (float (* float base-mul))
                   )
              float)
            ;; need 3 multipliers :\
            (let* ((float (* float base-mul))
                   ;; exp can be arbitrarily small; cap it.
                   ;; If it's that small, the result will definitely be zero so w/e.
                   ;; Single float exponent range is 277; we've handled up to 252, and
                   ;; 25=277-252, so 26 is enough to zero most-positive-single-float.
                   ;; Choose the value of smallest magnitude in order to maximise the chance
                   ;; it fits into an immediate on any given architecture.
                   (exp (max -26 (- integer base-exp base-exp)))
                   (float (* float (float-features:bits-single-float (ash (+ 127 exp) 23))))
                   (float (* float base-mul)))
              float)))))
(defun scale-double-float (float integer)
  ;; can (expt 2 integer) be exactly represented in a normalised float?  If so, just multiply once
  ;; (the compiler should reduce this to a single branch)
  (if (<= -1022 integer 1023)
      (* float (float-features:bits-double-float (ash (+ 1023 integer) 52)))
      (let* ((base-exp (if (minusp integer) -1022 1023))
             (base-mul (float-features:bits-double-float (ash (+ 1023 base-exp) 52))))
        (if (<= -2044 integer 2046)
            (let* ((exp (- integer base-exp))
                   (float (* float (float-features:bits-double-float (ash (+ 1023 exp) 52))))
                   (float (* float base-mul)))
              float)
            ;; need 3 multipliers :\
            (let* ((float (* float base-mul))
                   ;; exp can be arbitrarily small; cap it.
                   ;; If it's that small, the result will definitely be zero so w/e.
                   ;; Double float exponent range is 2098; we've handled up to 2044, and
                   ;; 54=2098-2044, so 55 is enough to zero most-positive-double-float.
                   ;; Choose the value of smallest magnitude in order to maximise the chance
                   ;; it fits into an immediate on any given architecture.
                   (exp (max -55 (- integer base-exp base-exp)))
                   (float (* float (float-features:bits-double-float (ash (+ 1023 exp) 52))))
                   (float (* float base-mul)))
              float)))))
