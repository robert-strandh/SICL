(cl:in-package #:sicl-arithmetic)

(defclass simulated-float ()
  ((%sign :initarg :sign :reader sign)
   (%floatr :initarg :floatr :reader floatr)))

(setf (find-class 'single-float) nil)

(defclass single-float (simulated-float)
  ())

(defun make-single-float (sign floatr)
  (make-instance 'single-float
    :sign sign :floatr floatr))

(setf (find-class 'double-float) nil)

(defclass double-float (simulated-float)
  ())

(defun make-double-float (sign floatr)
  (make-instance 'double-float
    :sign sign :floatr floatr))

(defun bits-to-single-float (bits)
  (let* ((sign-bit (ldb (byte 1 31) bits))
         (exponent-bits (ldb (byte 8 23) bits))
         (mantissa-bits (ldb (byte 23 0) bits))
         (numerator
           (cond ((zerop exponent-bits)
                  ;; We have a subnormal float.
                  mantissa-bits)
                 ((>= exponent-bits (+ 127 23))
                  (ash (+ mantissa-bits (ash 1 23))
                       (- exponent-bits (+ 127 23))))
                 (t
                  (+ mantissa-bits (ash 1 23)))))
         (denominator
           (if (>= exponent-bits (+ 127 23))
               1
               (ash 1 (- (+ 127 23) exponent-bits)))))
    (make-instance 'single-float
      :sign (if (zerop sign-bit) 1 -1)
      :floatr (make-ratio numerator denominator))))

(defun bits-to-double-float (bits)
  (let* ((sign-bit (ldb (byte 1 63) bits))
         (exponent-bits (ldb (byte 11 52) bits))
         (mantissa-bits (ldb (byte 52 0) bits))
         (numerator
           (cond ((zerop exponent-bits)
                  ;; We have a subnormal float.
                  mantissa-bits)
                 ((>= exponent-bits (+ 1023 52))
                  (ash (+ mantissa-bits (ash 1 52))
                       (- exponent-bits (+ 1023 52))))
                 (t
                  (+ mantissa-bits (ash 1 52)))))
         (denominator
           (if (>= exponent-bits (+ 1023 52))
               1
               (ash 1 (- (+ 1023 52) exponent-bits)))))
    (make-instance 'double-float
      :sign (if (zerop sign-bit) 1 -1)
      :floatr (make-ratio numerator denominator))))


(defun coerce (value type)
  (assert (zerop value))
  (ecase type
    (single-float (bits-to-single-float value))
    (double-float (bits-to-double-float value))))

(defgeneric float-disgits (float))

(defmethod float-digits ((float single-float))
  (declare (ignore float))
  24)

(defmethod float-digits ((float double-float))
  (declare (ignore float))
  53)
