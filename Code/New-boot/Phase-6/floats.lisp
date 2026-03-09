(cl:in-package #:sicl-new-boot-phase-6)

(defvar *make-single-float*)

(defvar *make-double-float*)

(defvar *float-components*)

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
    (funcall *make-single-float*
             (if (zerop sign-bit) 1 -1)
             (/ numerator denominator))))

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
    (funcall *make-double-float*
             (if (zerop sign-bit) 1 -1)
             (/ numerator denominator))))
