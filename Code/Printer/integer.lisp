(cl:in-package #:sicl-printer)

(defparameter *digits* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun print-positive-integer (integer base stream)
  (let ((factor (log 2f0 (float base 1f0)))
        (digits *digits*))
    (labels ((upper (integer)
               (if (< integer base)
                   (princ (schar digits integer) stream)
                   (let* ((binary-length (integer-length integer))
                          (length (max 2 (floor (* factor binary-length))))
                          (length/2 (floor length 2))
                          (divisor (expt base length/2)))
                     (upper (floor integer divisor))
                     (lower (mod integer divisor) length/2))))
             (lower (integer length)
               (if (= length 1)
                   (princ (schar digits integer) stream)
                   (let* ((length/2 (floor length 2))
                          (divisor (expt base length/2)))
                     (multiple-value-bind (quotient remainder)
                         (floor integer divisor)
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
