(cl:in-package #:sicl-arithmetic)

(defgeneric decode-float (float))
(defmethod decode-float ((float single-float)) (buoy:decode-single-float float))
(defmethod decode-float ((float double-float)) (buoy:decode-double-float float))

(defgeneric scale-float (float integer))
(defmethod scale-float ((float single-float) (integer integer))
  (buoy:scale-single-float float integer))
(defmethod scale-float ((float double-float) (integer integer))
  (buoy:scale-double-float float integer))

(defun float-radix (float)
  (check-type float float)
  2)

(defgeneric generic-float-sign (float-1 float-2))
(defmethod generic-float-sign ((float-1 single-float) (float-2 single-float))
  (buoy:single-copysign float-2 float-1))
(defmethod generic-float-sign ((float-1 double-float) (float-2 double-float))
  (buoy:double-copysign float-2 float-1))
(defmethod generic-float-sign ((float-1 single-float) (float-2 double-float))
  (buoy:double-copysign float-2 (float float-1 float-2)))
(defmethod generic-float-sign ((float-1 double-float) (float-2 single-float))
  (buoy:double-copysign (float float-2 float-1) float-1))

(defun float-sign (float-1 &optional (float-2 (float 1 float-1)))
  (generic-float-sign float-1 float-2))

(defgeneric float-digits (float))
(defmethod float-digits ((float single-float)) 24)
(defmethod float-digits ((float double-float)) 53)

(defgeneric float-precision (float))
(defmethod float-precision ((float single-float)) (buoy:single-float-precision float))
(defmethod float-precision ((float double-float)) (buoy:double-float-precision float))

(defgeneric integer-decode-float (float))
(defmethod integer-decode-float ((float single-float)) (buoy:integer-decode-single-float float))
(defmethod integer-decode-float ((float double-float)) (buoy:integer-decode-double-float float))

(defmethod signum ((float single-float))
  (if (zerop float)
      float
      (buoy:single-copysign 1s0 float)))
(defmethod signum ((float double-float))
  (if (zerop float)
      float
      (buoy:double-copysign 1d0 float)))
  
