(cl:in-package #:sicl-arithmetic)

(defgeneric decode-float (float))

(defmethod decode-float ((float single-float))
  (sicl-primop:primop :decode-single-float float))

(defmethod decode-float ((float double-float))
  (sicl-primop:primop :decode-double-float float))

(defgeneric scale-float (float integer))

(defmethod scale-float ((float single-float) (integer integer))
  (sicl-primop:primop :scale-single-float float))

(defmethod scale-float ((float double-float) (integer integer))
  (sicl-primop:primop :scale-double-float float))

(defun float-radix (float)
  (check-type float float)
  2)

(defgeneric generic-float-sign (float-1 float-2))

(defmethod generic-float-sign ((float-1 single-float) (float-2 single-float))
  (sicl-primop:primop :single-copysign float-2 float-1))

(defmethod generic-float-sign ((float-1 double-float) (float-2 double-float))
  (sicl-primop:primop :double-copysign float-2 float-1))

(defmethod generic-float-sign ((float-1 single-float) (float-2 double-float))
  (sicl-primop:primop :double-copysign float-2 (float float-1 float-2)))

(defmethod generic-float-sign ((float-1 double-float) (float-2 single-float))
  (sicl-primop:primop :double-copysign(float float-2 float-1) float-1))

(defun float-sign (float-1 &optional (float-2 (float 1 float-1)))
  (generic-float-sign float-1 float-2))

(defgeneric float-digits (float))

(defmethod float-digits ((float single-float)) 24)

(defmethod float-digits ((float double-float)) 53)

(defgeneric float-precision (float))

(defmethod float-precision ((float single-float))
  (sicl-primop:primop :single-float-precision float))

(defmethod float-precision ((float double-float))
  (sicl-primop:primop :double-float-precision float))

(defgeneric integer-decode-float (float))

(defmethod integer-decode-float ((float single-float))
  (sicl-primop:primop :integer-decode-single-float float))

(defmethod integer-decode-float ((float double-float))
  (sicl-primop:primop :integer-decode-double-float float))

(defmethod signum ((float single-float))
  (if (zerop float)
      float
      (sicl-primop:primop :single-copysign 1f0 float)))

(defmethod signum ((float double-float))
  (if (zerop float)
      float
      (sicl-primop:primop :double-copysign 1d0 float)))
  
