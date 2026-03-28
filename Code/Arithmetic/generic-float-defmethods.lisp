(cl:in-package #:sicl-arithmetic)

(defparameter *most-positive-single-float-as-integer*
  (* (1- (ash 1 24)) (ash 1 (- 127 23))))

(defparameter *most-positive-double-float-as-integer*
  (* (1- (ash 1 53)) (ash 1 (- 1023 52))))

(defmethod generic-float ((number float) (prototype null))
  number)

(defmethod generic-float ((number fixnum) (prototype null))
  (sicl-primop:primop :convert-fixnum-to-single-float number))

(defmethod generic-float ((number fixnum) (prototype single-float))
  (sicl-primop:primop :convert-fixnum-to-single-float number))

(defmethod generic-float ((number fixnum) (prototype double-float))
  (sicl-primop:primop :convert-fixnum-to-double-float number))

(defun bignum-to-single-float (bignum)
  (if (> bignum *most-positive-single-float-as-integer*)
      (error 'floating-point-overflow
             :operands (list bignum)
             :operation 'float)
      (sicl-primop:primop :convert-bignum-to-single-float bignum)))

(defmethod generic-float ((number bignum) (prototype null))
  (bignum-to-single-float number))

(defmethod generic-float ((number bignum) (prototype single-float))
  (bignum-to-single-float number))

(defun bignum-to-double-float (bignum)
  (if (> bignum *most-positive-double-float-as-integer*)
      (error 'floating-point-overflow
             :operands (list bignum)
             :operation 'float)
      (sicl-primop:primop :convert-bignum-to-double-float bignum)))

(defmethod generic-float ((number bignum) (prototype null))
  (bignum-to-double-float number))

(defmethod generic-float ((number bignum) (prototype double-float))
  (bignum-to-double-float number))

(defmethod generic-float ((number single-float) (prototype single-float))
  number)

(defmethod generic-float ((number single-float) (prototype double-float))
  (sicl-primop:primop :convert-single-float-to-double-float))

(defmethod generic-float ((number double-float) (prototype double-float))
  number)

(defmethod generic-float ((number double-float) (prototype single-float))
  (if (> number most-positive-single-float)
      (error 'floating-point-overflow
             :operands (list number)
             :operation 'float)
      (sicl-primop:primop :convert-double-float-to-single-float)))
