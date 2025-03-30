(cl:in-package #:sicl-arithmetic)

(defmethod generic-truncate ((number fixnum) (divisor fixnum))
  (if (zerop divisor)
      (error 'division-by-zero
             :operation 'truncate
             :operands (list number divisor))
      (po:primop :fixnum-divide number divisor)))

(defmethod generic-truncate ((number float) (divisor integer))
  (generic-truncate number (float divisor)))

(defmethod generic-truncate ((number single-float) (divisor single-float))
  (let ((float-quotient (po:primop :single-float-divide number divisor)))
    (multiple-value-bind (mantissa exponent sign)
        (integer-decode-float float-quotient)
      (let ((quotient (* sign (ash mantissa exponent))))
        (values quotient (- float-quotient quotient))))))
