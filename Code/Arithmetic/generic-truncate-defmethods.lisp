(cl:in-package #:sicl-arithmetic)

(defmethod generic-truncate ((number fixnum) (divisor fixnum))
  (if (zerop divisor)
      (error 'division-by-zero
             :operation 'truncate
             :operands (list number divisor))
      (po:primop :fixnum-divide number divisor)))
