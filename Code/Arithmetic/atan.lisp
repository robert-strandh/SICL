(cl:in-package #:sicl-arithmetic)

(defgeneric atan (argument))

(defmethod atan ((argument single-float))
  (sicl-primop:primop :single-float-atan argument))

(defmethod atan ((argument double-float))
  (sicl-primop:primop :double-float-atan argument))

(defmethod atan ((argument rational))
  (atan (float argument)))

(defmethod atan ((argument number))
  (/ (log (/ (- (complex 0 1) argument)
             (+ (complex 0 1) argument)))
     (complex 0 2)))
