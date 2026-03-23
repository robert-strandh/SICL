(cl:in-package #:sicl-arithmetic)

(defgeneric tanh (argument))

(defmethod tanh ((argument single-float))
  (sicl-primop:primop :single-float-tanh argument))

(defmethod tanh ((argument double-float))
  (sicl-primop:primop :double-float-tanh argument))

(defmethod tanh ((argument rational))
  (tanh (float argument)))

(defmethod tanh ((argument number))
  (/ (+ (sinh (* 2 realpart))
        (* (complex 0 1) (sin (* 2 imagpart))))
     (+ (cosh (* 2 realpart))
        (* (cos (* 2 imagpart))))))
