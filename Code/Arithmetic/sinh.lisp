(cl:in-package #:sicl-arithmetic)

(defgeneric sinh (argument))

(defmethod sinh ((argument single-float))
  (sicl-primop:primop :single-float-sinh argument))

(defmethod sinh ((argument double-float))
  (sicl-primop:primop :double-float-sinh argument))

(defmethod sinh ((argument rational))
  (sinh (float argument)))

(defmethod sinh ((argument number))
  (let ((relpart (realpart argument))
        (imagpart (imagpart argument)))
    (complex (* (sinh realpart) (cos imagpart))
             (* (cosh realpart) (sin imagpart)))))
