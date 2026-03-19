(cl:in-package #:sicl-arithmetic)

(defgeneric sin (argument))

(defmethod sin ((argument single-float))
  (sicl-primop:primop :single-float-sin argument))

(defmethod sin ((argument double-float))
  (sicl-primop:primop :double-float-sin argument))

(defmethod sin ((argument rational))
  (sin (float argument)))

(defmethod sin ((argument number))
  (let ((realpart (realpart argument))
        (imagpart (imagpart argument)))
    (+ (* (sin realpart) (cosh imagpart))
       (* (complex (cos realpart) (sin imagpart))))))
