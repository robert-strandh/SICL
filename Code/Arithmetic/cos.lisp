(cl:in-package #:sicl-arithmetic)

(defgeneric cos (argument))

(defmethod cos ((argument single-float))
  (sicl-primop:primop :single-float-cos argument))

(defmethod cos ((argument double-float))
  (sicl-primop:primop :double-float-cos argument))

(defmethod cos ((argument rational))
  (cos (float argument)))

(defmethod cos ((argument number))
  (let ((realpart (realpart argument))
        (imagpart (imagpart argument)))
    (complex (* (cos realpart) (cosh imagpart))
             (* (sin realpart) (sinh imagpart)))))
