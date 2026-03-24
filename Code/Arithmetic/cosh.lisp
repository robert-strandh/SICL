(cl:in-package #:sicl-arithmetic)

(defgeneric cosh (argument))

(defmethod cosh ((argument single-float))
  (sicl-primop:primop :single-float-cosh argument))

(defmethod cosh ((argument double-float))
  (sicl-primop:primop :double-float-cosh argument))

(defmethod cosh ((argument rational))
  (cosh (float argument)))

(defmethod cosh ((argument number))
  (let ((realpart (realpart argument))
        (imagpart (imagpart argument)))
    (complex (* (cosh realpart) (cos imagpart))
             (* (sinh realpart) (sin imagpart)))))
