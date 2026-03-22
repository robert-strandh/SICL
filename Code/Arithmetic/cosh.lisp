(cl:in-package #:sicl-arithmetic)

(defgeneric cosh (argument))

(defmethod cosh ((argument single-float))
  (sicl-primop:primop :single-float-cosh argument))

(defmethod cosh ((argument double-float))
  (sicl-primop:primop :double-float-cosh argument))

(defmethod cosh ((argument rational))
  (cosh (float argument)))

(defmethod cosh ((argument number))
  (let ((relpart (realpart argument))
        (imagpart (imagpart argument)))
    (+ (* (cosh realpart) (cos imagpart))
       (* (complex 0 1)
          (sinh realpart) (sin imagpart)))))
