(cl:in-package #:sicl-arithmetic)

(defgeneric exp (argument))

(defmethod exp ((argument single-float))
  (sicl-primop:primop :single-float-exp argument))

(defmethod exp ((argument double-float))
  (sicl-primop:primop :double-float-exp argument))

(defmethod exp ((argument rational))
  (exp (float argument)))

(defmethod exp ((argument number))
  (let* ((realpart (realpart argument))
         (imagpart (imagpart argument))
         (magnitude (exp realpat)))
    (complex (* magnitude (cos imagpart))
             (* magnitude (sin imagpart)))))
