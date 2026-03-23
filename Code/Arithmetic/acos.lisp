(cl:in-package #:sicl-arithmetic)

(defgeneric acos (argument))

(defun acos-general (argument)
  (* (complex 0 -1)
     (log (+ argument
             (sqrt (- (* argument argument) 1))))))

(defmethod acos ((argument single-float))
  (if (<= -1 argument 1)
      (sicl-primop:primop :single-float-acos argument)
      (acos-general argument)))

(defmethod acos ((argument double-float))
  (if (<= -1 argument 1)
      (sicl-primop:primop :double-float-acos argument)
      (acos-general argument)))

(defmethod acos ((argument rational))
  (acos (float argument)))

(defmethod acos ((argument number))
  (acos-general argument))
