(cl:in-package #:sicl-arithmetic)

(defgeneric asin (argument))

(defun asin-general (argument)
  (/ (log (+ (* (complex 0 1) argument)
             (sqrt (1- (* argument argument)))))
     (complex 0 1)))

(defmethod asin ((argument single-float))
  (if (<= -1 argument 1)
      (sicl-primop:primop :single-float-asin argument)
      (asin-general argument)))

(defmethod asin ((argument double-float))
  (if (<= -1 argument 1)
      (sicl-primop:primop :double-float-asin argument)
      (asin-general argument)))

(defmethod asin ((argument rational))
  (asin (float argument)))

(defmethod asin ((argument number))
  (asin-general argument))
