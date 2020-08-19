(cl:in-package #:sicl-arithmetic)

(defmethod expt ((base integer) (power integer))
  (cond ((zerop power) 1)
        ((minusp power)
         ;; FIXME: handle this case
         (error 'fixme-handle-negative-powers))
        ((= power 1)
         base)
        ((evenp power)
         (expt (* base base) (floor power 2)))
        (t
         (* base (expt base (1- power))))))
