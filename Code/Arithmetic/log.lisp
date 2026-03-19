(cl:in-package #:sicl-arithmetic)

(defgeneric log (argument &optional base))

(defmethod log ((argument single-float) &optional (base nil base-p))
  (cond ((base-p
          (/ (log argument) (log base))))
        ((zerop argument)
         (error 'floating-point-underflow))
        ((plusp argument)
         (sicl-primop:primop :single-float-ln argument))
        (t
         (complex (log (abs argument)) (phase argument)))))

(defmethod log ((argument double-float) &optional (base nil base-p))
  (cond ((base-p
          (/ (log argument) (log base))))
        ((zerop argument)
         (error 'floating-point-underflow))
        ((plusp argument)
         (sicl-primop:primop :double-float-ln argument))
        (t
         (complex (log (abs argument)) (phase argument)))))

(defmethod log ((argument rational) &optional (base nil base-p))
  (if base-p
      (log (float argument) base)
      (log (float argument))))

(defmethod log ((argument number) &optional (base nil base-p))
  (if base-p
      (/ (log argument) (log base))
      (complex (log (abs argument)) (phase argument))))
