(cl:in-package #:cleavir-meter)

(defgeneric reset (meter)
  (:method-combination progn))

(defclass meter () ())

(defclass basic-meter (meter)
  ((%cpu-time :initform 0 :accessor cpu-time)
   (%invocation-count :initform 0 :accessor invocation-count)))

(defmethod reset progn ((meter basic-meter))
  (setf (cpu-time meter) 0)
  (setf (invocation-count meter) 0))
