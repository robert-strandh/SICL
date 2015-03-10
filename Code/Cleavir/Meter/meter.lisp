(cl:in-package #:cleavir-meter)

(defclass meter () ())

(defclass basic-meter (meter)
  ((%cpu-time :initform 0 :accessor cpu-time)
   (%invocation-count :initform 0 :accessor invocation-count)))
