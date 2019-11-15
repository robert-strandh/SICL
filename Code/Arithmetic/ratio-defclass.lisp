(cl:in-package #:sicl-arithmetic)

(defclass ratio (rational standard-object)
  ((%numerator :initarg :numerator :reader numerator)
   (%denominator :initarg :denominator :reader denominator)))
