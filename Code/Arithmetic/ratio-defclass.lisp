(cl:in-package #:sicl-arithmetic)

(defgeneric numerator (ratio))

(defgeneric denominator (ratio))

(defclass ratio (rational standard-object)
  ((%numerator :initarg :numerator :reader numerator)
   (%denominator :initarg :denominator :reader denominator)))
