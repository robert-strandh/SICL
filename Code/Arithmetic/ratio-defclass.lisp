(cl:in-package #:sicl-arithmetic)

(defgeneric numerator (ratio))

(defgeneric denominator (ratio))

(defclass ratio (rational)
  ((%numerator :initarg :numerator :reader numerator)
   (%denominator :initarg :denominator :reader denominator)))
