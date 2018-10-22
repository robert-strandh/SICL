(cl:in-package #:sicl-arithmetic)

(defclass ratio (rational)
  ((%numerator :initarg :numerator :reader numerator)
   (%denominator :initarg :denominator :reader denominator))
  (:metaclass built-in-class))
