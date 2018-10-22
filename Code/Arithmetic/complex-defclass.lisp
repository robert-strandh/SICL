(cl:in-package #:sicl-arithmetic)

(defclass complex (number)
  ((%realpart :initarg :realpart :reader realpart)
   (%imagpart :initarg :imagpart :reader imagpart))
  (:metaclass built-in-class))
