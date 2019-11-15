(cl:in-package #:sicl-arithmetic)

(defclass complex (number standard-object)
  ((%realpart :initarg :realpart :reader realpart)
   (%imagpart :initarg :imagpart :reader imagpart)))
