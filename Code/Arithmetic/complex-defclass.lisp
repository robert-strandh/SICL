(cl:in-package #:sicl-arithmetic)

(defgeneric realpart (number))

(defgeneric imagpart (number))

(defclass complex (number standard-object)
  ((%realpart :initarg :realpart :reader realpart)
   (%imagpart :initarg :imagpart :reader imagpart)))
