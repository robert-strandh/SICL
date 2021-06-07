(cl:in-package #:sicl-arithmetic)

(defgeneric realpart (number))

(defgeneric imagpart (number))

(defclass complex (number standard-object)
  ((%realpart :initarg :realpart :reader realpart)
   (%imagpart :initarg :imagpart :reader imagpart)))

(defclass complex-rational (complex) ())

(defclass complex-single-float (complex) ())

(defclass complex-double-float (complex) ())
