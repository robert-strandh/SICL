(cl:in-package #:sicl-string)

(defgeneric string (object))

(defmethod string ((object string))
  object)

(defmethod string ((object symbol))
  (symbol-name object))

(defmethod string ((object character))
  (make-string 1 :initial-element object))
