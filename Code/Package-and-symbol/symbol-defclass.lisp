(cl:in-package #:sicl-symbol)

(defclass symbol (t)
  ((%name
    :initarg :name
    :accessor symbol-name)
   (%package
    :initarg :package
    :accessor symbol-package))
  (:metaclass built-in-class))
