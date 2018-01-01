(cl:in-package #:sicl-symbol)

(defclass symbol (t)
  ((%name
    :initarg :name
    :accessor name)
   (%package
    :initarg :package
    :accessor package))
  #+sicl (:metaclass built-in-class))
