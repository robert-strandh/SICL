(cl:in-package #:sicl-symbol)

(defclass symbol (t)
  ((%name
    :initarg :name
    :reader symbol-name)
   (%package
    :initarg :package
    :reader symbol-package)))
