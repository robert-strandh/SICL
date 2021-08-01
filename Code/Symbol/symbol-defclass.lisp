(cl:in-package #:sicl-symbol)

(defgeneric symbol-name (symbol))

(defgeneric symbol-package (symbol))

(defclass symbol ()
  ((%name
    :initarg :name
    :reader symbol-name)
   (%package
    :initarg :package
    :reader symbol-package)))
