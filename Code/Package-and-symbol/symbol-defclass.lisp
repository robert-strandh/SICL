(cl:in-package #:sicl-symbol)

(defclass symbol ()
  ((%name
    :initarg :name
    :reader symbol-name)
   (%package
    :initarg :package
    :reader symbol-package)))
