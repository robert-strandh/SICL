(cl:in-package #:sicl-symbol)

(define-built-in-class symbol (t)
  ((%name
    :initarg :name
    :reader symbol-name)
   (%package
    :initarg :package
    :reader symbol-package
    :writer (setf package))))
