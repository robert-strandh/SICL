(in-package #:sicl-symbol-low)

(define-built-in-class symbol ()
  ((%name
    :initarg :name
    :reader symbol-name)
   (%package
    :initarg :package
    :reader symbol-package)
   (%plist
    :initarg :plist
    :accessor symbol-plist)))

