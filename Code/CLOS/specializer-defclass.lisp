(cl:in-package #:sicl-clos)

(defclass specializer (metaobject)
  ((%direct-generic-functions
    :initform '()
    :reader specializer-direct-generic-functions
    :writer (setf direct-generic-functions))
   (%direct-methods
    :initform '()
    :accessor specializer-direct-methods)))
