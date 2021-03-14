(cl:in-package #:sicl-clos)

(defclass specializer (metaobject)
  ((%direct-generic-functions
    :initform '()
    :accessor specializer-direct-generic-functions)
   (%direct-methods
    :initform '()
    :accessor specializer-direct-methods)))
