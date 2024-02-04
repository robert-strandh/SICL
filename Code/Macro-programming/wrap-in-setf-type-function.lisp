(cl:in-package #:common-macro-definitions)

(defgeneric wrap-in-setf-type-function (client name function environment))

(defmethod wrap-in-setf-type-function (client name function environment)
  `(setf (sicl-environment:type-expander ,client ,environment ,name) ,function))
