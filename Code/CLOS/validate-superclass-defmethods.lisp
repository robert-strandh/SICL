(cl:in-package #:sicl-clos)

(defmethod validate-superclass ((class class) (superclass class))
  (validate-superclass-default class superclass))
