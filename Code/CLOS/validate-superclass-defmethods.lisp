(cl:in-package #:sicl-clos)

(defgeneric validate-superclass ((class class) (superclass class))
  (validate-superclass-default class superclass))
