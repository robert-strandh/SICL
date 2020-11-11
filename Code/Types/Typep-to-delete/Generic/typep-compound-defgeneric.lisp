(cl:in-package #:sicl-type)

;;; Implement TYPEP for a type specifier of the form (HEAD . REST).
;;; OBJECT is the object to test.
(defgeneric typep-compound (object head rest))

