(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/validate-superclass.html
(defgeneric validate-superclass (class superclass)
  (:argument-precedence-order superclass class))

;;; The argument precedence order is (SUPERCLASS CLASS).  Therefore,
;;; when both arguments are built-in classes, the third method is more
;;; specific than the second, and we return T.

(defmethod validate-superclass ((class class) (superclass class))
  t)

(defmethod validate-superclass ((class built-in-class) (superclass class))
  nil)

(defmethod validate-superclass ((class class) (superclass built-in-class))
  t)
