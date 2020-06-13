(cl:in-package #:sicl-arithmetic)

;;; We must explicitly add STANDARD-OBJECT as a superclass since the
;;; presence of INTEGER as a superclass prevents the automatic
;;; inclusion of STANDARD-OBJECT as a superclass.
(defclass bignum (integer standard-object)
  ((%sign-and-digit-count
    :initarg :sign-and-digit-count
    :reader sign-and-digit-count)))
