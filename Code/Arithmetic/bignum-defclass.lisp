(cl:in-package #:sicl-arithmetic)

;;; We must explicitly add STANDARD-OBJECT as a superclass since the
;;; presence of INTEGER as a superclass prevents the automatic
;;; inclusion of STANDARD-OBJECT as a superclass.
(defclass bignum (integer standard-object)
  ((%sign-and-limb-count
    :initarg :limb-count
    :accessor sign-and-limb-count)))
