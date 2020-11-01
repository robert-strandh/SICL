(cl:in-package #:sicl-arithmetic)

(defgeneric limb-count (bignum))

(defgeneric (setf limb-count) (limb-count bignum))

;;; We must explicitly add STANDARD-OBJECT as a superclass since the
;;; presence of INTEGER as a superclass prevents the automatic
;;; inclusion of STANDARD-OBJECT as a superclass.
(defclass bignum (integer standard-object)
  ((%limb-count
    :initarg :limb-count
    :accessor limb-count)))

(defclass positive-bignum (bignum) ())

(defclass negative-bignum (bignum) ())
