(cl:in-package #:sicl-arithmetic)

(defgeneric sign-and-limb-count (bignum))

(defgeneric (setf sign-and-limb-count) (new-value bignum))
