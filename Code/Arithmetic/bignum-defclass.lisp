(cl:in-package #:sicl-arithmetic)

(defclass bignum (integer)
  ((%sign-and-digit-count
    :initarg :sign-and-digit-count
    :reader sign-and-digit-count)))
