(cl:in-package #:sicl-arithmetic)

(defclass bignum (integer)
  ((%sign :initarg :sign :reader sign)
   (%digit-count :initarg :digit-count :reader digit-count)))
