(cl:in-package #:sicl-arithmetic)

(defmethod signum ((number fixnum))
  (cond ((zerop number) 0)
        ((plusp number) 1)
        (t -1)))

;; bignums are never zero, so we can skip one comparison
(defmethod signum ((number bignum)) (if (plusp number) 1 -1))

(defmethod signum ((number ratio)) (signum (numerator number)))
