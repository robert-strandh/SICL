(cl:in-package #:sicl-arithmetic)

(defmethod binary-equal ((x fixnum) (y fixnum))
  (if (cleavir-primop:fixnum-equal x y) t nil))

(defmethod binary-equal ((x fixnum) (y bignum)) nil)
(defmethod binary-equal ((x bignum) (y fixnum)) nil)

(defmethod binary-equal ((x integer) (y ratio)) nil)
(defmethod binary-equal ((x ratio) (y integer)) nil)
(defmethod binary-equal ((x ratio) (y ratio))
  (and (binary-equal (numerator x) (numerator y))
       (binary-equal (denominator x) (denominator y))))
