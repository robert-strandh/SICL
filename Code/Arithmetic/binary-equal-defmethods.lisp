(cl:in-package #:sicl-arithmetic)

(defmethod binary-equal ((x fixnum) (y fixnum))
  (if (po:primop :fixnum-equal x y) t nil))

(defmethod binary-equal ((x fixnum) (y bignum))
  nil)

(defmethod binary-equal ((x bignum) (y fixnum))
  nil)

(defmethod binary-equal ((x integer) (y ratio))
  nil)

(defmethod binary-equal ((x ratio) (y integer))
  nil)

(defmethod binary-equal ((x ratio) (y ratio))
  (and (binary-equal (numerator x) (numerator y))
       (binary-equal (denominator x) (denominator y))))

(defmethod binary-equal ((x single-float) (y single-float))
  (if (po:primop :single-float-equal x y) t nil))

(defmethod binary-equal ((x double-float) (y double-float))
  (if (po:primop :double-float-equal x y) t nil))
