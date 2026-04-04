(cl:in-package #:sicl-arithmetic)

(defmethod binary-not-greater ((x fixnum) (y fixnum))
  (if (po:primop :fixnum-not-greater x y) t nil))

(defmethod binary-not-greater ((x ratio) (y integer))
  (binary-not-greater (ceiling (numerator x) (denominator x)) y))

(defmethod binary-not-greater ((x integer) (y ratio))
  (binary-not-greater x (floor (numerator y) (denominator y))))

(defmethod binary-not-greater ((x ratio) (y ratio))
  (binary-not-greater (* (numerator x) (denominator y))
                      (* (numerator y) (denominator x))))

(defmethod binary-not-greater ((x rational) (y single-float))
  (binary-not-greater (float x 1f0) y))

(defmethod binary-not-greater ((x rational) (y double-float))
  (binary-not-greater (float x 1d0) y))

(defmethod binary-not-greater ((x single-float) (y rational))
  (binary-not-greater x (float y 1f0)))

(defmethod binary-not-greater ((x double-float) (y rational))
  (binary-not-greater x (float y 1d0)))
