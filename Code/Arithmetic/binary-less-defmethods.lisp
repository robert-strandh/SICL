(cl:in-package #:sicl-arithmetic)

(defmethod binary-less ((x fixnum) (y fixnum))
  (if (po:primop :fixnum-less x y) t nil))

(defmethod binary-less ((x ratio) (y integer))
  (binary-less (floor (numerator x) (denominator x)) y))

(defmethod binary-less ((x integer) (y ratio))
  (binary-less x (ceiling (numerator y) (denominator y))))

(defmethod binary-less ((x ratio) (y ratio))
  (binary-less (* (numerator x) (denominator y))
               (* (numerator y) (denominator x))))

(defmethod binary-less ((x single-float) (y integer))
  (if (po:primop :single-float-less x (float y 1s0)) t nil))

(defmethod binary-less ((x integer) (y single-float))
  (if (po:primop :single-float-less (float x 1s0) y) t nil))

(defmethod binary-less ((x single-float) (y single-float))
  (if (po:primop :single-float-less x y) t nil))

(defmethod binary-less ((x double-float) (y integer))
  (if (po:primop :double-float-less x (float y 1d0)) t nil))

(defmethod binary-less ((x integer) (y double-float))
  (if (po:primop :double-float-less (float x 1d0) y) t nil))

(defmethod binary-less ((x double-float) (y single-float))
  (if (po:primop :double-float-less x (float y 1d0)) t nil))

(defmethod binary-less ((x single-float) (y double-float))
  (if (po:primop :double-float-less (float x 1d0) y) t nil))

(defmethod binary-less ((x double-float) (y double-float))
  (if (po:primop :double-float-less x y) t nil))
