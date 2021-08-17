(cl:in-package #:sicl-arithmetic)

(defmethod binary-less ((x fixnum) (y fixnum))
  (if (cleavir-primop:fixnum-less x y) t nil))

(defmethod binary-less ((x ratio) (y integer))
  (binary-less (floor (numerator x) (denominator x)) y))

(defmethod binary-less ((x integer) (y ratio))
  (binary-less x (ceiling (numerator y) (denominator y))))

(defmethod binary-less ((x ratio) (y ratio))
  (binary-less (* (numerator x) (denominator y))
               (* (numerator y) (denominator x))))
