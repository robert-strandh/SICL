(cl:in-package #:sicl-arithmetic)

(defmethod binary-not-greater ((x fixnum) (y fixnum))
  (if (cleavir-primop:fixnum-not-greater x y) t nil))

(defmethod binary-not-greater ((x ratio) (y integer))
  (binary-not-greater (ceiling (numerator x) (denominator x)) y))

(defmethod binary-not-greater ((x integer) (y ratio))
  (binary-not-greater x (floor (numerator y) (denominator y))))

(defmethod binary-not-greater ((x ratio) (y ratio))
  (binary-not-greater (* (numerator x) (denominator y))
                      (* (numerator y) (denominator x))))
