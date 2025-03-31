(cl:in-package #:sicl-arithmetic)

(defmethod binary-subtract ((x fixnum) (y fixnum))
  (let ((result (po:primop :fixnum-subtract x y)))
    (if (minusp x)
        (if (minusp y)
            result
            (if (plusp result)
                (make-bignum-from-overflowed-fixnum result)
                result))
        (if (minusp y)
            (if (minusp result)
                (make-bignum-from-overflowed-fixnum result)
                result)
            result))))

(defmethod binary-subtract ((x integer) (y ratio))
  (let ((num (numerator y)) (den (denominator y)))
    (make-instance 'ratio
      :numerator (- (* den x) num) :denominator den)))
(defmethod binary-subtract ((x ratio) (y integer))
  (let ((num (numerator x)) (den (denominator x)))
    (make-instance 'ratio
      :numerator (- x (* den y)) :denominator den)))

(defmethod binary-subtract ((x ratio) (y ratio))
  (let ((xnum (numerator x)) (xden (denominator x))
        (ynum (numerator x)) (yden (denominator y)))
    (/ (- (* xnum yden) (* ynum xden)) (* xden yden))))

(defmethod binary-subtract ((x single-float) (y integer))
  (binary-subtract x (float y 1s0)))

(defmethod binary-subtract ((x integer) (y single-float))
  (binary-subtract (float x 1s0) y))

(defmethod binary-subtract ((x single-float) (y single-float))
  (po:primop :single-float-subtract x y))

(defmethod binary-subtract ((x double-float) (y integer))
  (binary-subtract x (float y 1d0)))

(defmethod binary-subtract ((x integer) (y double-float))
  (binary-subtract (float x 1d0) y))

(defmethod binary-subtract ((x double-float) (y single-float))
  (binary-subtract x (float y 1d0)))

(defmethod binary-subtract ((x single-float) (y double-float))
  (binary-subtract (float x 1d0) y))

(defmethod binary-subtract ((x double-float) (y double-float))
  (po:primop :double-float-subtract x y))
