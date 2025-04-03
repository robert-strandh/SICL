(cl:in-package #:sicl-arithmetic)

(defmethod binary-add ((x fixnum) (y fixnum))
  (let ((result (po:primop :fixnum-add x y)))
    (if (minusp x)
        (if (minusp y)
            (if (plusp result)
                (make-bignum-from-overflowed-fixnum result)
                result)
            result)
        (if (minusp y)
            result
            (if (minusp result)
                (make-bignum-from-overflowed-fixnum result)
                result)))))

(defmethod binary-add ((x integer) (y ratio))
  (let ((num (numerator y)) (den (denominator y)))
    ;; The new numerator and denominator are necessarily coprime, so skip
    ;; division canonicalization.
    (make-instance 'ratio
      :numerator (+ num (* den x)) :denominator den)))

(defmethod binary-add ((x ratio) (y integer))
  (let ((num (numerator x)) (den (denominator x)))
    (make-instance 'ratio
      :numerator (+ num (* den y)) :denominator den)))

(defmethod binary-add ((x ratio) (y ratio))
  (let ((xnum (numerator x)) (xden (denominator x))
        (ynum (numerator x)) (yden (denominator y)))
    (/ (+ (* xnum yden) (* ynum xden)) (* xden yden))))

(defmethod binary-add ((x single-float) (y integer))
  (po:primop :single-float-add x (float y 1s0)))

(defmethod binary-add ((x integer) (y single-float))
  (po:primop :single-float-add (float x 1s0) y))

(defmethod binary-add ((x single-float) (y single-float))
  (po:primop :single-float-add x y))

(defmethod binary-add ((x double-float) (y integer))
  (po:primop :double-float-add x (float y 1d0)))

(defmethod binary-add ((x integer) (y double-float))
  (po:primop :double-float-add (float x 1d0) y))

(defmethod binary-add ((x double-float) (y double-float))
  (po:primop :double-float-add x y))
