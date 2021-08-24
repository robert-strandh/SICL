(cl:in-package #:sicl-arithmetic)

(defmethod binary-add ((x fixnum) (y fixnum))
  (cleavir-primop:let-uninitialized
   (z)
   (if (cleavir-primop:fixnum-add x y z)
       z
       (convert-fixnum-to-bignum z))))

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
