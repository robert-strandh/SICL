(cl:in-package #:sicl-arithmetic)

(defmethod binary-subtract ((minuend fixnum) (subtrahend fixnum))
  (cleavir-primop:let-uninitialized
   (z)
   (if (cleavir-primop:fixnum-sub minuend subtrahend z)
       z
       (convert-fixnum-to-bignum z))))

(defmethod binary-sub ((x integer) (y ratio))
  (let ((num (numerator y)) (den (denominator y)))
    (make-instance 'ratio
      :numerator (- (* den x) num) :denominator den)))
(defmethod binary-sub ((x ratio) (y integer))
  (let ((num (numerator x)) (den (denominator x)))
    (make-instance 'ratio
      :numerator (- x (* den y)) :denominator den)))

(defmethod binary-sub ((x ratio) (y ratio))
  (let ((xnum (numerator x)) (xden (denominator x))
        (ynum (numerator x)) (yden (denominator y)))
    (/ (- (* xnum yden) (* ynum xden)) (* xden yden))))
