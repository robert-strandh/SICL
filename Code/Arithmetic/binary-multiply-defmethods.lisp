(cl:in-package #:sicl-arithmetic)

(defmethod binary-multiply ((multiplier fixnum) (multiplicand fixnum))
  (if (minusp multiplier)
      (if (minusp multiplicand)
          (multiple-value-bind (most-significant least-significant)
              (cleavir-primop:fixnum-multiply (- multiplier) (- multiplicand))
            (if (zerop most-significant)
                least-significant
                ;; FIXME: create a bignum once we know how to do that.
                (error 'create-a-bignum)))
          (multiple-value-bind (most-significant least-significant)
              (cleavir-primop:fixnum-multiply (- multiplier) multiplicand)
            (if (zerop most-significant)
                (- least-significant)
                ;; FIXME: create a bignum once we know how to do that.
                (error 'create-a-bignum))))
      (if (minusp multiplicand)
          (multiple-value-bind (most-significant least-significant)
              (cleavir-primop:fixnum-multiply multiplier (- multiplicand))
            (if (zerop most-significant)
                (- least-significant)
                ;; FIXME: create a bignum once we know how to do that.
                (error 'create-a-bignum)))
          (multiple-value-bind (most-significant least-significant)
              (cleavir-primop:fixnum-multiply multiplier multiplicand)
            (if (zerop most-significant)
                least-significant
                ;; FIXME: create a bignum once we know how to do that.
                (error 'create-a-bignum))))))

(defmethod binary-multiply ((multiplier integer) (multiplicand ratio))
  ;; We divide out common factors ahead of time rather than use the naive
  ;; method, so as to avoid large intermediates.
  (let* ((num (numerator multiplicand)) (den (denominator multiplicand))
         (g (gcd multiplier den))
         ;; TODO?: All of these divisions are exact, so for large bignums
         ;; it might be profitable to use an exact division algorithm like
         ;; Jebelean's; these can be more efficient than general division.
         (nmul (truncate multiplier g)) (nden (truncate den g))
         (nnum (* nmul num)))
    ;; We can end up with nden = 1, e.g. from (* 7 8/7); the gcd divisions will
    ;; give us nmul = 1, nden = 1, nnum = 8 for that example.
    (if (= nden 1)
        nnum
        (make-instance 'ratio :numerator nnum :denominator nden))))
(defmethod binary-multiply ((multiplier ratio) (multiplicand integer))
  (let* ((num (numerator multiplier)) (den (denominator multiplier))
         (g (gcd multiplicand den))
         (nmul (truncate multiplicand g)) (nden (truncate den g))
         (nnum (* nmul num)))
    (if (= nden 1)
        nnum
        (make-instance 'ratio :numerator nnum :denominator nden))))

(defmethod binary-multiply ((multiplier ratio) (multiplicand ratio))
  (let* ((num1 (numerator multiplier)) (den1 (denominator multiplier))
         (num2 (numerator multiplicand)) (den2 (denominator multiplicand))
         (gcd1 (gcd num1 den2)) (gcd2 (gcd num2 den1))
         (num1 (truncate num1 gcd1)) (den1 (truncate den1 gcd2))
         (num2 (truncate num2 gcd2)) (den2 (truncate den2 gcd1))
         (num (* num1 num2)) (den (* den1 den2)))
    (if (= den 1)
        num
        (make-instance 'ratio :numerator num :denominator den))))
