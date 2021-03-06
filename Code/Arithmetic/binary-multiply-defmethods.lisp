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
