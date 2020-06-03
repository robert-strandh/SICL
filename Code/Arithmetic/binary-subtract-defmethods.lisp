(cl:in-package #:sicl-arithmetic)

(defmethod binary-subtract ((minuend fixnum) (subtrahend fixnum))
  (cleavir-primop:let-uninitialized
   (z)
   (if (cleavir-primop:fixnum-sub minuend subtrahend z)
       z
       (convert-fixnum-to-bignum z))))
