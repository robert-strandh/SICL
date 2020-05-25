(cl:in-package #:sicl-arithmetic)

(defmethod binary-minus ((minuend fixnum) (subrahend fixnum))
  (cleavir-primop:let-uninitialized
   (z)
   (if (cleavir-primop:fixnum-sub x y z)
       z
       (convert-fixnum-to-bignum z))))
