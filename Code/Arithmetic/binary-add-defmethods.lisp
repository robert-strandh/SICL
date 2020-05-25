(cl:in-package #:sicl-arithmetic)

(defmethod binary-add ((x fixnum) (y fixnum))
  (cleavir-primop:let-uninitialized
   (z)
   (if (cleavir-primop:fixnum-add x y z)
       z
       (convert-fixnum-to-bignum z))))
