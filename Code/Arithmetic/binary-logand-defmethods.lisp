(cl:in-package #:sicl-arithmetic)

(defmethod binary-logand ((x fixnum) (y fixnum))
  (cleavir-primop:fixnum-logand x y))
