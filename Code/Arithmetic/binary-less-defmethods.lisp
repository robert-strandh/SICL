(cl:in-package #:sicl-arithmetic)

(defmethod binary-less ((x fixnum) (y fixnum))
  (cleavir-primop:fixnum-less x y))
